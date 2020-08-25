package HOME

import java.lang.reflect.MalformedParametersException

import HOME.ConstantsTest.testSleepTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers

class CoordinatorTest extends AnyFunSuite with Eventually with Matchers with BeforeAndAfterAll {
  override def beforeAll(): Unit = {
    Logger.setTestFile()
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    Logger.resetFile()
    Logger.unsetTestFile()
    super.beforeAll()
  }

  def prepareDevices(args: AssociableDevice*): Unit = {
    assert(Coordinator.connect)
    assert(Coordinator.subscribe)
    for (device <- args){
      assert(device.connect)
      assert(device.subscribe)
      Coordinator.addDevice(device)
    }
  }

  def concludeTest(args: AssociableDevice*): Unit = {
    for (device <- args){
      device.disconnect
    }
    Coordinator.removeAllDevices()
    Coordinator.disconnect
  }

  Rooms.addRoom("Living room")

  test("Basic coordinator with no devices"){
    assert(Coordinator.getDevices.isEmpty)
    assert(Coordinator.getActiveProfile.name == Constants.default_profile_name)
  }

  private val salotto: String = "Living room"

  test("Coordinator correctly calculates Consuption"){
    val Light1 = Light("Light1",salotto)
    val Light2 = Light("Light2",salotto)

    Light1.setValue(100)
    Light2.setValue(100)

    assert(Coordinator.getActiveConsumption == 0)
    Coordinator.addDevice(Light1)
    Light1.turnOn()
    assert(Coordinator.getActiveConsumption == 5)
    Coordinator.addDevice(Light2)
    Light2.turnOn()
    assert(Coordinator.getActiveConsumption == 10)
    Light2.turnOff()
    assert(Coordinator.getActiveConsumption == 5)
    Coordinator.removeAllDevices()
    assert(Coordinator.getActiveConsumption == 0)
  }

  test("Adding and removing devices which are identified by ID") {
    Coordinator.addDevice(Light("Light1",salotto))
    assert(Coordinator.getDevices.size == 1)
    Coordinator.addDevice(Light("Light2",salotto))
    assert(Coordinator.getDevices.size == 2)
    Coordinator.addDevice(Light("Light2",salotto))
    assert(Coordinator.getDevices.size == 2)
    Rooms.addRoom("Salottino")
    Coordinator.addDevice(Light("Light2","Salottino"))
    assert(Coordinator.getDevices.size == 2)
    Rooms.removeRoom("Salottino")
    Coordinator.removeDevice("Light2")
    assert(Coordinator.getDevices.size == 1)
    Coordinator.removeDevice("Light1")
    assert(Coordinator.getDevices.isEmpty)
  }

  test("The coordinator connects and disconnects to/from the MQTT broker correctly", BrokerRequired) {
    assert(Coordinator.connect)
    assert(Coordinator.connect)
    assert(Coordinator.disconnect)
    assert(Coordinator.disconnect)
    assert(Coordinator.connect)
    assert(Coordinator.disconnect)
  }

  test("The coordinator publishes mock messages correctly", BrokerRequired) {
    assert(Coordinator.connect)
    assert(Coordinator.publish("pubTopic", "abc"))
    assert(Coordinator.disconnect)
    assert(!Coordinator.publish("pubTopic", "abc"))
  }

  test("The coordinator throws exceptions on received mock messages correctly", BrokerRequired) {
    assert(Coordinator.connect)
    assertThrows[MalformedParametersException](Coordinator.onMessageReceived("registration", "off"))
    assertThrows[IllegalArgumentException](Coordinator.onMessageReceived("asd", "off"))
    assert(Coordinator.disconnect)
  }

  test("The coordinator correctly applies the NIGHT profile", BrokerRequired) {
    val light = Light("Light1",salotto)
    val shutter = Shutter("Shutter1",salotto)
    val ac = AirConditioner("AC1",salotto)
    val humid = Dehumidifier("Dehumidifier1",salotto)

    light.turnOn()
    shutter.open()

    prepareDevices(light, shutter, ac, humid)

    Coordinator.setProfile(Profile("NIGHT"))
    assert(Coordinator.getActiveProfile.name == "NIGHT")

    eventually { Thread.sleep(testSleepTime); shutter.isOpen should be (false) }
    eventually { Thread.sleep(testSleepTime); ac.isOn should be (true) }
    eventually { Thread.sleep(testSleepTime); humid.isOn should be (true) }
    eventually { Thread.sleep(testSleepTime); ac.getValue should be (25) }
    eventually { Thread.sleep(testSleepTime); humid.getValue should be (40) }
    eventually { Thread.sleep(testSleepTime); light.isOn should be (false) }

    Coordinator.getActiveProfile.onMotionSensorNotification(salotto, true)
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }
    eventually { Thread.sleep(testSleepTime); light.getValue should be (30) }

    Coordinator.getActiveProfile.onPhotometerNotification(salotto, 45)
    eventually { Thread.sleep(testSleepTime); Coordinator.getActiveProfile.name should be ("DAY") }

    concludeTest(light, shutter, ac, humid)
  }

  test("The custom profile builder builds and Saves a Set of instructions correctly", BrokerRequired) {
    val tv = TV("TV1",salotto)

    assert(!tv.isOn)
    assert(tv.value == 50)

    prepareDevices(tv)

    val commands: Set[(Device,CommandMsg)] = Set((tv,CommandMsg(cmd = Msg.on)), (tv,CommandMsg(cmd = Msg.mute)))
    val generatedCommands: Set[Device => Unit] = CustomProfileBuilder.generateCommandSet(commands)

    val temperatureCheckMoreThan50 = CustomProfileBuilder.generateCheckFunction(">",50, salotto)
    val temperatureCommandsMoreThan50: Set[(Device,CommandMsg)] = Set((tv, CommandMsg(Msg.nullCommandId, Msg.setVolume, 100)))
    val generatedtemperatureCommandsMoreThan50 = CustomProfileBuilder.generateCommandSet(temperatureCommandsMoreThan50)

    val temperatureCheckLessThan50 = CustomProfileBuilder.generateCheckFunction("<", 50, salotto)
    val temperatureCommandsLessThan50: Set[(Device,CommandMsg)] = Set((tv, CommandMsg(Msg.nullCommandId, Msg.setVolume, 30)))
    val generatedtemperatureCommandsLessThan50 = CustomProfileBuilder.generateCommandSet(temperatureCommandsLessThan50)

    val generatedTemperatureCommandsMap = CustomProfileBuilder.generateSensorCommandsMap((temperatureCheckMoreThan50, generatedtemperatureCommandsMoreThan50), (temperatureCheckLessThan50, generatedtemperatureCommandsLessThan50))


    val dummySet: Set[Device => Unit] = Set({_.id})
    val dummyMap: Map[(String,Double) => Boolean, Set[Device => Unit]] = Map.empty
    val dummySensorMap: Map[String, Set[Device => Unit]] = Map.empty

    val builtProfile = CustomProfileBuilder.generateFromParams("Custom1","test", generatedCommands, generatedTemperatureCommandsMap, dummyMap,
      dummyMap, dummySensorMap,dummySet,{})

    Profile.addProfile(builtProfile)
    assert(Profile.savedProfiles.contains(builtProfile))

    Coordinator.setProfile(builtProfile)
    assert(Coordinator.getActiveProfile.name == "Custom1")

    eventually { Thread.sleep(testSleepTime); tv.isOn should be (true) }
    eventually { Thread.sleep(testSleepTime); tv.value should be (tv.minValue) }

    Coordinator.getActiveProfile.onThermometerNotification(salotto, 51)
    eventually { Thread.sleep(testSleepTime); tv.value should be (100) }

    Coordinator.getActiveProfile.onThermometerNotification(salotto, 49)
    eventually { Thread.sleep(testSleepTime); tv.value should be (30) }

    Profile.removeProfile("Custom1")
    assert(!Profile.savedProfiles.contains(builtProfile))

    Coordinator.setProfile(Profile(Constants.default_profile_name))
    concludeTest(tv)
  }

  test("The Coordinator correctly receives and logs Log messages", BrokerRequired) {
    Logger.resetFile()

    val light: SimulatedLight = Light("A","Living room")
    prepareDevices(light)

    assert(Coordinator.publish(light, CommandMsg(0, Msg.on)))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }
    assert(Coordinator.publish(light, CommandMsg(0, Msg.off)))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (false) }
    val fileData = Logger.getLogAsListWithHeader
    val firstRow = fileData.head
    val secondRow = fileData(1)
    assert(firstRow("ID") == "A" && firstRow("Consumption") == "5")
    assert(secondRow("ID") == "A" && secondRow("Consumption") == "5")

    concludeTest(light)
  }


  test("The Coordinator correctly calculates last month Consumption", BrokerRequired) {
    val light: SimulatedLight = Light("A","Living room")
    val light2: SimulatedLight = Light("B","Living room")
    light.setValue(100)
    light2.setValue(100)
    prepareDevices(light, light2)

    val now = org.joda.time.DateTime.now()
    assert(Logger.log("A", Constants.outputDateFormat.print(now), Msg.on, "5"))
    assert(Logger.log("A", Constants.outputDateFormat.print(now.plusMinutes(10)), Msg.off, "5"))
    assert(Logger.log("A", Constants.outputDateFormat.print(now.plusMinutes(10)), Msg.on, "5"))
    assert(Logger.log("A", Constants.outputDateFormat.print(now.plusMinutes(20)), Msg.off, "5"))
    assert(Logger.log("B", Constants.outputDateFormat.print(now), Msg.on, "5"))
    assert(Logger.log("B", Constants.outputDateFormat.print(now.plusMinutes(10)), Msg.off, "5"))
    assert(Logger.log("B", Constants.outputDateFormat.print(now.plusMinutes(10)), Msg.on, "5"))
    assert(Logger.log("B", Constants.outputDateFormat.print(now.plusMinutes(20)), Msg.off, "5"))

    assert(Coordinator.getTotalConsumption == 0.003333333333333333)

    concludeTest(light, light2)
  }
}