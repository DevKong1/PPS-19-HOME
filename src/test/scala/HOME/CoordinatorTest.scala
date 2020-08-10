package HOME

import java.lang.reflect.MalformedParametersException

import HOME.ConstantsTest.testSleepTime
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers

class CoordinatorTest extends AnyFunSuite with Eventually with Matchers {
  Rooms.addRoom("Living room")

  test("Basic coordinator with no devices"){
    assert(Coordinator.getDevices.isEmpty)
    assert(Coordinator.activeProfile.name == Constants.default_profile_name)
  }


  private val salotto: String = "Living room"

  test("Coordinator correctly calculates Consuption"){
    val Light1 = Light("Light1",salotto)
    val Light2 = Light("Light2",salotto)

    assert(Coordinator.getActiveConsumption == 0)
    Coordinator.addDevice(Light1)
    Light1.turnOn()
    assert(Coordinator.getActiveConsumption == 5)
    Coordinator.addDevice(Light2)
    Light2.turnOn()
    assert(Coordinator.getActiveConsumption == 10)
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

  def prepareDevices(args: AssociableDevice*): Unit = {
    assert(Coordinator.connect)
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

  test("The coordinator correctly applies the NIGHT profile", BrokerRequired) {
    val light = Light("Light1",salotto)
    val shutter = Shutter("Shutter1",salotto)
    val ac = AirConditioner("AC1",salotto)
    val humid = Dehumidifier("Dehumidifier1",salotto)

    light.turnOn()
    shutter.turnOn()
    shutter.open()

    prepareDevices(light, shutter, ac, humid)

    Coordinator.setProfile(Profile("NIGHT"))
    assert(Coordinator.activeProfile.name == "NIGHT")

    eventually { Thread.sleep(testSleepTime); light.isOn should be (false) }
    eventually { Thread.sleep(testSleepTime); shutter.isOpen should be (false) }
    eventually { Thread.sleep(testSleepTime); ac.isOn should be (true) }
    eventually { Thread.sleep(testSleepTime); humid.isOn should be (true) }
    eventually { Thread.sleep(testSleepTime); ac.getValue should be (21) }
    eventually { Thread.sleep(testSleepTime); humid.getValue should be (40) }

    Coordinator.activeProfile.onMotionSensorNotification(salotto)
    eventually { Thread.sleep(testSleepTime); light.getValue should be (30) }
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }

    Coordinator.activeProfile.onPhotometerNotification(salotto, 45)
    eventually { Thread.sleep(testSleepTime); Coordinator.getActiveProfile should be (Profile(Constants.default_profile_name)) }

    concludeTest(light, shutter, ac, humid)
  }

  test("The custom profile builder builds and Saves a Set of instructions correctly", BrokerRequired) {
    val tv = TV("TV1",salotto)

    assert(!tv.isOn)
    assert(tv.value == 50)

    prepareDevices(tv)

    val commands: Set[(Device,CommandMsg)] = Set((tv,CommandMsg(cmd = Msg.on)), (tv,CommandMsg(cmd = Msg.mute)))
    val generatedCommands: Set[Device => Unit] = CustomProfileBuilder.generateCommandSet(commands)
    val temperatureCommands: Set[(Device,CommandMsg)] = Set((tv, CommandMsg(Msg.nullCommandId, Msg.setVolume, 100)))
    val generatedTemperatureCommands: Set[Device => Unit] = CustomProfileBuilder.generateCommandSet(temperatureCommands)
    val temperatureCheck = CustomProfileBuilder.generateCheckFunction(">",50)

    val dummySet: Set[Device => Unit] = Set({_.id})
    val dummyCheck: Int => Boolean = _ => false
    val builtProfile = CustomProfileBuilder.generateFromParams("Custom1","test", generatedCommands, temperatureCheck, generatedTemperatureCommands, dummyCheck, dummySet,
      dummyCheck, dummySet, dummySet,dummySet,{})

    Profile.addProfile(builtProfile)
    assert(Profile.savedProfiles.contains(builtProfile))

    Coordinator.setProfile(builtProfile)
    eventually { Thread.sleep(testSleepTime); tv.isOn should be (true) }
    eventually { Thread.sleep(testSleepTime); tv.value should be (tv.minValue) }

    Coordinator.activeProfile.onThermometerNotification(salotto, 51)
    eventually { Thread.sleep(testSleepTime); tv.value should be (100) }

    Profile.removeProfile("Custom1")
    assert(!Profile.savedProfiles.contains(builtProfile))

    Coordinator.setProfile(Profile(Constants.default_profile_name))
    concludeTest(tv)
  }
}