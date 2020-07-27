package HOME

import java.lang.reflect.MalformedParametersException

import HOME.ConstantsTest.testSleepTime
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers

class CoordinatorTest extends AnyFunSuite with Eventually with Matchers {

  test("Basic coordinator with no devices"){
    assert(Coordinator.getDevices.isEmpty)
    assert(Coordinator.activeProfile.name == Constants.default_profile_name)
  }

  test("Adding and removing devices which are identified by ID") {
    Coordinator.addDevice(Light("Light1","Salotto"))
    assert(Coordinator.getDevices.size == 1)
    Coordinator.addDevice(Light("Light2","Salotto"))
    assert(Coordinator.getDevices.size == 2)
    Coordinator.addDevice(Light("Light2","Salotto"))
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

  test("The coordinator correctly applies the NIGHT profile)") {
    val tv = TV("TV1","Salotto")
    val light = Light("Light1","Salotto")
    val shutter = Shutter("Shutter1","Salotto")
    tv.turnOn()
    light.turnOn()
    shutter.turnOn()
    shutter.open()

    assert(tv.isOn)
    assert(tv.value == 50)
    assert(light.isOn)
    assert(shutter.isOn)
    assert(shutter.isOpen)

    assert(tv.connect)
    assert(tv.subscribe)
    assert(light.connect)
    assert(light.subscribe)
    assert(shutter.connect)
    assert(shutter.subscribe)

    assert(Coordinator.connect)
    Coordinator.addDevice(tv)
    Coordinator.addDevice(light)
    Coordinator.addDevice(shutter)
    Coordinator.setProfile(Profile("NIGHT"))
    assert(Coordinator.activeProfile.name == "NIGHT")

    eventually { Thread.sleep(testSleepTime); tv.value should be (tv.minValue) }
    eventually { Thread.sleep(testSleepTime); light.isOn should be (false) }
    eventually { Thread.sleep(testSleepTime); shutter.isOpen should be (false) }

    light.disconnect
    tv.disconnect
    shutter.disconnect
    Coordinator.removeAllDevices()
    Coordinator.setProfile(Profile(Constants.default_profile_name))
    Coordinator.disconnect
  }

  test("The custom profile builder builds and Saves a Set of instructions correctly)") {
    val tv = TV("TV1","Salotto")
    tv.connect
    tv.subscribe

    assert(!tv.isOn)
    assert(tv.value == 50)

    Coordinator.connect
    Coordinator.addDevice(tv)

    val commands: Set[(Device,CommandMsg)] = Set((tv,CommandMsg(cmd = Msg.on)), (tv,CommandMsg(cmd = Msg.mute)))
    val generatedCommands: Set[Device => Unit] = CustomProfileBuilder.generateCommandSet(commands)
    val dummySet: Set[Device => Unit] = Set({_.id})
    val builtProfile = CustomProfileBuilder.generateFromParams("Custom1","test", generatedCommands, dummySet, dummySet,
      dummySet, dummySet,dummySet,{})

    Profile.addProfile(builtProfile)
    assert(Profile.savedProfiles.contains(builtProfile))

    Coordinator.setProfile(builtProfile)
    eventually { Thread.sleep(testSleepTime); tv.isOn should be (true) }
    eventually { Thread.sleep(testSleepTime); tv.value should be (tv.minValue) }

    tv.disconnect
    Coordinator.removeAllDevices()
    Profile.removeProfile("Custom1")
    assert(!Profile.savedProfiles.contains(builtProfile))

    Coordinator.disconnect
    Coordinator.setProfile(Profile(Constants.default_profile_name))
  }
}