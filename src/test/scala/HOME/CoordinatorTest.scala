package HOME

import java.lang.reflect.MalformedParametersException

import org.scalatest.funsuite.AnyFunSuite

class CoordinatorTest extends AnyFunSuite {

  test("Basic coordinator with no devices"){
    assert(Coordinator.getDevices.isEmpty)
    assert(Coordinator.activeProfile.name == constants.default_profile_name)
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
    Coordinator.removeDevice(Light("Light2","Salotto"))
    assert(Coordinator.getDevices.size == 1)
    Coordinator.removeDevice(Light("Light1","Salotto"))
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
}