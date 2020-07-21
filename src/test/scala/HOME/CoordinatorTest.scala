package HOME

import java.lang.reflect.MalformedParametersException

import org.scalatest.funsuite.AnyFunSuite

class CoordinatorTest extends AnyFunSuite {

  val coordinator: Coordinator = CoordinatorImpl()

  test("Basic coordinator with no devices"){
    assert(coordinator.getDevices.isEmpty)
    assert(coordinator.activeProfile.name == ProfileNameDefault)
  }

  test("Adding and removing devices which are identified by ID") {
    coordinator.addDevice(Light("Light1","Salotto"))
    assert(coordinator.getDevices.size == 1)
    coordinator.addDevice(Light("Light2","Salotto"))
    assert(coordinator.getDevices.size == 2)
    coordinator.addDevice(Light("Light2","Salotto"))
    assert(coordinator.getDevices.size == 2)
    Rooms.addRoom("Salottino")
    coordinator.addDevice(Light("Light2","Salottino"))
    assert(coordinator.getDevices.size == 2)
    Rooms.removeRoom("Salottino")
    coordinator.removeDevice(Light("Light2","Salotto"))
    assert(coordinator.getDevices.size == 1)
    coordinator.removeDevice(Light("Light1","Salotto"))
    assert(coordinator.getDevices.isEmpty)
  }

  test("The coordinator connects and disconnects to/from the MQTT broker correctly", BrokerRequired) {
    assert(coordinator.connect)
    assert(coordinator.connect)
    assert(coordinator.disconnect)
    assert(coordinator.disconnect)
    assert(coordinator.connect)
    assert(coordinator.disconnect)
  }

  test("The coordinator publishes mock messages correctly", BrokerRequired) {
    assert(coordinator.connect)
    assert(coordinator.publish("pubTopic", "abc"))
    assert(coordinator.disconnect)
    assert(!coordinator.publish("pubTopic", "abc"))
  }

  test("The coordinator throws exceptions on received mock messages correctly", BrokerRequired) {
    assert(coordinator.connect)
    assertThrows[MalformedParametersException](coordinator.onMessageReceived("registration", "off"))
    assertThrows[IllegalArgumentException](coordinator.onMessageReceived("asd", "off"))
    assert(coordinator.disconnect)
  }
}