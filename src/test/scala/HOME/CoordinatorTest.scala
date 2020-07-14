package HOME

import org.scalatest.funsuite.AnyFunSuite

class CoordinatorTest extends AnyFunSuite {

  val coordinator: Coordinator = CoordinatorImpl()

  test("Basic coordinator with no devices"){
    assert(coordinator.getDevices.isEmpty)
  }

  test("Adding and removing devices"){
    coordinator.addDevice(Light("Light1","salotto"))
    assert(coordinator.getDevices.size == 1)
    coordinator.addDevice(Light("Light2","salotto"))
    assert(coordinator.getDevices.size == 2)
    coordinator.removeDevice(Light("Light2","salotto"))
    assert(coordinator.getDevices.size == 1)
    coordinator.removeDevice(Light("Light1","salotto"))
    assert(coordinator.getDevices.isEmpty)
  }

  //This test needs the MQTT Broker active and running
  test("The coordinator connects and disconnects to/from the MQTT broker correctly") {
    assert(coordinator.connect)
    assert(coordinator.connect)
    assert(coordinator.disconnect)
    assert(coordinator.disconnect)
    assert(coordinator.connect)
    assert(coordinator.disconnect)
  }

  //This test needs the MQTT Broker active and running
  test("The coordinator publishes mock messages correctly") {
    assert(coordinator.connect)
    assert(coordinator.publish("pubTopic", "abc"))
    assert(coordinator.disconnect)
    assert(!coordinator.publish("pubTopic", "abc"))
  }

  //This test needs the MQTT Broker active and running
  test("The coordinator throws exceptions on received mock messages correctly") {
    assert(coordinator.connect)
    assertThrows[IllegalArgumentException](coordinator.onMessageReceived("registration", "off"))
    assertThrows[IllegalArgumentException](coordinator.onMessageReceived("asd", "off"))
    assert(coordinator.disconnect)
  }
}