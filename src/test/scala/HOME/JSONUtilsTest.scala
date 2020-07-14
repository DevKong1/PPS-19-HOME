package HOME

import org.scalatest.concurrent.Eventually
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import HOME.Constants._

class JSONUtilsTest extends AnyFunSuite with JSONUtils with Eventually with Matchers {

  val coordinator: Coordinator = CoordinatorImpl()
  val light: SimulatedLight = Light("A","salotto")
  val retrievedDevice: Device = getDeviceFromRegistrationMsg(getRegistrationMsg(light))

  test("The light is encoded/decoded via JSON correctly") {
    assert(light.id == retrievedDevice.id)
    assert(light.room == retrievedDevice.room)
    assert(light.device_type == retrievedDevice.device_type)
    assert(light.consumption == retrievedDevice.consumption)
  }

  //This test needs the MQTT Broker active and running
  test( "Device registers correctly") {
    eventually { Thread.sleep(testSleepTime); light.connect should be (true) }
    eventually { Thread.sleep(testSleepTime); light.subscribe should be (true) }
    eventually { Thread.sleep(testSleepTime); coordinator.connect should be (true) }
    eventually { Thread.sleep(testSleepTime); coordinator.subscribe should be (true) }
    eventually { Thread.sleep(testSleepTime); light.register should be (true) }
    eventually { Thread.sleep(testSleepTime); coordinator.devices.size should be (1) }
    assert(light.register)
    eventually { Thread.sleep(testSleepTime); coordinator.devices.size should be (1) }
    val registeredDevice: Device = coordinator.devices.head
    assert(light.id == registeredDevice.id)
    assert(light.room == registeredDevice.room)
    assert(light.device_type == registeredDevice.device_type)
    assert(light.consumption == registeredDevice.consumption)
    assert(light.disconnect)
    assert(coordinator.disconnect)
  }
}
