package HOME

import org.scalatest.concurrent.Eventually
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import HOME.Constants._

class JSONUtilsTest extends AnyFunSuite with Eventually with Matchers with JSONUtils {

  val coordinator: Coordinator = CoordinatorImpl()
  val light: SimulatedLight = Light("A","Salotto")

  test("The message + device/coordinator is encoded/decoded via JSON correctly") {
    val msgD: String = getMsg("testMsgD", light)
    val retrievedMessageD: String = getMessageFromMsg(msgD)
    val retrievedDevice: AssociableDevice = getSenderFromMsg[AssociableDevice](msgD)
    assert("testMsgD" == retrievedMessageD)
    assert(light.id == retrievedDevice.id)
    assert(light.room == retrievedDevice.room)
    assert(light.deviceType == retrievedDevice.deviceType)
    assert(light.consumption == retrievedDevice.consumption)
    assert(light.subTopic == retrievedDevice.subTopic)

    val msgC: String = getMsg("testMsgC", coordinator)
    val retrievedMessageC: String = getMessageFromMsg(msgC)
    val retrievedCoordinator: Coordinator = getSenderFromMsg[Coordinator](msgC)
    assert("testMsgC" == retrievedMessageC)
    assert(coordinator.name == retrievedCoordinator.name)

    val msgN: String = getMsg(null.asInstanceOf[String], null)
    val retrievedMessageN: String = getMessageFromMsg(msgN)
    val retrievedC: Coordinator = getSenderFromMsg[Coordinator](msgN)
    val retrievedD: AssociableDevice = getSenderFromMsg[AssociableDevice](msgN)
    assert(retrievedMessageN == null)
    assert(retrievedC == null)
    assert(retrievedD == null)
  }

  //This test needs the MQTT Broker active and running
  test( "Device registers correctly") {
    eventually { Thread.sleep(testSleepTime); light.connect should be (true) }
    eventually { Thread.sleep(testSleepTime); light.subscribe should be (true) }
    eventually { Thread.sleep(testSleepTime); coordinator.connect should be (true) }
    eventually { Thread.sleep(testSleepTime); coordinator.subscribe should be (true) }
    assert(light.isConnected)
    assert(!light.isRegistered)
    eventually { Thread.sleep(testSleepTime); light.register should be (true) }
    eventually { Thread.sleep(testSleepTime); coordinator.devices.size should be (1) }
    eventually { Thread.sleep(testSleepTime); light.isRegistered should be (true) }
    assert(light.register)
    eventually { Thread.sleep(testSleepTime); coordinator.devices.size should be (1) }
    assert(light.isRegistered)
    val registeredDevice: Device = coordinator.devices.head
    assert(light.id == registeredDevice.id)
    assert(light.room == registeredDevice.room)
    assert(light.deviceType == registeredDevice.deviceType)
    assert(light.consumption == registeredDevice.consumption)
    assert(light.disconnect)
    assert(!light.isConnected)
    assert(!light.isRegistered)
    eventually { Thread.sleep(testSleepTime); coordinator.devices.size should be (0)}
    assert(coordinator.disconnect)
  }
}
