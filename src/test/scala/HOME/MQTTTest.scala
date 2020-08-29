package HOME

import HOME.ConstantsTest._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.Eventually
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MQTTTest extends AnyFunSuite with Eventually with Matchers with BeforeAndAfterAll {
  override def beforeAll(): Unit = {
    Logger.setTestFile()
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    Logger.resetFile()
    Logger.unsetTestFile()
    super.beforeAll()
  }

  Rooms.addRoom("Living room")
  val light: SimulatedLight = Light("A","Living room")
  val thermometer: SimulatedThermometer = Thermometer("T","Living room")
  val motionSensor: SimulatedMotionSensor = MotionSensor("M","Living room")

  test("Coordinator sends commands to the light", BrokerRequired){
    assert(Coordinator.connect)
    assert(Coordinator.subscribe)
    assert(light.connect)
    assert(light.subscribe)
    assert(!light.isOn)
    assert(Coordinator.publish(light.getSubTopic, "0_on"))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }
    assert(Coordinator.publish(light, CommandMsg(Msg.nullCommandId, Msg.on)))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }
    assert(Coordinator.publish(light.getSubTopic, "0_off"))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (false) }
    assert(Coordinator.publish(light, CommandMsg(cmd = Msg.on)))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }
    assert(light.value == 50)
    assert(Coordinator.publish(light.getSubTopic, "0_setIntensity_15"))
    eventually { Thread.sleep(testSleepTime); light.value should be (15) }
    assert(Coordinator.publish(light, CommandMsg(3, Msg.setIntensity, 12)))
    eventually { Thread.sleep(testSleepTime); light.value should be (12) }
    assert(Coordinator.publish(light, CommandMsg(4, Msg.setIntensity, 13)))
    eventually { Thread.sleep(testSleepTime); light.value shouldNot be (12) }
    assert(Coordinator.disconnect)
    assert(light.disconnect)
  }

  test("Coordinator forces a registered device to disconnect", BrokerRequired){
    assert(Coordinator.connect)
    assert(Coordinator.subscribe)
    assert(light.connect)
    assert(light.subscribe)
    val p = light.register
    eventually { Thread.sleep(testSleepTime); Coordinator.getDevices.size should be (1) }
    eventually { Thread.sleep(testSleepTime); light.isRegistered should be (true) }
    eventually { Thread.sleep(testSleepTime); p.isCompleted should be (true) }
    assert(Coordinator.publish(Coordinator.getDevices.head.asInstanceOf[AssociableDevice].getSubTopic, Msg.disconnect))
    eventually { Thread.sleep(testSleepTime); light.isConnected should be (false) }
    eventually { Thread.sleep(testSleepTime); light.isRegistered should be (false) }
    eventually { Thread.sleep(testSleepTime); Coordinator.getDevices.size should be (0) }
    assert(Coordinator.disconnect)
  }

  test("Coordinator receives messages from sensors", BrokerRequired){
    assert(Coordinator.connect)
    assert(Coordinator.subscribe)
    assert(thermometer.connect)
    assert(thermometer.subscribe)
    val p = thermometer.register
    eventually { Thread.sleep(testSleepTime); Coordinator.getDevices.size should be (1) }
    eventually { Thread.sleep(testSleepTime); thermometer.isRegistered should be (true) }
    eventually { Thread.sleep(testSleepTime); p.isCompleted should be (true) }
    assert(thermometer.valueChanged(12.5))
    assert(!thermometer.valueChanged(12.5))
    assert(thermometer.valueChanged(22.5))

    assert(motionSensor.connect)
    assert(motionSensor.subscribe)
    val p1 = motionSensor.register
    assert(light.connect)
    assert(light.subscribe)
    val p2 = light.register
    eventually { Thread.sleep(testSleepTime); Coordinator.getDevices.size should be (3) }
    eventually { Thread.sleep(testSleepTime); motionSensor.isRegistered should be (true) }
    eventually { Thread.sleep(testSleepTime); light.isRegistered should be (true) }
    eventually { Thread.sleep(testSleepTime); p1.isCompleted should be (true) }
    eventually { Thread.sleep(testSleepTime); p2.isCompleted should be (true) }
    Coordinator.setProfile(Profile("NIGHT"))
    assert(!light.isOn)
    assert(motionSensor.valueChanged(true))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }

    assert(Coordinator.disconnect)
    eventually { Thread.sleep(testSleepTime); thermometer.isRegistered should be (false) }
    eventually { Thread.sleep(testSleepTime); motionSensor.isRegistered should be (false) }
    eventually { Thread.sleep(testSleepTime); light.isRegistered should be (false) }
    assert(thermometer.disconnect)
    assert(motionSensor.disconnect)
    assert(light.disconnect)
    Coordinator.removeAllDevices()
    Coordinator.setProfile(Profile(Constants.default_profile_name))
  }
}
