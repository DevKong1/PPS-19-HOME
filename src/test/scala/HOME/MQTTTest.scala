package HOME

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import HOME.ConstantsTest._

class MQTTTest extends AnyFunSuite with Eventually with Matchers {
  val light: SimulatedLight = Light("A","Living room")

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
}
