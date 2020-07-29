package HOME

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers

import HOME.ConstantsTest._

class MQTTTest extends AnyFunSuite with Eventually with Matchers {
  val light: SimulatedLight = Light("A","Salotto")

  test("Coordinator sends commands to the light", BrokerRequired){
    assert(Coordinator.connect)
    assert(light.connect)
    assert(light.subscribe)
    assert(!light.isOn)
    assert(Coordinator.publish(light.getSubTopic, "on"))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }
    assert(Coordinator.publish(light.getSubTopic, CommandMsg(Msg.on)))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }
    assert(Coordinator.publish(light.getSubTopic, "off"))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (false) }
    assert(Coordinator.publish(light.getSubTopic, CommandMsg(Msg.on)))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }
    assert(light.value == 50)
    assert(Coordinator.publish(light.getSubTopic, "setIntensity_15"))
    eventually { Thread.sleep(testSleepTime); light.value should be (15) }
    assert(Coordinator.publish(light.getSubTopic, CommandMsg(Msg.setIntensity, 12)))
    eventually { Thread.sleep(testSleepTime); light.value should be (12) }
    assert(Coordinator.publish(light.getSubTopic, CommandMsg(Msg.setIntensity, 13)))
    eventually { Thread.sleep(testSleepTime); light.value shouldNot be (12) }
    assert(Coordinator.disconnect)
    assert(light.disconnect)
  }
}
