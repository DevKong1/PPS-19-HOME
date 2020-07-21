package HOME

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import HOME.Constants._

class MQTTTest extends AnyFunSuite with Eventually with Matchers {
  val coordinator: Coordinator = CoordinatorImpl()
  val light: SimulatedLight = Light("A","Salotto")

  //This test needs the MQTT Broker active and running
  test("Coordinator sends commands to the light"){
    assert(coordinator.connect)
    assert(light.connect)
    assert(light.subscribe)
    assert(!light.isOn)
    assert(coordinator.publish(light.getSubTopic, "on"))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }
    assert(coordinator.publish(light.getSubTopic, CommandMsg(Msg.on)))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }
    assert(coordinator.publish(light.getSubTopic, "off"))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (false) }
    assert(coordinator.publish(light.getSubTopic, CommandMsg(Msg.on)))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }
    assert(light.value == 50)
    assert(coordinator.publish(light.getSubTopic, "setIntensity_15"))
    eventually { Thread.sleep(testSleepTime); light.value should be (15) }
    assert(coordinator.publish(light.getSubTopic, CommandMsg(Msg.setIntensity, 12)))
    eventually { Thread.sleep(testSleepTime); light.value should be (12) }
    assert(coordinator.publish(light.getSubTopic, CommandMsg(Msg.setIntensity, 13)))
    eventually { Thread.sleep(testSleepTime); light.value shouldNot be (12) }
    assert(coordinator.disconnect)
    assert(light.disconnect)
  }
}
