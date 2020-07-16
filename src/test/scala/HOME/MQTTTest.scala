package HOME

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import HOME.Constants._

class MQTTTest extends AnyFunSuite with Eventually with Matchers {
  val coordinator: Coordinator = CoordinatorImpl()
  val light: SimulatedLight = Light("A","salotto")

  //This test needs the MQTT Broker active and running
  test("Coordinator sends commands to the light"){
    assert(coordinator.connect)
    assert(light.connect)
    assert(light.subscribe)
    assert(!light.isOn)
    assert(coordinator.publish(light.subTopic, "on"))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }
    assert(coordinator.publish(light.getSubTopic, "on"))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (true) }
    assert(coordinator.publish(light.getSubTopic, "off"))
    eventually { Thread.sleep(testSleepTime); light.isOn should be (false) }
    assert(light.getIntensity == 50)
    assert(coordinator.publish(light.getSubTopic, "setIntensity_15"))
    eventually { Thread.sleep(testSleepTime); light.getIntensity should be (15) }
    assert(coordinator.publish(light.getSubTopic, light.deviceType.subTopicMsg + 12))
    eventually { Thread.sleep(testSleepTime); light.getIntensity should be (12) }
    assert(coordinator.publish(light.getSubTopic, LightType.subTopicMsg + 13))
    eventually { Thread.sleep(testSleepTime); light.getIntensity shouldNot be (12) }
    assert(coordinator.disconnect)
    assert(light.disconnect)
  }
}
