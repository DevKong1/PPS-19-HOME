package HOME

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers

class MQTTTest extends AnyFunSuite with Eventually with Matchers {
  val coordinator: Coordinator = CoordinatorImpl()
  val light: SimulatedLight = Light("A","salotto")

  val sleepTime = 50; //millis, time the matcher waits to evaluate eventually expression

  //This test needs the MQTT Broker active and running
  test("Coordinator sends commands to the light"){
    assert(coordinator.connect)
    assert(light.connect)
    assert(light.subscribe)
    assert(!light.isOn)
    assert(coordinator.publish(light.subTopic, "on"))
    eventually { Thread.sleep(sleepTime); light.isOn should be (true) }
    assert(coordinator.publish(light.getSubTopic, "on"))
    eventually { Thread.sleep(sleepTime); light.isOn should be (true) }
    assert(coordinator.publish(light.getSubTopic, "off"))
    eventually { Thread.sleep(sleepTime); light.isOn should be (false) }
    assert(light.getIntensity == 50)
    assert(coordinator.publish(light.getSubTopic, "setIntensity_12"))
    eventually { Thread.sleep(sleepTime); light.getIntensity should be (12) }
    assert(coordinator.publish(light.getSubTopic, "setIntensity_13"))
    eventually { Thread.sleep(sleepTime); light.getIntensity shouldNot be (12) }
    assert(coordinator.disconnect)
    assert(light.disconnect)
  }

  //TODO TEST REGISTRATION ROUTINE
}
