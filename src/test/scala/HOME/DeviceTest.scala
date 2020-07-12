package HOME

import org.scalatest.funsuite.AnyFunSuite

class DeviceTest extends AnyFunSuite {

  val light: SimulatedLight = Light("A","salotto")

  test("The light has been instantiated correctly") {
    assert(light.name == "A")
    assert(light.room == "salotto")
    assert(light.device_type == "Light")
    assert(light.consumption == 5)

    assert(!light.isOn)
  }

  test("The light switches on and off correctly") {
    assert(!light.isOn)
    light.turnOn()
    assert(light.isOn)
    light.turnOff()
    assert(!light.isOn)
  }

  test("The light changes intensity correctly") {
    assert(light.getIntensity == 50)
    light.setIntensity(41)
    assert(light.getIntensity == 41)
    light.setIntensity(-100)
    assert(light.getIntensity == 1)
    light.setIntensity(200)
    assert(light.getIntensity == 100)
  }

  test("The subscription topic is created correctly") {
    assert(light.getSubTopic == light.room + "/" + light.device_type + "/" + light.name)
  }

  //This test needs the MQTT Broker active and running
  test("The light connects and disconnects to/from the MQTT broker correctly") {
    assert(light.connect)
    assert(light.connect)
    assert(light.disconnect)
    assert(light.disconnect)
    assert(light.connect)
    assert(light.disconnect)
  }

  //This test needs the MQTT Broker active and running
  test("The light handles received mock messages correctly") {
    assert(light.connect)
    light.onMessageReceived(light.subTopic, "on")
    assert(light.isOn)
    light.onMessageReceived(light.subTopic,"on")
    assert(light.isOn)
    light.onMessageReceived(light.subTopic,"off")
    assert(!light.isOn)
    light.onMessageReceived(light.subTopic,"on")
    assert(light.isOn)
    light.onMessageReceived(light.subTopic,"setIntensity_255")
    assert(light.getIntensity == 100)
    light.onMessageReceived(light.subTopic,"setIntensity_35")
    assert(light.getIntensity == 35)
    assertThrows[IllegalArgumentException](light.onMessageReceived(light.subTopic,"setIntensity_a22"))
    assertThrows[IllegalArgumentException](light.onMessageReceived(light.pubTopic, "off"))
    assert(light.getIntensity == 35)
    assert(light.disconnect)
  }
}
