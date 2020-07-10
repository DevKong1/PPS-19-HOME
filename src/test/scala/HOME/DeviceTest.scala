package HOME

import org.scalatest.funsuite.AnyFunSuite

class DeviceTest extends AnyFunSuite {

  val light = Light("A","salotto")

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

}
