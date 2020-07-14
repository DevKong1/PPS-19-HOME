package HOME

import org.scalatest.funsuite.AnyFunSuite

class JSONUtilsTest extends AnyFunSuite with JSONUtils {
  val light: SimulatedLight = Light("A","salotto")
  val retrievedDevice: Device = getDeviceFromRegistrationMsg(getRegistrationMsg(light))

  test("The light is encoded/decoded via JSON correctly") {
    assert(light.name == retrievedDevice.name)
    assert(light.room == retrievedDevice.room)
    assert(light.device_type == retrievedDevice.device_type)
    assert(light.consumption == retrievedDevice.consumption)
  }
}
