package HOME

import org.scalatest.funsuite.AnyFunSuite

class DeviceTest extends AnyFunSuite with JSONUtils {

  val light: SimulatedLight = Light("A","salotto")

  test("The light has been instantiated correctly") {
    assert(light.id == "A")
    assert(light.room == "salotto")
    assert(light.deviceType == LightType)
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

  test("Adding and removing rooms") {
    assert(Rooms.allRooms contains "salotto")
    assert(!(Rooms.allRooms contains "salottino"))
    assertThrows[IllegalArgumentException](Light("A", "salottino"))
    Rooms.addRoom("salottino")
    assert(Rooms.allRooms contains "salottino")
    Light("A", "salottino")
    Rooms.removeRoom("salottino")
    assert(!(Rooms.allRooms contains "salottino"))
  }

  test("The subscription topic is created correctly") {
    assert(light.getSubTopic == light.room + "/" + light.deviceType + "/" + light.id)
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
    light.onMessageReceived(light.subTopic, getMsg("on", light))
    assert(light.isOn)
    light.onMessageReceived(light.subTopic, getMsg("on", light))
    assert(light.isOn)
    light.onMessageReceived(light.subTopic, getMsg("off", light))
    assert(!light.isOn)
    light.onMessageReceived(light.subTopic, getMsg("on", light))
    assert(light.isOn)
    light.onMessageReceived(light.subTopic, getMsg("setIntensity_255", light))
    assert(light.getIntensity == 100)
    light.onMessageReceived(light.subTopic, getMsg(light.deviceType.subTopicMsg + 35, light))
    assert(light.getIntensity == 35)
    light.onMessageReceived(light.subTopic, getMsg(LightType.subTopicMsg + 30, light))
    assert(light.getIntensity == 30)
    assertThrows[IllegalArgumentException](light.onMessageReceived(light.subTopic, getMsg("setIntensity_a22", light)))
    assertThrows[IllegalArgumentException](light.onMessageReceived(light.pubTopic, "off"))
    assert(light.getIntensity == 30)
    assert(light.disconnect)
  }
}
