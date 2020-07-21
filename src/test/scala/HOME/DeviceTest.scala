package HOME

import java.lang.reflect.MalformedParametersException

import org.scalatest.funsuite.AnyFunSuite

class DeviceTest extends AnyFunSuite with JSONUtils {
  val room: String = "Salotto"

  //Actuators
  val light: SimulatedLight = Light("A", room)
  val AC: SimulatedAirConditioner = AirConditioner("B", room)
  val dehumidifier: SimulatedDehumidifier = Dehumidifier("C", room)
  val shutter: SimulatedShutter = Shutter("D", room)
  val boiler: SimulatedBoiler = Boiler("E", room)
  val tv: SimulatedTV = TV("F", room)
  val washingMachine: SimulatedWashingMachine = WashingMachine("G", room)
  val dishWasher: SimulatedDishWasher = DishWasher("H", room)
  val oven: SimulatedOven = Oven("I", room)
  val stereoSystem: SimulatedStereoSystem = StereoSystem("J", room)

  //Sensors
  val thermometer: SimulatedThermometer = Thermometer("K", room)
  val hygrometer: SimulatedHygrometer = Hygrometer("L", room)
  val photometer: SimulatedPhotometer = Photometer("M", room)
  val motionSensor: SimulatedMotionSensor = MotionSensor("N", room)

  test("The light is instantiated correctly") {
    assert(light.id == "A")
    assert(light.room == "Salotto")
    assert(light.deviceType == LightType)
    assert(light.consumption == 5)

    assert(!light.isOn)
  }

  test("The room is always inserted correctly") {
    assertThrows[IllegalArgumentException](Light("A","zzz"))
  }

  test("The light switches on and off correctly") {
    assert(!light.isOn)
    light.turnOn()
    assert(light.isOn)
    light.turnOff()
    assert(!light.isOn)
  }

  test("The light changes intensity correctly") {
    assert(light.value == 50)
    light.setValue(41)
    assert(light.value == 41)
    light.setValue(-100)
    assert(light.value == 1)
    light.setValue(200)
    assert(light.value == 100)
  }

  test("The Air Conditioner works correctly") {
    assert(AC.value == 22)
    AC.setValue(25)
    assert(AC.value == 25)
    AC.setValue(-100)
    assert(AC.value == 10)
    AC.setValue(200)
    assert(AC.value == 35)
  }

  test("The Dehumidifier works correctly") {
    assert(dehumidifier.value == 10)
    dehumidifier.setValue(25)
    assert(dehumidifier.value == 25)
    dehumidifier.setValue(-100)
    assert(dehumidifier.value == 1)
    dehumidifier.setValue(200)
    assert(dehumidifier.value == 100)
  }

  test("The Shutter works correctly") {
    assert(!shutter.isOpen)
    shutter.open()
    assert(shutter.isOpen)
    shutter.close()
    assert(!shutter.isOpen)
  }

  test("The Boiler works correctly") {
    assert(boiler.value == 22)
    boiler.setValue(25)
    assert(boiler.value == 25)
    boiler.setValue(-100)
    assert(boiler.value == 10)
    boiler.setValue(200)
    assert(boiler.value == 35)
  }

  test("The TV works correctly") {
    assert(tv.value == 50)
    tv.setValue(41)
    assert(tv.value == 41)
    tv.setValue(-100)
    assert(tv.value == 0)
    tv.setValue(200)
    assert(tv.value == 100)
  }

  test("The Washing Machine works correctly") {
    assert(washingMachine.getWashingType == WashingType.MIX)
    washingMachine.setWashingType(WashingType.RAPID)
    assert(washingMachine.getWashingType == WashingType.RAPID)

    assert(washingMachine.getRPM == RPM.MEDIUM)
    washingMachine.setRPM(RPM.FAST)
    assert(washingMachine.getRPM == RPM.FAST)

    assert(washingMachine.getExtras.isEmpty)
    washingMachine.addExtra(WashingMachineExtra.SpecialColors)
    assert(washingMachine.getExtras.contains(WashingMachineExtra.SpecialColors))
    washingMachine.addExtra(WashingMachineExtra.SuperDirty)
    assert(washingMachine.getExtras.size == 2)
    assert(washingMachine.getExtras.contains(WashingMachineExtra.SuperDirty))
    washingMachine.removeExtra(WashingMachineExtra.SpecialColors)
    assert(washingMachine.getExtras.size == 1)
    washingMachine.removeExtra(WashingMachineExtra.SuperDirty)
    assert(washingMachine.getExtras.isEmpty)

    assert(WashingMachineExtra.SuperDirty == WashingMachineExtra("SuperDirty"))
    assertThrows[IllegalArgumentException](WashingMachineExtra("AAA"))
  }

  test("The DishWasher works correctly") {
    assert(dishWasher.getWashingProgram == DishWasherProgram.FAST)
    dishWasher.setWashingProgram(DishWasherProgram.FRAGILE)
    assert(dishWasher.getWashingProgram == DishWasherProgram.FRAGILE)

    assert(dishWasher.getExtras.isEmpty)
    dishWasher.addExtra(DishWasherExtra.SuperDirty)
    assert(dishWasher.getExtras.contains(DishWasherExtra.SuperDirty))
    dishWasher.addExtra(DishWasherExtra.SuperHygiene)
    assert(dishWasher.getExtras.size == 2)
    assert(dishWasher.getExtras.contains(DishWasherExtra.SuperHygiene))
    dishWasher.removeExtra(DishWasherExtra.SuperDirty)
    assert(dishWasher.getExtras.size == 1)
    dishWasher.removeExtra(DishWasherExtra.SuperHygiene)
    assert(dishWasher.getExtras.isEmpty)

    assert(DishWasherExtra.SuperDirty == DishWasherExtra("SuperDirty"))
    assertThrows[IllegalArgumentException](DishWasherExtra("AAA"))
  }

  test("The Oven works correctly") {
    assert(oven.value == 0)
    oven.setValue(180)
    assert(oven.value == 180)
    oven.setValue(-100)
    assert(oven.value == 0)
    oven.setValue(300)
    assert(oven.value == 250)

    assert(oven.getOvenMode == OvenMode.CONVENTIONAL)
    oven.setOvenMode(OvenMode.GRILL)
    assert(oven.getOvenMode == OvenMode.GRILL)
  }

  test("The Stereo System works correctly") {
    assert(stereoSystem.value == 50)
    stereoSystem.setValue(41)
    assert(stereoSystem.value == 41)
    stereoSystem.setValue(-100)
    assert(stereoSystem.value == 0)
    stereoSystem.setValue(200)
    assert(stereoSystem.value == 100)
  }

  test("The thermometer is instantiated correctly", BrokerRequired) {
    assert(thermometer.id == "K")
    assert(thermometer.deviceType == ThermometerType)
    assert(thermometer.connect)
    assert(thermometer.valueChanged(12))
    assert(!thermometer.valueChanged(13))
    assert(thermometer.valueChanged(50.5))
    assert(thermometer.disconnect)
  }

  test("The hygrometer is instantiated correctly", BrokerRequired) {
    assert(hygrometer.id == "L")
    assert(hygrometer.deviceType == HygrometerType)
    assert(hygrometer.connect)
    assert(hygrometer.valueChanged(12))
    assert(!hygrometer.valueChanged(13))
    assert(hygrometer.valueChanged(50.5))
    assert(hygrometer.disconnect)
  }

  test("The photometer is instantiated correctly", BrokerRequired) {
    assert(photometer.id == "M")
    assert(photometer.deviceType == PhotometerType)
    assert(photometer.connect)
    assert(photometer.valueChanged(12))
    assert(!photometer.valueChanged(13))
    assert(photometer.valueChanged(50.5))
    assert(photometer.disconnect)
  }

  test("The motionSensor is instantiated correctly", BrokerRequired) {
    assert(motionSensor.id == "N")
    assert(motionSensor.deviceType == MotionSensorType)
    assert(motionSensor.connect)
    assert(motionSensor.valueChanged(false))
    assert(!motionSensor.valueChanged(false))
    assert(motionSensor.valueChanged(true))
    assert(motionSensor.disconnect)
  }

  test("Adding and removing rooms") {
    assert(Rooms.allRooms contains "Salotto")
    assert(!(Rooms.allRooms contains "Salottino"))
    assertThrows[IllegalArgumentException](Light("A", "Salottino"))
    Rooms.addRoom("Salottino")
    assert(Rooms.allRooms contains "Salottino")
    Light("A", "Salottino")
    Rooms.removeRoom("Salottino")
    assert(!(Rooms.allRooms contains "Salottino"))
  }

  test("The publish/subscription topics are created correctly") {
    assert(light.getPubTopic == light.room + "/" + light.deviceType + "/" + light.id + "/" + "From")
    assert(light.getSubTopic == light.room + "/" + light.deviceType + "/" + light.id + "/" + "To")
  }

  test("The light connects and disconnects to/from the MQTT broker correctly", BrokerRequired) {
    assert(light.connect)
    assert(light.connect)
    assert(light.disconnect)
    assert(light.disconnect)
    assert(light.connect)
    assert(light.disconnect)
  }

  test("The light handles received mock messages correctly", BrokerRequired) {
    assert(light.connect)
    light.onMessageReceived(light.getSubTopic, getMsg("on", light))
    assert(light.isOn)
    light.onMessageReceived(light.getSubTopic, getMsg(CommandMsg(Msg.on), light))
    assert(light.isOn)
    light.onMessageReceived(light.getSubTopic, getMsg(CommandMsg(Msg.off), light))
    assert(!light.isOn)
    light.onMessageReceived(light.getSubTopic, getMsg("on", light))
    assert(light.isOn)
    light.onMessageReceived(light.getSubTopic, getMsg("setIntensity_255", light))
    assert(light.value == 100)
    light.onMessageReceived(light.getSubTopic, getMsg(CommandMsg(Msg.setIntensity, 35), light))
    assert(light.value == 35)
    light.onMessageReceived(light.getSubTopic, getMsg(CommandMsg(Msg.setIntensity, 30), light))
    assert(light.value == 30)
    assertThrows[MalformedParametersException](light.onMessageReceived(light.getSubTopic,"setIntensity_a22"))
    assertThrows[IllegalArgumentException](light.onMessageReceived(light.getPubTopic, "off"))
    assert(light.value == 30)
    assert(light.disconnect)
  }
}
