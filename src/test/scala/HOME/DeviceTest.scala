package HOME

import java.lang.reflect.MalformedParametersException

import org.scalatest.funsuite.AnyFunSuite

class DeviceTest extends AnyFunSuite with JSONUtils {

  val light: SimulatedLight = Light("A","Salotto")
  val AC: SimulatedAirConditioner = AirConditioner("B","Salotto")
  val dehumidifier: SimulatedDehumidifier = Dehumidifier("C","Salotto")
  val shutter: SimulatedShutter = Shutter("D","Salotto")
  val boiler: SimulatedBoiler = Boiler("E","Salotto")
  val tv: SimulatedTV = TV("F","Salotto")
  val washingMachine: SimulatedWashingMachine = WashingMachine("G","Salotto")
  val dishWasher: SimulatedDishWasher = DishWasher("H","Salotto")
  val oven: SimulatedOven = Oven("I","Salotto")
  val stereoSystem: SimulatedStereoSystem = StereoSystem("L","Salotto")

  test("The light has been instantiated correctly") {
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
    assert(washingMachine.activeWashing == WashingType.MIX)
    washingMachine.setWashingType(WashingType.RAPID)
    assert(washingMachine.activeWashing == WashingType.RAPID)

    assert(washingMachine.activeRPM == RPM.MEDIUM)
    washingMachine.setRPM(RPM.FAST)
    assert(washingMachine.activeRPM == RPM.FAST)

    assert(washingMachine.activeExtras.isEmpty)
    washingMachine.addExtra(WashingMachineExtra.SpecialColors)
    assert(washingMachine.activeExtras.contains(WashingMachineExtra.SpecialColors))
    washingMachine.addExtra(WashingMachineExtra.SuperDirty)
    assert(washingMachine.activeExtras.size == 2)
    assert(washingMachine.activeExtras.contains(WashingMachineExtra.SuperDirty))
    washingMachine.removeExtra(WashingMachineExtra.SpecialColors)
    assert(washingMachine.activeExtras.size == 1)
    washingMachine.removeExtra(WashingMachineExtra.SuperDirty)
    assert(washingMachine.activeExtras.isEmpty)

    assert(WashingMachineExtra.SuperDirty == WashingMachineExtra("SuperDirty"))
    assertThrows[IllegalArgumentException](WashingMachineExtra("AAA"))
  }

  test("The DishWasher works correctly") {
    assert(dishWasher.activeWashing == DishWasherProgram.FAST)
    dishWasher.setWashingProgram(DishWasherProgram.FRAGILE)
    assert(dishWasher.activeWashing == DishWasherProgram.FRAGILE)

    assert(dishWasher.activeExtras.isEmpty)
    dishWasher.addExtra(DishWasherExtra.SuperDirty)
    assert(dishWasher.activeExtras.contains(DishWasherExtra.SuperDirty))
    dishWasher.addExtra(DishWasherExtra.SuperHygiene)
    assert(dishWasher.activeExtras.size == 2)
    assert(dishWasher.activeExtras.contains(DishWasherExtra.SuperHygiene))
    dishWasher.removeExtra(DishWasherExtra.SuperDirty)
    assert(dishWasher.activeExtras.size == 1)
    dishWasher.removeExtra(DishWasherExtra.SuperHygiene)
    assert(dishWasher.activeExtras.isEmpty)

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

    assert(oven.activeMode == OvenMode.CONVENTIONAL)
    oven.setOvenMode(OvenMode.GRILL)
    assert(oven.activeMode == OvenMode.GRILL)
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

  test("Adding and removing rooms") {
    assert(Rooms.allRooms contains "Salotto")
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
    light.onMessageReceived(light.subTopic, getMsg(CommandMsg(Msg.on), light))
    assert(light.isOn)
    light.onMessageReceived(light.subTopic, getMsg(CommandMsg(Msg.off), light))
    assert(!light.isOn)
    light.onMessageReceived(light.subTopic, getMsg("on", light))
    assert(light.isOn)
    light.onMessageReceived(light.subTopic, getMsg("setIntensity_255", light))
    assert(light.value == 100)
    light.onMessageReceived(light.subTopic, getMsg(CommandMsg(Msg.setIntensity, 35), light))
    assert(light.value == 35)
    light.onMessageReceived(light.subTopic, getMsg(CommandMsg(Msg.setIntensity, 30), light))
    assert(light.value == 30)
    assertThrows[MalformedParametersException](light.onMessageReceived(light.subTopic,"setIntensity_a22"))
    assertThrows[IllegalArgumentException](light.onMessageReceived(light.pubTopic, "off"))
    assert(light.value == 30)
    assert(light.disconnect)
  }
}
