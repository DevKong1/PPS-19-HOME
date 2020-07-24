package HOME

import org.scalatest.PrivateMethodTester.PrivateMethod
import org.scalatest.funsuite.AnyFunSuite

import scala.swing.{ComboBox, Component}

class GUITest extends AnyFunSuite {

  val gui = GUI
  val home = new GUIRoom("Home")
  val bedroom = new GUIRoom("Bedroom")
  val kitchen = new GUIRoom("Kitchen")
  val homePanel = HomePage()
  val deviceDialog = DeviceDialog()

  test("FailTest") {
    assert(!gui.tp.pages.contains("Bathroom"))
    //TODO: Need to refactor GUI to let this test work
    //assert(!home.devices.contains(Light("Lamp", home.name)))
    //assert(!home.devices.contains(AirConditioner("AirConditioner", home.name)))
    //assert(!home.devices.contains(Dehumidifier("Dehumidifier", home.name)))
  }

  test("RoomsTest") {
    assert(gui.tp.pages.contains(home))
    assert(gui.tp.pages.contains(bedroom))
    assert(gui.tp.pages.contains(kitchen))
  }

  test("HomePageTest") {
    assert(home.bp.contents.contains(home.devicePanel))
    assert(homePanel.contents.contains(homePanel.welcomePanel))
    assert(homePanel.contents.contains(homePanel.temperaturePanel))
    assert(homePanel.contents.contains(homePanel.humidityPanel))
    assert(homePanel.contents.contains(homePanel.alarmPanel))
    assert(homePanel.contents.contains(homePanel.profilePanel))
  }

  test("BedroomDevicesTest") {
    assert(bedroom.bp.contents.contains(bedroom.devicePanel))
    assert(bedroom.devices.contains(Light("Lamp", bedroom.name)))
    assert(bedroom.devices.contains(AirConditioner("AirConditioner", bedroom.name)))
    assert(bedroom.devices.contains(Dehumidifier("Dehumidifier", bedroom.name)))
  }

  test("KitchenDevicesTest") {
    assert(kitchen.bp.contents.contains(kitchen.devicePanel))
    assert(kitchen.devices.contains(Light("Lamp", kitchen.name)))
    assert(kitchen.devices.contains(AirConditioner("AirConditioner", kitchen.name)))
    assert(kitchen.devices.contains(Dehumidifier("Dehumidifier", kitchen.name)))
  }

  test("DeviceDialogTest") {
    println(""+deviceDialog.getClass)
    //val m = deviceDialog.getClass.getDeclaredField("deviceType")
    //m.setAccessible(true)
    //assert(deviceDialog.contents.contains(m))
  }
}