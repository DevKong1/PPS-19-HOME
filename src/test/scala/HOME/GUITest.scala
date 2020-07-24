package HOME

import org.scalatest.funsuite.AnyFunSuite

class GUITest extends AnyFunSuite {

  val gui = GUI
  val home = new GUIRoom("Home")
  val bedroom = new GUIRoom("Bedroom")
  val kitchen = new GUIRoom("Kitchen")

  test("FailTest") {
    assert(!gui.tp.pages.contains("fjhrfjsdhf"))
  }

  test("FirstComponentsCheck") {
    assert(gui.tp.pages.contains(home))
    assert(gui.tp.pages.contains(bedroom))
    assert(gui.tp.pages.contains(kitchen))
  }

  test("BedroomDevices") {
    assert(bedroom.devices.contains(Light("A", bedroom.name)))
    assert(bedroom.devices.contains(AirConditioner("B", bedroom.name)))
    assert(bedroom.devices.contains(Dehumidifier("C", bedroom.name)))
  }
}