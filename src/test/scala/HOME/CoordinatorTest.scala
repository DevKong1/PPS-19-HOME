package HOME

import org.scalatest.funsuite.AnyFunSuite

class CoordinatorTest extends AnyFunSuite {

  val controller: Coordinator = CoordinatorImpl()

  test("Basic coordinator with no devices"){
    assert(controller.getDevices.isEmpty)
  }

  test("Adding and removing devices"){
    controller.addDevice(Light("Light","salotto"))
    assert(controller.getDevices.size == 1)
    controller.addDevice(Light("Light","salotto"))
    assert(controller.getDevices.size == 2)
    controller.removeDevice(Light("Light","salotto"))
    assert(controller.getDevices.size == 1)
    controller.removeDevice(Light("Light","salotto"))
    assert(controller.getDevices.isEmpty)
  }

}