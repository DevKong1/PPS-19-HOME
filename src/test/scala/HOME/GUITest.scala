package HOME

import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.Eventually
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import HOME.ConstantsTest.testSleepTime

class GUITest extends AnyFunSuite with Eventually with Matchers with BeforeAndAfterAll{

  override def beforeAll(): Unit = {
    Logger.setTestFile()
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    Logger.resetFile()
    Logger.unsetTestFile()
    super.beforeAll()
  }

  val room = "Living room"
  val home = "Home"
  Rooms.addRoom(home)
  Rooms.addRoom(room)

  val light: SimulatedLight = Light("A", room)
  val dehumidifier: SimulatedDehumidifier = Dehumidifier("C", room)

  val thermometerH: SimulatedThermometer = Thermometer("T",home)
  val hygrometerH: SimulatedHygrometer = Hygrometer("H",home)

  val devices:Seq[AssociableDevice] = Seq(light,dehumidifier)



  test("GUIRoom works correctly",BrokerRequired){
    assert(Coordinator.connect)
    assert(Coordinator.subscribe)

    assert(thermometerH.connect)
    assert(thermometerH.subscribe)
    thermometerH.register
    assert( hygrometerH.connect)
    assert( hygrometerH.subscribe)
    hygrometerH.register

    for(d<- devices){
      d.connect
      d.subscribe
      d.register
    }
    eventually {
      Thread.sleep(testSleepTime)
      assert(GUI.rooms.size == 2)
      //assert(GUI.rooms.toList.map(_.devices.size).sum == 4)
    }

    GUI.removeDevice(dehumidifier)
    eventually {
      assert(GUI.rooms.find(_.name == "Home").get.devices.size == 2)
      assert(GUI.rooms.find(_.name == room).get.devices.size == 1)
      assert(Coordinator.getDevices.size == 3)
      assert(!dehumidifier.isConnected)
    }
    val ac = AirConditioner("AC",room)
    GUI.rooms.find(_.name==room).get.addDevice(ac)

    assert(ac.connect)
    assert(ac.subscribe)
    ac.register
    /** can't add devices to home */
    eventually {
      Thread.sleep(testSleepTime)
      assert(GUI.rooms.find(_.name == "Home").get.devices.size == 2)
      assert(GUI.rooms.find(_.name == room).get.devices.size == 2)
      assert(Coordinator.getDevices.size == 4)
    }
    assert(Coordinator.disconnect)
    Coordinator.removeAllDevices()
  }

  test("Updatable devices do actually update",BrokerRequired){
    assert(Coordinator.connect)
    assert(Coordinator.subscribe)
    thermometerH.connect
    thermometerH.subscribe
    thermometerH.register
    hygrometerH.connect
    hygrometerH.subscribe
    hygrometerH.register
    for(d<- devices){
      d.connect
      d.subscribe
      d.register
    }
    assert(Coordinator.publish(light.getSubTopic, "0_on"))
    eventually {
      Thread.sleep(testSleepTime)
      light.isOn should be (true)
      GUI.rooms.find(_.name == room).get.devices.find(_.device.name == "A").get.device.isOn should be(true)
    }

    assert(Coordinator.publish(light.getSubTopic, "0_off"))
    eventually {
      Thread.sleep(testSleepTime)
      light.isOn should be(false)
      GUI.rooms.find(_.name == room).get.devices.find(_.device.name == "A").get.device.isOn should be(false)
    }

    assert(Coordinator.publish(light.getSubTopic, "0_setIntensity_15"))
    eventually {
      Thread.sleep(testSleepTime)
      light.value should be (15)
      GUI.rooms.find(_.name == room).get.devices.find(_.device.name == "A").get.device.asInstanceOf[SimulatedLight].value should be(15)
    }

    assert(Coordinator.disconnect)
    Coordinator.removeAllDevices()
  }
}