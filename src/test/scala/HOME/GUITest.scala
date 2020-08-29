package HOME

import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.Eventually
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GUITest extends AnyFunSuite with Eventually with Matchers with BeforeAndAfterAll{
/*
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
  Rooms.addRoom("Home")
  Rooms.addRoom(room)
  val light: SimulatedLight = Light("A", room)
  val AC: SimulatedAirConditioner = AirConditioner("B", room)
  val dehumidifier: SimulatedDehumidifier = Dehumidifier("C", room)

  val thermometerH: SimulatedThermometer = Thermometer("T","Home")
  val hygrometerH: SimulatedHygrometer = Hygrometer("H","Home")
  val devices:Seq[AssociableDevice] = Seq(light,AC,dehumidifier)

  test("Environment setup", BrokerRequired){
    assert(Coordinator.connect)
    assert(Coordinator.subscribe)
    assert(thermometerH.connect)
    assert(thermometerH.subscribe)
    assert(hygrometerH.connect)
    assert(hygrometerH.subscribe)
    Coordinator.addDevice(thermometerH)
    Coordinator.addDevice(hygrometerH)
    for( d <- devices) {
      d.connect
      d.subscribe
      Coordinator.addDevice(d)
    }

    assert(Coordinator.getDevices.size == 5)
  }


  test("GUIRoom works correctly",BrokerRequired){
    assert(GUI.rooms.size == 2)
    assert(GUI.rooms.map(_.devices.size).sum == 5)

    GUI.removeDevice(dehumidifier)
    assert(GUI.rooms.find(_.name=="Home").get.devices.size==2)
    assert(GUI.rooms.find(_.name==room).get.devices.size==2)
    assert(!Coordinator.getDevices.exists(_.name == dehumidifier.name))

    dehumidifier.connect
    dehumidifier.subscribe
    Coordinator.addDevice(dehumidifier)

    GUI.rooms.find(_.name==room).get.addDevice(dehumidifier)
    /** can't add devices to home */
    assert(GUI.rooms.find(_.name=="Home").get.devices.size==2)
    assert(GUI.rooms.find(_.name==room).get.devices.size==3 )
    assert(Coordinator.getDevices(dehumidifier))
  }

  test("Updatable devices do actually update",BrokerRequired){
    assert(Coordinator.publish(light.getSubTopic, "0_on"))
    eventually {
      Thread.sleep(testSleepTime)
      light.isOn should be (true)
      GUI.rooms.find(_.name == room).get.devices.find(_.device.name == "A").get.device.isOn should be(true)
    }

    assert(Coordinator.publish(light, CommandMsg(cmd = Msg.off)))
    eventually {
      Thread.sleep(testSleepTime)
      GUI.rooms.find(_.name == room).get.devices.find(_.device.name == "A").get.device.isOn should be(false)
    }

    assert(Coordinator.publish(light.getSubTopic, "0_setIntensity_15"))
    eventually {
      Thread.sleep(testSleepTime)
      light.value should be (15)
      GUI.rooms.find(_.name == room).get.devices.find(_.device.name == "A").get.device.asInstanceOf[SimulatedLight].value should be(15)
    }
  }*/
}