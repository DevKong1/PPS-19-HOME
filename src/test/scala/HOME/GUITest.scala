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
  val kitchen = "Kitchen"
  Rooms.addRoom(kitchen)
  Rooms.addRoom(room)

  val light: SimulatedLight = Light("A", room)
  val dehumidifier: SimulatedDehumidifier = Dehumidifier("C", room)

  val thermometerH: SimulatedThermometer = Thermometer("T",kitchen)
  val hygrometerH: SimulatedHygrometer = Hygrometer("H",kitchen)

  val devices:Seq[AssociableDevice] = Seq(light,dehumidifier)


  test("GUIRoom works correctly",BrokerRequired){
    start()

    eventually {
      Thread.sleep(testSleepTime)
      GUI.rooms.size should be (3)
      GUI.rooms.toList.map(_.devices.size).sum should be (4)
    }

    GUI.removeDevice(dehumidifier)
    eventually {
      Thread.sleep(testSleepTime)
      GUI.rooms.find(_.name == kitchen).get.devices.size should be (2)
      GUI.rooms.find(_.name == room).get.devices.size should be (1)
      Coordinator.getDevices.size should be (3)
      dehumidifier.isConnected should be (false)
    }

    assert(dehumidifier.connect)
    assert(dehumidifier.subscribe)
    dehumidifier.register
    GUI.rooms.find(_.name==room).get.addDevice(dehumidifier)
    /** can't add devices to home */
    eventually {
      Thread.sleep(testSleepTime)
      GUI.rooms.find(_.name == kitchen).get.devices.size should be (2)
      GUI.rooms.find(_.name == room).get.devices.size should be (2)
      Coordinator.getDevices.size should be (4)
    }

    stop()
  }

  test("Updatable devices do actually update",BrokerRequired){
    start()

    assert(Coordinator.publish(light, CommandMsg(cmd = Msg.on) ))
    eventually {
      Thread.sleep(testSleepTime)
      light.isOn should be (true)
      GUI.rooms.find(_.name == room).get.devices.find(_.device.name == "A").get.device.isOn should be(true)
    }

    assert(Coordinator.publish(light, CommandMsg(cmd = Msg.off)))
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

    stop()
  }

  private def start():Unit = {
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

    GUI.rooms.find(_.name == kitchen).get.addDevice(thermometerH)
    GUI.rooms.find(_.name == kitchen).get.addDevice(hygrometerH)
    GUI.rooms.find(_.name == room).get.addDevice(light)
    GUI.rooms.find(_.name == room).get.addDevice(dehumidifier)
  }

  private def stop():Unit = {
    Coordinator.removeAllDevices()
    assert(Coordinator.disconnect)
  }
}