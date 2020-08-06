package HOME

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object Application {
  private var devices: Set[AssociableDevice] = Set.empty  //Internal use for instantiating devices

  def main(args: Array[String]): Unit = {
    Constants.defaultRooms foreach Rooms.addRoom
    //GUI.setRooms(Constants.defaultRooms)
    if (!startCoordinator() || !startDevices()) {
      return
    }
    registerDevices()
    println("Launching GUI")
    GUI.pack()
    GUI.top.visible = true
  }

  private def startCoordinator(): Boolean = {
    if (!(Coordinator.connect && Coordinator.subscribe)) {
      println("ERR, can't start Coordinator")
      return false
    }
    true
  }

  private def stopCoordinator(): Unit = {
    Coordinator.disconnect
  }

  private def startDevices(): Boolean = {
    Rooms.allRooms foreach { d =>
      devices += Light(DeviceIDGenerator(), d)
      devices += Thermometer(DeviceIDGenerator(), d)
      devices += Hygrometer(DeviceIDGenerator(), d)
      devices += MotionSensor(DeviceIDGenerator(), d)
    }

    devices foreach { d =>
      if (!(d.connect && d.subscribe)) {
        println("ERR, can't start device: " + d)
        return false
      }
    }
    true
  }

  private def stopDevices(): Unit = {
    Coordinator.getDevices.foreach {
      case d: AssociableDevice => d.disconnect
      case _ => //Do nothing
    }
  }

  private def registerDevices(): Unit = {
    Await.ready(Future.sequence(devices.map(_.register)), Duration.Inf).onComplete {
      case Failure(exception) => println("ERR, can't register device, " + exception); closeApplication()
      case Success(_) =>
    }
  }

  private def closeApplication(): Unit = {
    stopCoordinator()
    stopDevices()
    GUI.top.visible = false
    System.exit(0)
  }
}
