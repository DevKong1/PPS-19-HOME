package HOME

import java.io.{File, FileWriter}

object Application {
  private var devices: Set[AssociableDevice] = Set.empty  //Internal use for instantiating devices

  def main(args: Array[String]): Unit = {
    checkAndCreateLoginFile(Constants.HomePath)
    Constants.defaultRooms foreach Rooms.addRoom
    //GUI.setRooms(Constants.defaultRooms)
    if (!startCoordinator() || !startDevices()) {
      return
    }
    RegisterDevice(devices)

    //Patch, need to have real sensors in Coordinator and not copies for GUI simulation purposes
    for (d <- Coordinator.getDevices.filter(Device.isSensor)) Coordinator.removeDevice(d.name)
    for (d <- devices.filter(_.isInstanceOf[SensorAssociableDevice[_]])) Coordinator.addDevice(d)

    println("Launching GUI")
    LoginPage
    //GUI.top.visible = true
  }

  private def startCoordinator(): Boolean = {
    if (!(Coordinator.connect && Coordinator.subscribe)) {
      println("ERR, can't start Coordinator")
      return false
    }
    true
  }
  private def checkAndCreateLoginFile(filePath:String): Unit ={
    println(filePath)
    val homeDir = new File(filePath)
    if(!homeDir.exists()){
      homeDir.mkdir()
    }
    if(!new File(Constants.LoginPath).exists()){
        ResourceOpener.open(new FileWriter(Constants.LoginPath)) { writer => {
          writer.write("")
        }}
      }
  }
  private def stopCoordinator(): Unit = {
    Coordinator.disconnect
  }

  private def startDevices(): Boolean = {
    /*Rooms.allRooms foreach { d =>
      devices += Light(DeviceIDGenerator(), d)
      devices += Thermometer(DeviceIDGenerator(), d)
      devices += Hygrometer(DeviceIDGenerator(), d)
      devices += MotionSensor(DeviceIDGenerator(), d)
    }*/

    instanceDevice()

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

  /*private def registerDevices(): Unit = {
    Await.ready(Future.sequence(devices.map(_.register)), Duration.Inf).onComplete {
      case Failure(exception) => println("ERR, can't register device, " + exception); closeApplication()
      case Success(_) =>
    }
  }*/

  def closeApplication(): Unit = {
    stopCoordinator()
    stopDevices()
    GUI.top.visible = false
    System.exit(0)
  }

  private def instanceDevice(): Unit = {
    //Add devices to the kitchen
    val light_kitchen: SimulatedLight = Light("Lamp_Kitchen", "Kitchen")
    val oven: SimulatedOven = Oven("Oven", "Kitchen")
    val tv_kitchen: SimulatedTV = TV("TV_Kitchen", "Kitchen")
    val shutter_kitchen: SimulatedShutter = Shutter("Shutter_Kitchen", "Kitchen")
    val dishWasher: SimulatedDishWasher = DishWasher("DishWasher", "Kitchen")

    //Add devices to the Bathroom
    val light_bath: SimulatedLight = Light("Lamp_Bath", "Bathroom")
    val dehumidifier_Bath: SimulatedDehumidifier = Dehumidifier("Dehumidifier_Bath", "Bathroom")
    val shutter_bathroom: SimulatedShutter = Shutter("Shutter_Bath", "Bathroom")

    //Add devices to Living room
    val light_living: SimulatedLight = Light("Lamp_Living", "Living room")
    val dehumidifier_Living: SimulatedDehumidifier = Dehumidifier("Dehumidifier_Living", "Living room")
    val airConditioner_living: SimulatedAirConditioner = AirConditioner("AirConditioner_Bath", "Living room")
    val tv_living: SimulatedTV = TV("TV_Living", "Living room")
    val stereo_living: SimulatedStereoSystem = StereoSystem("Stereo_Living", "Living room")
    val shutter_living: SimulatedShutter = Shutter("Shutter_Living", "Living room")

    //Add devices to Laundry room
    val light_laundry: SimulatedLight = Light("Lamp_Laundry", "Laundry room")
    val shutter_laundry: SimulatedShutter = Shutter("Shutter_Laundry", "Laundry room")
    val washingMachine: SimulatedWashingMachine = WashingMachine("WashingMachine", "Laundry room")

    //Add devices to Garage
    val light_garage: SimulatedLight = Light("Lamp_Garage", "Garage")
    val shutter_garage: SimulatedShutter = Shutter("Shutter_Garage", "Garage")
    //private val boiler: SimulatedBoiler = Boiler("Boiler", "Garage")

    //Add devices to Corridor
    val light_corridor: SimulatedLight = Light("Lamp_Corridor", "Corridor")
    val shutter_corridor: SimulatedShutter = Shutter("Shutter_Corridor", "Corridor")

    //Add devices to Bedroom
    val light_bedroom: SimulatedLight = Light("Lamp_Bedroom", "Bedroom")
    val shutter_bedroom: SimulatedShutter = Shutter("Shutter_Bedroom", "Bedroom")
    val airConditioner_bedroom: SimulatedAirConditioner = AirConditioner("AirConditioner_Bedroom", "Bedroom")
    val tv_bedroom: SimulatedTV = TV("TV_Bedroom", "Bedroom")
    val stereo_bedroom: SimulatedStereoSystem = StereoSystem("Stereo_Bedroom", "Bedroom")

    val externalThermometer: SimulatedThermometer = Thermometer("Thermometer_Home", "Home")
    val externalHygrometer: SimulatedHygrometer = Hygrometer("Hygrometer_Home", "Home")

    for(room <- Rooms.allRooms.filter(_ != "Home")) yield {
      val thermometer: SimulatedThermometer = Thermometer("Thermometer_"+room, room)
      val photometer: SimulatedPhotometer = Photometer("Photometer_"+room, room)
      val motionSensor: SimulatedMotionSensor = MotionSensor("MotionSensor_"+room, room)
      val hygrometer: SimulatedHygrometer = Hygrometer("Hygrometer_"+room, room)
      devices += thermometer
      devices += photometer
      devices += motionSensor
      devices += hygrometer
    }
    devices += externalThermometer
    devices += externalHygrometer

    //Add all devices to Coordinator
    devices += light_kitchen
    devices += oven
    devices += tv_kitchen
    devices += shutter_kitchen
    devices += dishWasher

    devices += light_bath
    devices += dehumidifier_Bath
    devices += shutter_bathroom

    devices += light_living
    devices += dehumidifier_Living
    devices += airConditioner_living
    devices += shutter_living
    devices += tv_living
    devices += stereo_living

    devices += light_laundry
    devices += washingMachine
    devices += shutter_laundry

    devices += light_garage
    devices += shutter_garage
    //Coordinator.addDevice(boiler)

    devices += light_corridor
    devices += shutter_corridor

    devices += light_bedroom
    devices += shutter_bedroom
    devices += tv_bedroom
    devices += airConditioner_bedroom
    devices += stereo_bedroom
  }
}
