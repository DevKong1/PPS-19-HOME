package HOME

import java.io.File
import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import HOME.MyClass._
import com.github.tototoshi.csv._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.language.implicitConversions
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import scala.language.reflectiveCalls

object Constants {
  //Room in every house
  val defaultRooms = Set("Home","Kitchen", "Garage", "Bedroom", "Bathroom", "Living room", "Corridor", "Laundry room")

  /** devices always present in every room
   *
   * @param name room name
   * @return set of devices in given room
   *
   * Generates dinamically a set of devices.
   */
  def devicesPerRoom(name: String) :Set[Device]= Set(Light(DeviceIDGenerator(),name),Thermometer(DeviceIDGenerator(),name),Hygrometer(DeviceIDGenerator(),name),MotionSensor(DeviceIDGenerator(),name))
  def default_profile_name: String = "DEFAULT"
  def dayLightValue: Int = 40
  val GUIDeviceGAP = 5
  val IconExt = ".jpg"
  val LoginTextSize = 20
  val AddPane = "+"
  val registrationTimeout = 500
  val outputDateFormat: org.joda.time.format.DateTimeFormatter = org.joda.time.format.DateTimeFormat.forPattern("MM/dd/yyyy HH:mm:ss")
  val maxWaitTime: FiniteDuration = 30.seconds
  val HomePath: String = System.getProperty("user.home")+File.separatorChar+"HOME"
  val LoginPath: String = HomePath+File.separatorChar+"login.txt"

}

object Logger {
  private val fileName: String = "Log.csv"
  private var csvFile: File = new File(fileName)
  private val header = List("ID","Date","CMD","Consumption")

  if(!Files.exists(Paths.get(fileName))) resetFile()

  def getLogAsListWithHeader : List[Map[String,String]] = CSVReader.open(csvFile).allWithHeaders()
  def getLogAsStream : Stream[List[String]] = CSVReader.open(csvFile).toStream

  def log(args: String*): Boolean = {
    try {
      val writer = CSVWriter.open(csvFile, append = true)
      writer.writeRow(args.toList)
      writer.close()
      true
    } catch {
      case _: Throwable => false
    }
  }

  def resetFile(): Unit = {
    val writer = CSVWriter.open(csvFile)
    writer.writeRow(header)
    writer.close()
  }

  //ONLY FOR TESTING
  def setTestFile() : Unit = {csvFile = new File("test.csv"); resetFile()}
  def unsetTestFile() : Unit = {resetFile(); csvFile = new File(fileName)}
}

/** Gives unique identifier to devices*/
object DeviceIDGenerator {
  private var id = 0

  /** simple counter
   *
   * @return new device ID
   */
  def apply(): String = {
    id += 1
    id.toString
  }
}

/** Abstracts resource using from opening and closing
 *
 * Implements loan pattern
 */
object ResourceOpener{
  /** Opens a resource stream and applies a function to it, then closes the stream.
   *
   * @param file to open
   * @param f function to apply to resource stream
   * @tparam A element that define "close()" method
   * @tparam B return type
   * @return an element of type B
   *
   * used to open resources, takes an element that defines close() method (usually a file stream) as first parameter and
   * a supplier function that maps such element in another type as second. Once the function is applied, such stream is closed.
   *
   */
   def open[A <: { def close(): Unit }, B](file: A)(f: A => B): B =
    try {
      f(file)
    } finally {
      file.close()
    }
}

/** Used to connect devices to Coordinator
 *
 * Whenever a device joins the system, it registers to Coordinator via this utility
 */
object RegisterDevice {
  /** handles a single device
   *
   * @param d device to register to coordinator
   * @return a future representing when the device will be connected
   *
   * When d successfully connects and notifies [[Coordinator]], this promise is completed
   */
  def apply(d : AssociableDevice): Future[Unit] = {
    val p = Promise[Unit]
    startDevice(d)
    registerDevice(d,p)
    p.future
  }
  /** handles a set of devices
   *
   * @param d devices to register to coordinator
   * @return a future representing when the device will be connected
   *
   * When all devices successfully connects and notifies [[Coordinator]], this promise is completed
   */
  def apply(d : Set[AssociableDevice]):Future[Unit] ={
    val p = Promise[Unit]
    d foreach startDevice
    Await.ready(Future.sequence(d.map(_.register)), Duration.Inf).onComplete {
      case Failure(exception) => println("ERR, can't register device, " + exception);
      case Success(_) =>p.success(()=>Unit)
    }
    p.future
  }

  /** connects a device to MQTT broker and register it to its topics
   *
   * @param d device to connect
   */
  private def startDevice(d : AssociableDevice): Unit ={
    d.connect && d.subscribe
  }

  /** register a device to [[Coordinator]]
   *
   * @param d device to register
   * @param p the promise to either complete or fail when the device'll be connected
   */
  private def registerDevice(d:AssociableDevice,p:Promise[Unit]) : Unit = {
    Await.ready(d.register, Duration.create(Constants.registrationTimeout,TimeUnit.MILLISECONDS)).onComplete {
      case Success(_) => p.success(()=>Unit)
      case Failure(exception) => println("ERR, can't register device, " + exception);
    }
  }
}

/** Handler of user update requests on devices
 *
 * When a user updates a device via GUI, such update is handled by this utility object.
 * It holds a Map of RequestId-Promise where requestId is generated by this object and will be contained in the request sent to the device via MQTT).
 * When the updated devices confirms such update to [[Coordinator]], handleRequest will be called and the promise linked to such requestID will be
 * completed.
 */
object RequestHandler {
  private var updateRequests : Map[Int,Promise[Unit]]= Map.empty
  private var nextNumber : Int = 0

  /** adds a new request
   *
   * @param newRequest promise to fullfill
   * @return a new request ID.
   */
  def addRequest(newRequest :Promise[Unit]): Int = {
    nextNumber += 1
    updateRequests += (nextNumber -> newRequest); nextNumber
  }

  /** completes one of the promises.
   *
   * @param id the promise to complete
   */
  def handleRequest(id : Int): Unit = {
    updateRequests(id).success(() => Unit); updateRequests -= id}
}

//pimp my library
case class MyClass(_class: Any) {
  def getSimpleClassName: String = _class.getClass.getSimpleName.split("\\$").last

  def errUnexpected[A](item: Unexpected, value: String): A =
    throw new IllegalArgumentException("Unexpected " + item.item + ": " + value)
}

object MyClass{
  implicit def toMyClass(_class: Any): MyClass = MyClass(_class)
}

case class MyIterable[A](_iterable: Iterable[A]) {
  def findSimpleClassName(item: String): Boolean = _iterable.find(_.getSimpleClassName == item) match {
    case Some(_) => true
    case _ => false
  }
}

object MyIterable{
  implicit def toMyIterable[A](_iterable: Iterable[A]): MyIterable[A] = MyIterable(_iterable)
}

//helper object used by various devices to set the output strength
object ValueChecker {
  def apply(min: Int, max: Int)(value: Int): Int = value match {
    case x if x > max => max
    case x if x < min => min
    case _ => value
  }
}
sealed trait Unexpected {
  var item: String
}
case object UnexpectedTopic extends Unexpected {
  override var item: String = "topic"
}
case object UnexpectedMessage extends Unexpected {
  override var item: String = "message"
}
case object UnexpectedRoom extends Unexpected {
  override var item: String = "room"
}
case object UnexpectedDevice extends Unexpected {
  override var item: String = "device"
}
case object UnexpectedDeviceType extends Unexpected {
  override var item: String = "deviceType"
}
case object UnexpectedResult extends Unexpected {
  override var item: String = "result"
}
case object UnexpectedProfile extends Unexpected {
  override var item: String = "profile"
}
case object UnexpectedValue extends Unexpected {
  override var item: String = "value"
}

trait WashingType
object WashingType {

  case object MIX extends WashingType
  case object WOOL extends WashingType
  case object RAPID extends WashingType

  def apply(washingType: String): WashingType = washingType match{
    case "MIX" => MIX
    case "WOOL" => WOOL
    case "RAPID" => RAPID
    case _ => this.errUnexpected(UnexpectedMessage, washingType)
  }
}
trait UpdateDevice
object UpdateDevice {

  case object INTENSITY extends UpdateDevice
  case object WORK_MODE extends UpdateDevice
  case object VOLUME extends UpdateDevice
  case object RPM extends UpdateDevice
  case object ON extends UpdateDevice
  case object OFF extends UpdateDevice
  case object OV_MODE extends UpdateDevice
  case object OV_TEMP extends UpdateDevice
  case object WASH_MODE extends UpdateDevice
  case object HUM extends UpdateDevice
  case object TEMP extends UpdateDevice

  def apply(devType: String): UpdateDevice = devType match{
    case "INTENSITY" => INTENSITY
    case "WORK_MODE" => WORK_MODE
    case "VOLUME" => VOLUME
    case "RPM" => RPM
    case "ON" => ON
    case "OFF" => OFF
    case "OV_MODE" => OV_MODE
    case "OV_TEMP" => OV_TEMP
    case "WASH_MODE" => WASH_MODE
    case "HUM" => HUM
    case "TEMP" => TEMP
    case _ => this.errUnexpected(UnexpectedMessage, devType)
  }
}
object Updater {


  def update(device : Device)(value:Int)(updateInfo: UpdateDevice)(implicit updateTypes: UpdateTypes[Device]): Unit ={
    updateTypes.update(device)(value)(updateInfo)
  }
}
abstract class UpdateTypes [A <: Device] {
  def update(device: A)(value:Int)( deviceType: UpdateDevice)
}

trait RPM
object RPM {

  case object SLOW extends RPM
  case object MEDIUM extends RPM
  case object FAST extends RPM

  def apply(rpm: String): RPM = rpm match{
    case "SLOW" => SLOW
    case "MEDIUM" => MEDIUM
    case "FAST" => FAST
    case _ => this.errUnexpected(UnexpectedMessage, rpm)
  }
}

trait GenericExtra

trait WashingMachineExtra extends GenericExtra
object WashingMachineExtra {
  case object SuperDry extends WashingMachineExtra
  case object SuperDirty extends WashingMachineExtra
  case object SpecialColors extends WashingMachineExtra

  def apply(extra: String): WashingMachineExtra = extra match{
    case "SuperDry" => SuperDry
    case "SuperDirty" => SuperDirty
    case "SpecialColors" => SpecialColors
    case _ => this.errUnexpected(UnexpectedMessage, extra)
  }
}

trait DishWasherProgram
object DishWasherProgram {

  case object FAST extends DishWasherProgram
  case object DIRTY extends DishWasherProgram
  case object FRAGILE extends DishWasherProgram

  def apply(dishWasherProgram: String): DishWasherProgram = dishWasherProgram match{
    case "FAST" => FAST
    case "DIRTY" => DIRTY
    case "FRAGILE" => FRAGILE
    case _ => this.errUnexpected(UnexpectedMessage, dishWasherProgram)
  }
}

trait DishWasherExtra extends GenericExtra
object DishWasherExtra {

  case object SuperSteam extends DishWasherExtra
  case object SuperDirty extends DishWasherExtra
  case object SuperHygiene extends DishWasherExtra

  def apply(dishWasherExtra: String): DishWasherExtra = dishWasherExtra match{
    case "SuperSteam" => SuperSteam
    case "SuperDirty" => SuperDirty
    case "SuperHygiene" => SuperHygiene
    case _ => this.errUnexpected(UnexpectedMessage, dishWasherExtra)
  }
}

trait OvenMode
object OvenMode {

  case object CONVENTIONAL extends OvenMode
  case object UPPER extends OvenMode
  case object LOWER extends OvenMode
  case object VENTILATED extends OvenMode
  case object GRILL extends OvenMode
  case object DEFROSTING extends OvenMode

  def apply(ovenMode: String): OvenMode = ovenMode match{
    case "CONVENTIONAL" => CONVENTIONAL
    case "UPPER" => UPPER
    case "LOWER" => LOWER
    case "VENTILATED" => VENTILATED
    case "GRILL" => GRILL
    case "DEFROSTING" => DEFROSTING
    case _ => this.errUnexpected(UnexpectedMessage, ovenMode)
  }
}

//TODO add a checkAndRemove pimping the Iterable

object DummyUtils {
  val dummySet: Set[Device => Unit] = Set({_.id})
  val dummyCheck: (String, Double) => Boolean = (_,_) => false
  val dummyMap: Map[(String, Double) => Boolean, Set[Device => Unit]] = Map(dummyCheck -> dummySet)
}

//A little object for create a starting demo of the program
/*object StartingDemo {

  //Add devices to the kitchen
  private val light_kitchen: SimulatedLight = Light("Lamp_Kitchen", "Kitchen")
  private val oven: SimulatedOven = Oven("Oven", "Kitchen")
  private val tv_kitchen: SimulatedTV = TV("TV_Kitchen", "Kitchen")
  private val shutter_kitchen: SimulatedShutter = Shutter("Shutter_Kitchen", "Kitchen")
  private val dishWasher: SimulatedDishWasher = DishWasher("DishWasher", "Kitchen")

  //Add devices to the Bathroom
  private val light_bath: SimulatedLight = Light("Lamp_Bath", "Bathroom")
  private val dehumidifier_Bath: SimulatedDehumidifier = Dehumidifier("Dehumidifier_Bath", "Bathroom")
  private val shutter_bathroom: SimulatedShutter = Shutter("Shutter_Bath", "Bathroom")

  //Add devices to Living room
  private val light_living: SimulatedLight = Light("Lamp_Living", "Living room")
  private val dehumidifier_Living: SimulatedDehumidifier = Dehumidifier("Dehumidifier_Living", "Living room")
  private val airConditioner_living: SimulatedAirConditioner = AirConditioner("AirConditioner_Bath", "Living room")
  private val tv_living: SimulatedTV = TV("TV_Living", "Living room")
  private val stereo_living: SimulatedStereoSystem = StereoSystem("Stereo_Living", "Living room")
  private val shutter_living: SimulatedShutter = Shutter("Shutter_Living", "Living room")

  //Add devices to Laundry room
  private val light_laundry: SimulatedLight = Light("Lamp_Laundry", "Laundry room")
  private val shutter_laundry: SimulatedShutter = Shutter("Shutter_Laundry", "Laundry room")
  private val washingMachine: SimulatedWashingMachine = WashingMachine("WashingMachine", "Laundry room")

  //Add devices to Garage
  private val light_garage: SimulatedLight = Light("Lamp_Garage", "Garage")
  private val shutter_garage: SimulatedShutter = Shutter("Shutter_Garage", "Garage")
  //private val boiler: SimulatedBoiler = Boiler("Boiler", "Garage")

  //Add devices to Corridor
  private val light_corridor: SimulatedLight = Light("Lamp_Corridor", "Corridor")
  private val shutter_corridor: SimulatedShutter = Shutter("Shutter_Corridor", "Corridor")

  //Add devices to Bedroom
  private val light_bedroom: SimulatedLight = Light("Lamp_Bedroom", "Bedroom")
  private val shutter_bedroom: SimulatedShutter = Shutter("Shutter_Bedroom", "Bedroom")
  private val airConditioner_bedroom: SimulatedAirConditioner = AirConditioner("AirConditioner_Bedroom", "Bedroom")
  private val tv_bedroom: SimulatedTV = TV("TV_Bedroom", "Bedroom")
  private val stereo_bedroom: SimulatedStereoSystem = StereoSystem("Stereo_Bedroom", "Bedroom")

  for(room <- Rooms.allRooms) yield {
    val thermometer: SimulatedThermometer = Thermometer("Thermometer_"+room, room)
    val photometer: SimulatedPhotometer = Photometer("Photometer_"+room, room)
    val motionSensor: SimulatedMotionSensor = MotionSensor("MotionSensor_"+room, room)
    val hygrometer: SimulatedHygrometer = Hygrometer("Hygrometer_"+room, room)
    Coordinator.addDevice(thermometer)
    Coordinator.addDevice(photometer)
    Coordinator.addDevice(motionSensor)
    Coordinator.addDevice(hygrometer)
  }

  def apply(): Unit = {
    //Add all devices to Coordinator
    Coordinator.addDevice(light_kitchen)
    Coordinator.addDevice(oven)
    Coordinator.addDevice(tv_kitchen)
    Coordinator.addDevice(shutter_kitchen)
    Coordinator.addDevice(dishWasher)

    Coordinator.addDevice(light_bath)
    Coordinator.addDevice(dehumidifier_Bath)
    Coordinator.addDevice(shutter_bathroom)

    Coordinator.addDevice(light_living)
    Coordinator.addDevice(dehumidifier_Living)
    Coordinator.addDevice(airConditioner_living)
    Coordinator.addDevice(shutter_living)
    Coordinator.addDevice(tv_living)
    Coordinator.addDevice(stereo_living)

    Coordinator.addDevice(light_laundry)
    Coordinator.addDevice(washingMachine)
    Coordinator.addDevice(shutter_laundry)

    Coordinator.addDevice(light_garage)
    Coordinator.addDevice(shutter_garage)
    //Coordinator.addDevice(boiler)

    Coordinator.addDevice(light_corridor)
    Coordinator.addDevice(shutter_corridor)

    Coordinator.addDevice(light_bedroom)
    Coordinator.addDevice(shutter_bedroom)
    Coordinator.addDevice(tv_bedroom)
    Coordinator.addDevice(airConditioner_bedroom)
    Coordinator.addDevice(stereo_bedroom)
  }
}*/