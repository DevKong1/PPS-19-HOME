package HOME

import scala.language.postfixOps
import java.awt.Color
import java.text.SimpleDateFormat
import java.util.Calendar

import scala.swing._
import scala.swing.event.{ButtonClicked, SelectionChanged}
import javax.swing.ImageIcon
import javax.swing.border.LineBorder
import scala.reflect.io.File

sealed trait Room {
  def devices : Set[Device]
  def name : String
}

class GUIRoom(override val name:String) extends ScrollPane with Room {
  /*always present device in every room*/
  private val light: SimulatedLight = Light("Lamp", name)
  private val AC: SimulatedAirConditioner = AirConditioner("AirConditioner", name)
  private val dehumidifier: SimulatedDehumidifier = Dehumidifier("Dehumidifier", name)

  val devicePanel = new BoxPanel(Orientation.Vertical)

  override def devices: Set[Device] = Set(light, AC, dehumidifier)

  val adDeviceBtn: Button =
    new Button("Add device") {
      reactions += {
        case ButtonClicked(_) => DeviceDialog()
      }
    }

  val bp: BorderPanel = new BorderPanel {
    add(devicePanel,BorderPanel.Position.North)
    add(adDeviceBtn, BorderPanel.Position.South)
  }
  contents = bp
  name toLowerCase match{
    case "home" => addDevice(HomePage())
    case _  => for (i <- devices) addDevice(PrintDevicePane(i))
  }

  def apply(roomName: String): GUIRoom = new GUIRoom(roomName)
  def addDevice(dev : Component): Unit ={
    devicePanel.contents+=dev
  }
  override def equals(that: Any): Boolean = that match{
    case that: GUIRoom => this.name equals that.name
    case _ => false
  }
}
object GUI extends SimpleSwingApplication {
  private val ADD = "+"
  var rooms: Set[GUIRoom] = Set(new GUIRoom("Home"), new GUIRoom("Kitchen"), new GUIRoom("Bedroom"))
  val tp: TabbedPane = new TabbedPane {
    for(i <- rooms) yield {pages += new TabbedPane.Page(i.name,i)}
    pages+= new TabbedPane.Page(ADD,new BorderPanel())
  }

  def top: MainFrame = new MainFrame {
    title = "Home!"
    println("Welcome")
    reactions += {
      case SelectionChanged(_) =>
        for {
          last <- getLastIndex()
          name <- getRoomName
        } yield {
          val newRoom = new GUIRoom(name)
          val newRoomPane = new TabbedPane.Page(name,  newRoom)
          rooms += newRoom
          tp.selection.page = newRoomPane
          tp.pages.insert(last.index,newRoomPane)
        }
    }
    //used to set items in the main window inside a vertical BoxPanel
    contents = new BoxPanel(Orientation.Vertical) {
      contents += tp
    }
    listenTo(tp.selection)
    size = WindowSize(WindowSizeType.MainW)

    object getLastIndex {
      def apply(): Option[TabbedPane.Page] = {
        tp.selection.page.title match {
          case ADD => tp.pages.find(page => page.title equals ADD)
          case _ => None
        }
      }
    }

    private def getRoomName: Option[String] = {
      import Dialog._
      val name = showInput(tp,
        "Room name:",
        "Add room",
        Message.Plain,
        Swing.EmptyIcon,
        Nil, "")
      //TODO: THINK OF A MORE FUNCTIONAL WAY TO IMPLEMENT INPUT CHECK
      if (name.isDefined && name.get.trim.length > 0 && !name.get.equals(ADD)&& !tp.pages.exists(page => page.title equals name.get)) {
        Rooms.addRoom(name.get)
        name
      } else {
        if (name.isDefined) {
          showMessage(tp, "Room already existing or incorrect room name", Message.Error toString)
        }
        None
      }
    }
  }
}

class CustomDeviceDialog extends Dialog {
  private val dimension = WindowSize(WindowSizeType.Dialog)
  private val deviceType = new ComboBox[DeviceType](DeviceType.listTypes toSeq)
  private val deviceName = new TextField(10)
  preferredSize = dimension
  title = "Add device"
  contents = new BoxPanel(Orientation.Vertical) {
    private val labels = new FlowPanel() {
      contents ++= Seq(new Label("Device name: "),deviceName,new Label("Device type: "),deviceType)
    }
    private val buttons = new FlowPanel() {
      contents ++=Seq(
        new Button("Create"){
          reactions += {
            case ButtonClicked(_) =>
              val name = deviceName.text
              val currentRoom : String = GUI.tp.selection.page.title
              val devType = deviceType.selection.item.toString
              if(name.trim.length > 0){
                for {
                  i <- GUI.rooms.find(_.name equals currentRoom)
                  c <- stringToDevice(devType,name,currentRoom)
                }yield{
                  i.addDevice(PrintDevicePane(c))
                  Coordinator.addDevice(c)
                  close()
                }
              }
          }
        },
        new Button("Cancel") {
        reactions += {
          case ButtonClicked(_) =>
            close()
        }
      })
    }
    contents++= Seq(labels,buttons)
  }
  open()
  def stringToDevice(devType: String,name:String,room : String) : Option[Device] = devType match{
    case "LightType" => Some(Light(name,room))
    case "AirConditionerType" => Some(AirConditioner(name,room))
    case "DehumidifierType" => Some(Dehumidifier(name,room))
    case "ShutterType" => Some(Shutter(name,room))
    case "BoilerType" => Some(Boiler(name,room))
    case "TvType" => Some(TV(name,room))
    case "WashingMachine" => Some(WashingMachine(name,room))
    case "DishWasherType" => Some(DishWasher(name,room))
    case "OvenType" =>Some(Oven(name,room))
    case "StereoSystem" =>Some( StereoSystem(name,room))
    case _ => None
  }
}
object DeviceDialog {
  def apply(): CustomDeviceDialog = {
    new CustomDeviceDialog()
  }
}

class HomePageLayout extends BoxPanel(Orientation.Vertical) {
  contents += new FlowPanel() {
    contents += new Label("Welcome to your HOME") {
      font = new Font("Arial", 0, 36)
    }
  }
  contents += new FlowPanel() {
    hGap = 70
    contents += new Label("Date: " + getDate)
    contents += new Label("Internal temperature: ")
    contents += new Label("External temperature: ")
  }
  contents += new FlowPanel() {
    hGap = 70
    contents += new Label("Time: " + getCurrentTime)
    contents += new Label("Internal humidity: ")
    contents += new Label("External humidity: ")
  }
  contents += new FlowPanel() {
    hGap = 70
    contents += new Label("Alarm status")
    contents += new ToggleButton() {

    }
  }
  contents += new FlowPanel() {
    hGap = 70
    contents += new Label("Current active profile: ")
    contents += new Button("Change profile")
    contents += new Button("Create profile")
    contents += new Button("Delete profile")
  }

  def getDate : String = {
    val cal = Calendar.getInstance()
    val date =cal.get(Calendar.DATE )
    val month =cal.get(Calendar.MONTH )
    val year =cal.get(Calendar.YEAR )

    date+"/"+month+"/"+year
  }

  def getCurrentTime : String = {
    val today = Calendar.getInstance().getTime
    val currentHour = new SimpleDateFormat("hh").format(today)
    val currentMinute = new SimpleDateFormat("mm").format(today)
    val amOrPm = new SimpleDateFormat("a").format(today)

    currentHour+":"+currentMinute+" " + amOrPm
  }
}
object HomePage {
  def apply(): HomePageLayout = {
    new HomePageLayout()
  }
}

abstract class GUIDevice(val d : Device) extends FlowPanel{
  //private val FONT_SIZE : Int = 18
  /** BASIC TEMPLATE */
     border = new LineBorder(Color.BLACK, 2)
    contents+= new myIcon(d.name,iconPath=d.deviceType.toString)


  /** define a function that depending on the thing to update chooses what to update
   * def updateVal(whatToUpdate: String(?), val : Int) = {
   *  valType match {
   *  case "sensitivity" => d.setSens(val)
   * }
   *
   *
   **/
   class myIcon(name:String, iconPath :String) extends Label{
    private val JPG = ".jpg"
    text=name
    border = new LineBorder(Color.black,1)
    icon = new ImageIcon(getClass.getClassLoader.getResource(iconPath + JPG) getPath)
    
    horizontalTextPosition = Alignment.Center
    verticalTextPosition = Alignment.Bottom

  }
}

object PrintDevicePane {
  def apply(device: Device) : GUIDevice = device.deviceType  match{
    case AirConditionerType => AirConditionerPane(AirConditioner(device.name,device.room))
    case BoilerType => BoilerPane(Boiler(device.name,device.room))
    case DehumidifierType => DehumidifierPane(Dehumidifier(device.name,device.room))
    case DishWasherType => DishWasherPane(DishWasher(device.name,device.room))
    case LightType => LightPane(Light(device.name,device.room))
    case MotionSensorType => MotionSensorPane(MotionSensor(device.name,device.room))
    case OvenType => OvenPane(Oven(device.name,device.room))
    case PhotometerType => PhotometerPane(Photometer(device.name,device.room))
    case ShutterType => ShutterPane(Shutter(device.name,device.room))
    case StereoSystemType => StereoPane(StereoSystem(device.name,device.room))
    case ThermometerType => ThermometerPane(Thermometer(device.name,device.room))
    case TvType => TVPane(TV(device.name,device.room))
    case WashingMachineType => WashingMachinePane(WashingMachine(device.name,device.room))
    case HygrometerType => HygrometerPane(Hygrometer(device.name,device.room))
  }
}

case class AirConditionerPane(override val d: SimulatedAirConditioner) extends GUIDevice(d){
  require (d.deviceType == AirConditionerType)
}
case class HygrometerPane(override val d: SimulatedHygrometer) extends GUIDevice(d){
  require (d.deviceType == HygrometerType)
}
case class BoilerPane(override val d: SimulatedBoiler) extends GUIDevice(d){
  require (d.deviceType == BoilerType)
}
case class DehumidifierPane(override val d: SimulatedDehumidifier) extends GUIDevice(d){
  require (d.deviceType == DehumidifierType)
}
case class DishWasherPane(override val d: SimulatedDishWasher) extends GUIDevice(d){
  require (d.deviceType == DishWasherType)
}
case class LightPane(override val d: SimulatedLight) extends GUIDevice(d) {
  require(d.deviceType == LightType)
  //contents += new Label("Intensity: " + d.consumption)
}
case class MotionSensorPane(override val d: SimulatedMotionSensor) extends GUIDevice(d){
  require (d.deviceType == MotionSensorType)
}
case class OvenPane(override val d: SimulatedOven) extends GUIDevice(d){
  require (d.deviceType == OvenType)
}
case class PhotometerPane(override val d: SimulatedPhotometer) extends GUIDevice(d){
  require (d.deviceType == PhotometerType)
}
case class ShutterPane(override val d: SimulatedShutter) extends GUIDevice(d){
  require (d.deviceType == ShutterType)
}
case class StereoPane(override val d: SimulatedStereoSystem) extends GUIDevice(d){
  require (d.deviceType == StereoSystemType)
}
case class ThermometerPane(override val d: SimulatedThermometer) extends GUIDevice(d){
  require (d.deviceType == ThermometerType)
}
case class TVPane(override val d: SimulatedTV) extends GUIDevice(d){
  require (d.deviceType == TvType)
}
case class WashingMachinePane(override val d: SimulatedWashingMachine) extends GUIDevice(d){
  require (d.deviceType == WashingMachineType)
}




