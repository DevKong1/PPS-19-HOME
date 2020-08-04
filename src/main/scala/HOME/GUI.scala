package HOME

import scala.language.postfixOps
import java.awt.Color
import java.text.SimpleDateFormat
import java.util.Calendar

import scala.swing._
import scala.swing.event.{ButtonClicked, MouseClicked, SelectionChanged, ValueChanged}
import javax.swing.{Box, ImageIcon}
import javax.swing.border.LineBorder

import HOME.MyClass._

sealed trait Room {
  def devices : Set[Device]
  def name : String
}
sealed trait EditableFeature{
  def update(id: String, device : Device, updateType: String)
  def getVal : String
}

class GUIRoom(override val name:String) extends ScrollPane  {
  /*always present device in every room*/
  /*private val light: SimulatedLight = Light("Lamp", name)
  private val AC: SimulatedAirConditioner = AirConditioner("AirConditioner", name)
  private val dehumidifier: SimulatedDehumidifier = Dehumidifier("Dehumidifier", name)
  private val motionSensor = MotionSensor("Motion",name)
  private val hygrometer = Hygrometer("Hygro",name)
  private val photometer = Photometer("photo",name)*/
  StartingDemo()

  val devicePanel = new BoxPanel(Orientation.Vertical)

  //override def devices: Set[Device] = Set(light, AC, dehumidifier)

  val adDeviceBtn: Button =
    new Button("Add device") {
      reactions += {
        case ButtonClicked(_) => DeviceDialog()
      }
    }

  name toLowerCase match {
    case "home" => contents = HomePage()
    case _ => val bp: BorderPanel = new BorderPanel {
      /*add(new FlowPanel() {
          border = new LineBorder(Color.black,3)
          contents ++=Seq(
            //TODO: ADD BASIC SENSORS
        },BorderPanel.Position.North)*/
      add(devicePanel, BorderPanel.Position.Center)
      add(adDeviceBtn, BorderPanel.Position.South)
    }
      contents = bp
      for (i <- Coordinator.getDevices.filter(_.room==name)) {
        addDevice(PrintDevicePane(i))
      }
  }
  //First release ugly print

  def apply(roomName: String): GUIRoom = new GUIRoom(roomName)
  def addDevice(dev : Component): Unit ={
    val GAP = 5
    devicePanel.peer.add(Box.createVerticalStrut(GAP))
    devicePanel.contents += dev
  }
  override def equals(that: Any):   Boolean = that match{
    case that: GUIRoom => this.name equals that.name
    case _ => false
  }
}
object GUI extends SimpleSwingApplication {
  private val ADD = "+"
  var rooms: Set[GUIRoom] = Set.empty//Set(new GUIRoom("Home"), new GUIRoom("Kitchen"), new GUIRoom("Bedroom"))
  for(i <- Rooms.allRooms) {
    rooms += new GUIRoom(i)
  }
  val tp: TabbedPane = new TabbedPane {
    pages += new TabbedPane.Page("Home", new GUIRoom("Home"))
    for (i <- rooms) yield {
      pages += new TabbedPane.Page(i.name, i)
    }
    pages += new TabbedPane.Page(ADD, new BorderPanel())
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
          val newRoomPane = new TabbedPane.Page(name, newRoom)
          rooms += newRoom
          tp.selection.page = newRoomPane
          tp.pages.insert(last.index, newRoomPane)
        }
    }
    //used to set items in the main window inside a vertical BoxPanel
    contents = tp
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
      if (name.isDefined && name.get.trim.length > 0 && !name.get.equals(ADD) && !tp.pages.exists(page => page.title equals name.get)) {
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

  def handleUpdateMsg(msg: CommandMsg): Unit = msg.command match {
    case Msg.confirmUpdate => println("Success from request " + msg.id) //TODO update GUI
    case _ => this.errUnexpected(UnexpectedMessage, msg.toString)
  }
}

class AddDeviceDialog extends Dialog {
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
      contents ++= Seq(
        new Button("Create"){
          reactions += {
            case ButtonClicked(_) =>
              val name = deviceName.text
              val currentRoom : String = GUI.tp.selection.page.title
              val devType = deviceType.selection.item.toString
              if(name.trim.length > 0){
                for {
                  i <- GUI.rooms.find(_.name equals currentRoom)
                  c <- Device(devType,name,currentRoom)
                }yield{
                  println(i)
                  println(c)
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
}
object DeviceDialog {
  def apply(): AddDeviceDialog = {
    new AddDeviceDialog()
  }
}

class ChangeOrDeleteProfileDialog(delete: String, labelProfile: Label) extends Dialog {
  private val dimension = WindowSize(WindowSizeType.AddProfile)
  private val profiles = new ComboBox[Profile](Profile.getProfiles toSeq)
  preferredSize = dimension
  private val dialog = new BoxPanel(Orientation.Vertical) {
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new FlowPanel() {
        contents += new Label("Profiles: ")
        contents += profiles
      }
    }
    contents += applyDialog
  }
  contents = dialog
  open()

  def applyDialog: Button = {
    delete match {
      case "Change profile" =>
        this.title = "Change Profile"
        new Button("Confirm") {
          reactions += {
            case ButtonClicked(_) => changeProfile
          }
        }
      case "Delete profile" =>
        this.title = "Delete Profile"
        new Button("Delete") {
          reactions += {
            case ButtonClicked(_) => close()
          }
        }
    }
  }

  def changeProfile : Unit = {
    var selectedProfile = profiles.selection.item.toString
    labelProfile.text = "Current active profile: " +  selectedProfile
    selectedProfile match {
      case "DEFAULT_PROFILE" => selectedProfile = Constants.default_profile_name
      case _ =>
    }
    //Coordinator.activeProfile = Profile(selectedProfile)
    close()
  }
}
object ChangeOrDeleteProfile {
  def apply(delete: String, labelProfile: Label): ChangeOrDeleteProfileDialog = {
    new ChangeOrDeleteProfileDialog(delete, labelProfile)
  }
}

class CreateProfileDialog extends Dialog {
  private val profileName = new TextField(10)
  private val description = new TextField(10)
  //private val allRooms = new ComboBox[String](Rooms.allRooms toSeq)
  //private val allDevice = new ComboBox[DeviceType](DeviceType.listTypes toSeq)
  title = "New Profile"

  contents = new GridPanel(6,2) {
    contents += new FlowPanel() {
      contents += new Label("Insert a profile name: ")
      contents += profileName
    }
    contents += new FlowPanel() {
      contents += new Label("Insert a description: ")
      contents += description
    }
    contents += new FlowPanel() {
      contents += new Label("On activation: ")
      contents += new Button("Modify") {
        reactions += {
          case ButtonClicked(_) => AllDevice(Rooms.allRooms, isRoutine = false)
        }
      }
    }
    contents += new FlowPanel() {
      contents += new Label("On sensor changed: ")
      contents += new Button("Add") {
        reactions += {
          case ButtonClicked(_) =>
            SensorReaction()
        }
      }
    }
    contents += new FlowPanel() {
      contents += new Label("Programmed Stuff: ")
      contents += new Button("Devices") {
        reactions += {
          case ButtonClicked(_) => AllDevice(Rooms.allRooms, isRoutine = true)
        }
      }
    }
    contents += new FlowPanel() {
      contents += new Button("Confirm")
      contents += new Button("Cancel") {
        reactions += {
          case ButtonClicked(_) => close()
        }
      }
    }
  }
  open()
}
object CreateProfile {
  def apply(): CreateProfileDialog = {
    new CreateProfileDialog()
  }
}

class SensorReactionDialog extends Dialog {
  //TODO: Used only for testing, need to delete those lines of code
  private val motionSensor = MotionSensor("Motion","Bedroom")
  private val hygrometer = Hygrometer("Hygro","Bedroom")
  private val photometer = Photometer("Photo","Bedroom")
  Coordinator.addDevice(motionSensor)
  Coordinator.addDevice(hygrometer)
  Coordinator.addDevice(photometer)
  //private val sensors = new ComboBox[Device](Coordinator.getDevices.filter(_.isInstanceOf[SensorAssociableDevice[Any]]) toSeq)
  title = "Sensor Reaction"
  location = new Point(300,0)
  contents = new ScrollPane() {
    contents = applyTemplate
  }

  def applyTemplate : BoxPanel = {
    val panel = new BoxPanel(Orientation.Vertical)
    for(i <- Coordinator.getDevices) {
      val devicePanel = new BoxPanel(Orientation.Horizontal) {
        if(i.isInstanceOf[SensorAssociableDevice[_]]) {
          contents += new FlowPanel() {
            contents += new Label(i.name + "-" + i.room)
            contents += new Label("On: ")
            contents += applyComponent(i)
            contents += new TextField(8)
            contents += new Button("Do") {
              reactions += {
                case ButtonClicked(_) => roomsDevices(i.room)
              }
            }
          }
        }
      }
      panel.contents += devicePanel
    }
    panel
  }

  def applyComponent(dev: Device) : Component = dev.deviceType match {
    case MotionSensorType => new ComboBox[String](Set("Detecting", "Not detecting") toSeq)
    case HygrometerType => new ComboBox[String](Set("Humidity =", "Humidity >=", "Humidity <=", "Humidity >", "Humidity <") toSeq)
    case PhotometerType => new ComboBox[String](Set("Intensity =", "Intensity >=", "Intensity <=", "Intensity >", "Intensity <") toSeq)
    case ThermometerType => new ComboBox[String](Set("Temperature =", "Temperature >=", "Temperature <=", "Temperature >", "Temperature <") toSeq)
    case _ => this.errUnexpected(UnexpectedDeviceType, dev.deviceType.toString)
  }

  def roomsDevices(room: String) : Dialog = {
    AllDevice(Set(room), isRoutine = false)
  }
  open()
}
object SensorReaction {
  def apply(): SensorReactionDialog = {
    new SensorReactionDialog()
  }
}

class AddProgrammedStuffDialog extends Dialog {
  title = "Add Programmed Stuff"
  location = new Point(0,250)

  contents = new ScrollPane() {
    contents = applyTemplate
  }

  def applyTemplate : BoxPanel = {
    val panel = new BoxPanel(Orientation.Vertical)
    for(i <- Coordinator.devices) {
      val devPanel = new BoxPanel(Orientation.Horizontal) {
        contents += new FlowPanel() {
          contents += new Label(i.name+""+i.room)
          contents += new BoxPanel(Orientation.Vertical) {
            contents += new TextField(8)
            contents += new TextField(8)
          }
          contents += new Button("Add")
        }
      }
      panel.contents += devPanel
    }
    panel
  }
  open()
}
object AddProgrammedStuff {
  def apply(): AddProgrammedStuffDialog = {
    new AddProgrammedStuffDialog()
  }
}

class AllDeviceDialog(rooms: Set[String], isRoutine: Boolean) extends Dialog {
  title = "All Devices"
  location = new Point(300,250)
  preferredSize = new Dimension(1000,500)

  contents = new ScrollPane() {
    contents = applyTemplate
  }

  def applyTemplate : BoxPanel = {
    val panel = new BoxPanel(Orientation.Vertical)
    for(i <- Coordinator.getDevices) {
      val devPanel = new BoxPanel(Orientation.Horizontal)
      if(!i.isInstanceOf[SensorAssociableDevice[_]] && rooms.contains(i.room)) {
          devPanel.peer.add(Box.createVerticalStrut(5))
          devPanel.border = new LineBorder(Color.BLACK, 2)
          devPanel.contents += new FlowPanel() {
            contents += new Label(i.name)
            MapDeviceCommands.apply(i)
            for(a <- MapDeviceCommands.getCommands) {
              contents += new BoxPanel(Orientation.Vertical) {
                contents += applyComponent(a,i,giveComponent(a,i))
              }
            }
            if(isRoutine) {
              contents += new Label("Start at: ")
              contents += new TextField(8)
              contents += new Label("End at: ")
              contents += new TextField(8)
            }
            contents += new Button("Apply")
          }
        panel.contents += devPanel
      }
    }

    panel.contents += new FlowPanel() {
      contents += new Button("Confirm")
    }
    panel
  }

  //TODO: Create a function to get a component
  def giveComponent(command: String, device: Device): Component = command match {
    case Msg.washingType => new ComboBox[String](Seq(WashingType.MIX, WashingType.RAPID, WashingType.WOOL)map(_.toString))
    case Msg.setProgram => new ComboBox[String](Seq(DishWasherProgram.DIRTY, DishWasherProgram.FAST, DishWasherProgram.FRAGILE)map(_.toString))
    case Msg.RPM =>new ComboBox[String](Seq(RPM.SLOW, RPM.MEDIUM, RPM.FAST)map(_.toString))
    case Msg.addExtra => device.deviceType match {
      case WashingMachineType => new ComboBox[String](Seq(WashingMachineExtra.SpecialColors, WashingMachineExtra.SuperDirty, WashingMachineExtra.SuperDry)map(_.toString))
      case DishWasherType =>new ComboBox[String](Seq(DishWasherExtra.SuperDirty, DishWasherExtra.SuperHygiene, DishWasherExtra.SuperSteam)map(_.toString))
      case _ => null
    }
    case Msg.setMode => new ComboBox[String](Seq(OvenMode.CONVENTIONAL, OvenMode.DEFROSTING, OvenMode.GRILL, OvenMode.LOWER,
      OvenMode.UPPER, OvenMode.VENTILATED)map(_.toString))
    case Msg.open | Msg.on | Msg.mute => new ToggleButton(command) {
      reactions += {
        case ButtonClicked(_) => this.text=switchStatus(this.text)
      }
    }
    case _ => new TextField(10)
  }

  //TODO: Change this function to add at the panel the component get from function giveComponent()
  def applyComponent(command: String, device: Device, component: Component): Panel = command match {
    case Msg.washingType => device.deviceType match {
      case WashingMachineType => new FlowPanel(){
        contents+=new Label("Washing type: ")
        contents+=component
      }
      case _ => null
    }
    case Msg.setProgram => device.deviceType match {
      case DishWasherType => new FlowPanel(){
        contents+=new Label("Program type: ")
        contents+=component
      }
      case _ => null
    }
    case Msg.RPM => device.deviceType match {
      case WashingMachineType | DishWasherType => new FlowPanel(){
        contents+=new Label("RPM: ")
        contents+=component
      }
      case _ => null
    }
    case Msg.addExtra => device.deviceType match {
      case WashingMachineType => new FlowPanel(){
        contents+=new Label("Extras: ")
        contents+=component
      }
      case DishWasherType => new FlowPanel(){
        contents+=new Label("Extras: ")
        contents+=component
      }
      case _ => null
    }
    case Msg.setMode => device.deviceType match {
      case OvenType => new FlowPanel(){
        contents+=new Label("Working mode: ")
        contents+=component
      }
      case _ => null
    }
    case Msg.on | Msg.mute | Msg.open => new FlowPanel() {
      contents+=component
    }
    case _ => new FlowPanel() {
      contents+=new Label(command)
      contents+=component
    }
  }

  def switchStatus(status: String) : String = status match {
    case "on" => "off"
    case "off" => "on"
    case "close" => "open"
    case "open" => "close"
    case "mute" => "muted"
    case _ => "mute"
  }

  open()
}
object AllDevice {
  def apply(rooms: Set[String], isRoutine: Boolean): AllDeviceDialog = {
    new AllDeviceDialog(rooms, isRoutine)
  }
}

class HomePageLayout extends BoxPanel(Orientation.Vertical) {
  val welcomePanel: FlowPanel = new FlowPanel() {
    contents += new Label("Welcome to your HOME") {
      font = new Font("Arial", 0, 36)
    }
  }
  val temperaturePanel: FlowPanel = new FlowPanel() {
    hGap = 70
    contents += new Label("Date: " + DateTime.getDate)
    contents += new Label("Internal temperature: ")
    contents += new Label("External temperature: ")
  }
  val humidityPanel: FlowPanel = new FlowPanel() {
    hGap = 70
    contents += new Label("Time: " + DateTime.getCurrentTime)
    contents += new Label("Internal humidity: ")
    contents += new Label("External humidity: ")
  }
  val alarmPanel: FlowPanel = new FlowPanel() {
    hGap = 70
    contents += new Label("Alarm status")
    contents += new ToggleButton()
  }
  val currentProfile = new Label("Current active profile: " + Coordinator.getActiveProfile)
  val profilePanel: FlowPanel = new FlowPanel() {
    hGap = 70
    contents ++= Seq(currentProfile,
      new Button("Change profile") {
        reactions += {
          case ButtonClicked(_) => ChangeOrDeleteProfile(this.text, currentProfile)
        }
      },
      new Button("Delete profile") {
        reactions += {
          case ButtonClicked(_) => ChangeOrDeleteProfile(this.text, currentProfile)
        }
      },
      new Button("Create profile") {
        reactions += {
          case ButtonClicked(_) => CreateProfile()
        }
      }
    )
  }
  contents ++= Seq(welcomePanel, temperaturePanel, humidityPanel, alarmPanel, profilePanel)
}
object HomePage {
  def apply(): HomePageLayout = {
    new HomePageLayout()
  }
}

abstract class GUIDevice(val d : Device) extends FlowPanel{
  //private val FONT_SIZE : Int = 18
  val ON = "ON"
  //OFF is lazy because of Shutter type, allowing to overwrite its value before using it as button text
  lazy val OFF = "OFF"
  private var status = OFF
  private val devType = new Label("DeviceType: "+d.deviceType)
  private val statusButton = new ToggleButton(status) {
    reactions += {
      case ButtonClicked(_) =>
        this.text = switchStatus { case ON => status = OFF case _ => status = ON }
      // TODO: device needs to be switched on/off
    }
  }
  /** BASIC TEMPLATE */
  val switchStatus: (String => Unit) => String = (c: String => Unit) => {
    c(status)
    status
  }

  border = new LineBorder(Color.BLACK, 2)
  contents += new myIcon(d.name, d.deviceType.toString)
  val deviceInfo = new GridPanel(3, 3)
  deviceInfo.contents ++= Seq(
    devType,
    new Label("Consumption: " + d.consumption),
    statusButton
  )

  if (d.deviceType == TvType) {
    deviceInfo.contents += new ToggleButton("Mute")
  }
  contents += deviceInfo



  /** define a function that depending on the thing to update chooses what to update
   * def updateVal(whatToUpdate: String(?), val : Int) = {
   *  valType match {
   *  case "sensitivity" => d.setSens(val)
   * }
   *
   *
   **/
  private class myIcon(name:String, iconPath :String) extends Label{
    private val JPG = ".jpg"
    text=name
    border = new LineBorder(Color.black,1)
    icon = new ImageIcon(getClass.getClassLoader.getResource(iconPath + JPG) getPath)

    horizontalTextPosition = Alignment.Center
    verticalTextPosition = Alignment.Bottom
  }
}
class DeviceFeature[A <: Component with EditableFeature](featureTitle : String, initialValue: String, setterComponent: A ) extends Label {
  text = initialValue
  border = new LineBorder(Color.black,1)
  reactions+={
    case MouseClicked(_,_,_,_,_) => new Dialog(){
      title = featureTitle
      private var changed = false
      private val value : Label = new Label("Value")

      contents = new BoxPanel(Orientation.Vertical) {
        contents ++= Seq(new FlowPanel() {
          contents ++= Seq(
            new Label("Set "+featureTitle+": "),
            setterComponent,
            value
          )
        },
          new FlowPanel() {
            contents ++= Seq(
              new Button("Confirm") {
                reactions += {
                  case ButtonClicked(_) => if (changed) {
                    println(setterComponent.getVal)
                    //TODO: Update val
                    close()
                  }else close()
                }
              },
              new Button("Cancel") {
                reactions += {
                  case ButtonClicked(_) => close()
                }
              })
          }
        )
      }
      reactions+={
        case ValueChanged(_) => changed = true; value.text = setterComponent getVal
      }
      listenTo(setterComponent)
      open()
    }
  }
  listenTo(mouse.clicks)
  this.visible = true
}
object Feature{
  def apply[A<: Component with EditableFeature](title:String,text:String,setterComponent:A): DeviceFeature[A] = new DeviceFeature(title,text,setterComponent)
}

object PrintDevicePane {
  def apply(device: Device) : GUIDevice = device.deviceType  match{
    case AirConditionerType => AirConditionerPane(AirConditioner(device.name,device.room))
    case DehumidifierType => DehumidifierPane(Dehumidifier(device.name,device.room))
    case DishWasherType => DishWasherPane(DishWasher(device.name,device.room))
    case LightType => LightPane(Light(device.name,device.room))
    case OvenType => OvenPane(Oven(device.name,device.room))
    case ShutterType => ShutterPane(Shutter(device.name,device.room))
    case StereoSystemType => StereoPane(StereoSystem(device.name,device.room))
    case ThermometerType => ThermometerPane(Thermometer(device.name,device.room))
    case TvType => TVPane(TV(device.name,device.room))
    case WashingMachineType => WashingMachinePane(WashingMachine(device.name,device.room))
    case _ => this.errUnexpected(UnexpectedDeviceType, device.deviceType.toString)
  }
}

case class AirConditionerPane(override val d: SimulatedAirConditioner) extends GUIDevice(d){
  require (d.deviceType == AirConditionerType)
  contents++=Seq(new Label("Temperature: "),
    Feature("Temperature",d.value toString,SliderFeature(d.minValue,d.maxValue)))
}//TODO: DONE
case class DehumidifierPane(override val d: SimulatedDehumidifier) extends GUIDevice(d){
  require (d.deviceType == DehumidifierType)
  contents++=Seq(new Label("Humidity %: "),
    Feature("Humidity",d.value toString,SliderFeature(d.minValue,d.maxValue)))
} //TODO:DONE
case class DishWasherPane(override val d: SimulatedDishWasher) extends GUIDevice(d){
  require (d.deviceType == DishWasherType)
  contents++= Seq(
    new Label("Washing program: "),
    Feature("Washing program",d.getWashingProgram toString,ListFeature(Seq(DishWasherProgram.DIRTY,DishWasherProgram.FAST,DishWasherProgram.FRAGILE)map(_ toString))),
    new Label("Extras: "),
    Feature("Extra","Extra",ListFeature(Seq(DishWasherExtra.SuperDirty,DishWasherExtra.SuperHygiene,DishWasherExtra.SuperSteam)map(_ toString))),
  )
} //TODO: DONE
case class LightPane(override val d: SimulatedLight) extends GUIDevice(d) {
  require(d.deviceType == LightType)
  contents++=Seq(new Label("Intensity: "),
    Feature("Intensity",d.value toString,SliderFeature(d.minValue,d.maxValue)))
} //TODO: DONE
case class OvenPane(override val d: SimulatedOven) extends GUIDevice(d){ //TODO: DONE
  require (d.deviceType == OvenType)
  println(d.maxValue)
  contents++=Seq(
    new Label("Oven temperature: "),
    Feature("Oven temperature",d.value toString, SliderFeature(d.minValue,d.maxValue)),
    new Label("Oven Mode: "),
    Feature("Oven mode",d.getOvenMode toString, ListFeature(Seq(OvenMode.CONVENTIONAL,OvenMode.DEFROSTING,OvenMode.GRILL,OvenMode.LOWER,
      OvenMode.UPPER,OvenMode.VENTILATED)map(_ toString)))
  )
}//TODO: DONE
case class ShutterPane(override val d: SimulatedShutter) extends GUIDevice(d){
  require (d.deviceType == ShutterType)
  override val ON = "OPEN"
  override lazy val OFF = "CLOSED"
} //TODO: DONE
case class StereoPane(override val d: SimulatedStereoSystem) extends GUIDevice(d){
  contents++=Seq(
    new Label("Volume: "),
    Feature("Volume",d.value toString,SliderFeature(d.minValue,d.maxValue))
  )
} //TODO: DONE
case class ThermometerPane(override val d: SimulatedThermometer) extends GUIDevice(d){
  require (d.deviceType == ThermometerType)
}
case class TVPane(override val d: SimulatedTV) extends GUIDevice(d){
  require (d.deviceType == TvType)
  contents++=Seq(
    new Label("Volume: "),
    Feature("Volume",d.value toString,SliderFeature(d.minValue,d.maxValue))
  )
} //TODO: DONE
case class WashingMachinePane(override val d: SimulatedWashingMachine) extends GUIDevice(d){
  require (d.deviceType == WashingMachineType)
  contents++= Seq(
    new Label("Working mode: "),
    Feature("Working mode",d.getWashingType toString,ListFeature(Seq(WashingType.RAPID,WashingType.MIX,WashingType.WOOL)map(_ toString))),
    new Label("Extras: "),
    Feature("Extras","Extra",ListFeature(Seq(WashingMachineExtra.SpecialColors,WashingMachineExtra.SuperDirty,WashingMachineExtra.SuperDry)map(_ toString))),
    new Label("RPM: "),
    Feature("RMP",d.getRPM toString,ListFeature(Seq(RPM.FAST,RPM.MEDIUM,RPM.SLOW)map(_ toString)))
  )
} //TODO: DONE


case class SliderFeature(mini : Int, maxi: Int) extends Slider with EditableFeature {
  min = mini
  max = maxi
  override def update(id: String, device: Device, updateType: String): Unit = println("Update")
  override def getVal :String = value toString
}
case class ListFeature(items: Seq[String]) extends ComboBox(items) with EditableFeature {
  override def update(id: String, device: Device, updateType: String): Unit = println("sdf")
  override def getVal : String = selection.item

}


