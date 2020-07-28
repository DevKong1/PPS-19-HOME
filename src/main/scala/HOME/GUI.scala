package HOME

import scala.language.postfixOps
import java.awt.Color
import java.text.SimpleDateFormat
import java.util.Calendar

import scala.swing._
import scala.swing.event.{ButtonClicked, MouseClicked, SelectionChanged, ValueChanged}
import javax.swing.{Box, ImageIcon}
import javax.swing.border.LineBorder

sealed trait Room {
  def devices : Set[Device]
  def name : String
}
sealed trait EditableFeature {
  def update(id: String, device : Device, updateType: String)
}

class GUIRoom(override val name:String) extends ScrollPane with Room {
  /*always present device in every room*/
  private val light: SimulatedLight = Light("Lamp", name)
  private val AC: SimulatedAirConditioner = AirConditioner("AirConditioner", name)
  private val dehumidifier: SimulatedDehumidifier = Dehumidifier("Dehumidifier", name)
  private val motionSensor = MotionSensor("Motion",name)
  private val hygrometer = Hygrometer("Hygro",name)
  private val photometer = Photometer("photo",name)

  val devicePanel = new BoxPanel(Orientation.Vertical)

  override def devices: Set[Device] = Set(light, AC, dehumidifier)

  val adDeviceBtn: Button =
    new Button("Add device") {
      reactions += {
        case ButtonClicked(_) => DeviceDialog()
      }
    }



  name toLowerCase match {
    case "home" => contents=HomePage()
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
      for (i <- devices) {
        addDevice(PrintDevicePane(i))
      }
  }

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
  def stringToDevice(devType: String,name:String,room : String) : Option[Device] = devType match{
    case "LightType" => Some(Light(name,room))
    case "AirConditionerType" => Some(AirConditioner(name,room))
    case "DehumidifierType" => Some(Dehumidifier(name,room))
    case "ShutterType" => Some(Shutter(name,room))
    case "BoilerType" => Some(Boiler(name,room))
    case "TvType" => Some(TV(name,room))
    case "WashingMachineType" => Some(WashingMachine(name,room))
    case "DishWasherType" => Some(DishWasher(name,room))
    case "OvenType" =>Some(Oven(name,room))
    case "StereoSystemType" =>Some(StereoSystem(name,room))
    case _ => None
  }
}
object DeviceDialog {
  def apply(): AddDeviceDialog = {
    new AddDeviceDialog()
  }
}

class ChangeProfileDialog(delete: String, labelProfile: Label) extends Dialog {
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
            case ButtonClicked(_) => {
              changeProfile
            }
          }
        }
      case "Delete profile" =>
        this.title = "Delete Profile"
        new Button("Delete") {
          reactions += {
            case ButtonClicked(_) => {
              close()
            }
          }
        }
    }
  }

  def changeProfile : Unit = {
    var selectedProfile = profiles.selection.item.toString
    labelProfile.text = "Current active profile: " +  selectedProfile
    selectedProfile match {
      case "DEFAULT_PROFILE" => selectedProfile = constants.default_profile_name
      case _ =>
    }
    Coordinator.activeProfile = Profile(selectedProfile)
    close()
  }
}
object ChangeProfile {
  def apply(delete: String, labelProfile: Label): ChangeProfileDialog = {
    new ChangeProfileDialog(delete, labelProfile)
  }
}

class CreateProfileDialog extends Dialog {
  private val profileName = new TextField(10)
  private val description = new TextField(10)
  private val allRooms = new ComboBox[String](Rooms.allRooms toSeq)
  private val allDevice = new ComboBox[DeviceType](DeviceType.listTypes toSeq)
  title = "New Profile"

  contents = new GridPanel(5,2) {
    contents += new FlowPanel() {
      contents += new Label("Insert a profile name: ")
      contents += profileName
    }
    contents += new FlowPanel() {
      contents += new Label("Insert a description: ")
      contents += description
    }
    contents += new FlowPanel() {
      contents += new Label("Select a room: ")
      contents += allRooms
    }
    contents += new FlowPanel() {
      contents += new Label("Select a device: ")
      contents += allDevice
    }
    contents += new FlowPanel() {
      contents += new Button("Confirm")
      contents += new Button("Cancel") {
        reactions += {
          case ButtonClicked(_) => {
            close()
          }
        }
      }
    }
  }
  open()

  //TODO: Change device's ComboBox based on selected room
  /*def getDevices() : Unit = allRooms.selection.item match {
    case "Bedroom" =>
  }*/

}
object CreateProfile {
  def apply(): CreateProfileDialog = {
    new CreateProfileDialog()
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
    contents += new Label("Date: " + getDate)
    contents += new Label("Internal temperature: ")
    contents += new Label("External temperature: ")
  }
  val humidityPanel: FlowPanel = new FlowPanel() {
    hGap = 70
    contents += new Label("Time: " + getCurrentTime)
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
    contents += currentProfile
    contents += new Button("Change profile") {
      reactions += {
        case ButtonClicked(_) => {
          ChangeProfile(this.text, currentProfile)
        }
      }
    }
    contents += new Button("Create profile") {
      reactions += {
        case ButtonClicked(_) => {
          CreateProfile()
        }
      }
    }
    contents += new Button("Delete profile") {
      reactions += {
        case ButtonClicked(_) => {
          ChangeProfile(this.text, currentProfile)
        }
      }
    }
  }

  contents += welcomePanel
  contents += temperaturePanel
  contents += humidityPanel
  contents += alarmPanel
  contents += profilePanel

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
   val ON = "ON"
  //OFF is lazy because of Shutter type, allowing to overwrite its value before using it as button text
   lazy val OFF = "OFF"
  private var status = OFF
  /** BASIC TEMPLATE */
  val switchStatus: (String => Unit) => String = (c : String => Unit) => {
    c(status)
    status
  }
     border = new LineBorder(Color.BLACK, 2)
    contents+= new myIcon(d.name,d.deviceType.toString)
    val deviceInfo =new GridPanel(3,3)
    deviceInfo.contents++= Seq(
      new Label("DeviceType: "+d.deviceType),
      new Label("Consumption: "+d.consumption),
      new ToggleButton(status){
          reactions+={
            case ButtonClicked(_) =>
              text = switchStatus{ case ON => status = OFF case _ => status = ON }
              // TODO: device needs to be switched on/off
          }
      }
    )
  contents+=deviceInfo



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
class DeviceFeature[A <: RichWindow](txt: String, setterComponent: A ) extends Label with EditableFeature{
  text = txt
  border = new LineBorder(Color.black,1)
  reactions+={
    case MouseClicked(_,_,_,_,_) => setterComponent open
  }
  listenTo(mouse.clicks)
  this.visible = true
  override def update(id: String, device: Device, updateType: String): Unit = ???
}
object Feature{
  def apply[A<:RichWindow](text:String,setterComponent:A): DeviceFeature[A] = new DeviceFeature(text,setterComponent)
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
  }
}

case class AirConditionerPane(override val d: SimulatedAirConditioner) extends GUIDevice(d){
  require (d.deviceType == AirConditionerType)
  contents++=Seq(new Label("Temperature: "),
  Feature(d.value toString,SliderFeature("Temperature",d.minValue,d.maxValue)))
}//TODO: DONE
case class DehumidifierPane(override val d: SimulatedDehumidifier) extends GUIDevice(d){
  require (d.deviceType == DehumidifierType)
  contents++=Seq(new Label("Humidity %: "),
    Feature(d.value toString,SliderFeature("Humidity",d.minValue,d.maxValue)))
} //TODO:DONE
case class DishWasherPane(override val d: SimulatedDishWasher) extends GUIDevice(d){
  require (d.deviceType == DishWasherType)
  contents++= Seq(
    new Label("Washing program: "),
    Feature(d.getWashingProgram toString,ListFeature("WashingProgram",Seq(DishWasherProgram.DIRTY,DishWasherProgram.FAST,DishWasherProgram.FRAGILE)map(_ toString))),
    new Label("Extras: "),
    Feature("Extra",ListFeature("Extras",Seq(DishWasherExtra.SuperDirty,DishWasherExtra.SuperHygiene,DishWasherExtra.SuperSteam)map(_ toString))),
  )
} //TODO: DONE
case class LightPane(override val d: SimulatedLight) extends GUIDevice(d) {
  require(d.deviceType == LightType)
  contents++=Seq(new Label("Intensity: "),
  Feature(d.value toString,SliderFeature("Intensity",d.minValue,d.maxValue)))
} //TODO: DONE
case class OvenPane(override val d: SimulatedOven) extends GUIDevice(d){ //TODO: DONE
  require (d.deviceType == OvenType)
  println(d.maxValue)
  contents++=Seq(
    new Label("Oven temperature: "),
    Feature(d.value toString, SliderFeature("Temperature",d.minValue,d.maxValue)),
    new Label("Oven Mode: "),
    Feature(d.getOvenMode toString, ListFeature("Mode",Seq(OvenMode.CONVENTIONAL,OvenMode.DEFROSTING,OvenMode.GRILL,OvenMode.LOWER,
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
    Feature(d.value toString,SliderFeature("Volume",d.minValue,d.maxValue))
  )
} //TODO: DONE
case class ThermometerPane(override val d: SimulatedThermometer) extends GUIDevice(d){
  require (d.deviceType == ThermometerType)
}
case class TVPane(override val d: SimulatedTV) extends GUIDevice(d){
  require (d.deviceType == TvType)
  contents++=Seq(
    new Label("Volume: "),
    Feature(d.value toString,SliderFeature("Volume",d.minValue,d.maxValue))
  )
} //TODO: DONE
case class WashingMachinePane(override val d: SimulatedWashingMachine) extends GUIDevice(d){
  require (d.deviceType == WashingMachineType)
  contents++= Seq(
  new Label("Working mode: "),
  Feature(d.getWashingType toString,ListFeature("Mode",Seq(WashingType.RAPID,WashingType.MIX,WashingType.WOOL)map(_ toString))),
  new Label("Extras: "),
  Feature("Extra",ListFeature("Extras",Seq(WashingMachineExtra.SpecialColors,WashingMachineExtra.SuperDirty,WashingMachineExtra.SuperDry)map(_ toString))),
  new Label("RPM: "),
  Feature(d.getRPM toString,ListFeature("RPM",Seq(RPM.FAST,RPM.MEDIUM,RPM.SLOW)map(_ toString)))
  )
} //TODO: DONE


case class SliderFeature(text:String, mini: Int, maxi: Int) extends Dialog {
  title = text
  private var changed = false
  private val slider = new Slider(){
    this.min = mini
    this.max = maxi
  }
  private val value : Label = new Label("Value")

  contents = new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(new FlowPanel() {
        contents ++= Seq(
          new Label("Set "+text+": "),
          slider,
          value
        )
      },
        new FlowPanel() {
          contents ++= Seq(
            new Button("Confirm") {
              reactions += {
                case ButtonClicked(_) => if (changed) {
                  println(slider.value)
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
    case ValueChanged(_) => changed = true; value.text = slider.value.toString
  }
  listenTo(slider)
}
case class ListFeature(text:String,values: Seq[String]) extends Dialog{
  title = text
  private val lw = new ComboBox(values)
  contents =
    new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(new FlowPanel() {
        contents ++= Seq(
          new Label("Set "+text+": "),
          lw,
        )
      },
        new FlowPanel() {
          contents ++= Seq(
            new Button("Confirm") {
              reactions += {
                case ButtonClicked(_) =>
                  println(lw.selection.item)
                  //TODO: Update val
                  close()
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

}


