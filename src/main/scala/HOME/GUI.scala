package HOME

import scala.language.postfixOps
import java.awt.Color

import scala.swing._
import scala.swing.event.{ButtonClicked, MouseClicked, SelectionChanged, ValueChanged}
import javax.swing.{Box, ImageIcon}
import javax.swing.border.LineBorder
import HOME.MyClass._

import scala.util.Success
import scala.concurrent._
import ExecutionContext.Implicits.global
sealed trait Room {
  def devices : Set[Device]
  def name : String
}

sealed trait EditableFeature{
  def update(devName : String,cmdMsg :String,newValue:String): Future[Unit] = {
    val p = Promise[Unit]
    Coordinator.sendUpdate(devName, cmdMsg, newValue).onComplete {
      case Success(_) => setVal(newValue); p.success(() => Unit)
      case _ => Dialog.showMessage(title ="Update Error",message = "Something wrong happened while trying to update a device",messageType = Dialog.Message.Error); p.failure(_)
    }
    p.future
  }

  def getVal : String
  def setVal(v:String) : Unit
}

class GUIRoom(override val name:String, override val devices:Set[Device] = Set.empty) extends ScrollPane with Room {
  val devicePanel = new BoxPanel(Orientation.Vertical)
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
      for (i <- devices) addDevicePane(PrintDevicePane(i))
  }

  def apply(roomName: String,devices:Set[Device]): GUIRoom = new GUIRoom(roomName,devices)
  def addDevicePane(dev : Component): Unit ={
    devicePanel.peer.add(Box.createVerticalStrut(Constants.GUIDeviceGAP))
    devicePanel.contents += dev
  }


}
object GUI extends MainFrame {
  //StartingDemo()
  var rooms: Set[GUIRoom] = Set.empty//Set(new GUIRoom("Home"), new GUIRoom("Kitchen"), new GUIRoom("Bedroom"))
  for(i <- Rooms.allRooms) {
    rooms += new GUIRoom(i, Coordinator.devices.filter(_.room equals i))
  }

  var requests : List[Int]  = List()
  protected val tp: TabbedPane = new TabbedPane {
    //Initializing basic rooms
    pages+= new TabbedPane.Page("Home", new GUIRoom("Home"))
    for(i <- rooms) pages += new TabbedPane.Page(i.name,i)
    pages+= new TabbedPane.Page(Constants.AddPane,new BorderPanel())
  }
  def setRooms(roomList : Set[String]): Unit = roomList foreach( room=> rooms+=new GUIRoom(room,Coordinator.devices.filter(_.room equals room)))
  def top: MainFrame = new MainFrame {
    title = "Home!"
    println("Welcome")
    reactions += {
      case SelectionChanged(_) =>
        for {
          last <- getLastIndex()
          name <- getRoomName
        } yield {
          val newRoom = new GUIRoom(name,Constants.devicesPerRoom(name))
          val newRoomPane = new TabbedPane.Page(name, newRoom)
          Rooms.addRoom(name)
          RegisterDevice(Constants.devicesPerRoom(name).map(_.asInstanceOf[AssociableDevice]))
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
          case Constants.AddPane => tp.pages.find(page => page.title equals Constants.AddPane)
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
      if (name.isDefined && name.get.trim.length > 0 && !name.get.equals(Constants.AddPane) && !tp.pages.exists(page => page.title equals name.get)) {
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
  def getCurrentRoom: String = {
    tp.selection.page.title
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
              val currentRoom = GUI.getCurrentRoom
              val devType = deviceType.selection.item.toString
              if(name.trim.length > 0){
                for {
                  room <- GUI.rooms.find(_.name equals currentRoom)
                  dev <- Coordinator.addDevice(devType,name,currentRoom)
                }yield{
                  room.addDevicePane(PrintDevicePane(dev))
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
  private val profiles = new ComboBox[String](Profile.getProfileNames toSeq)
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
            case ButtonClicked(_) => changeProfile()
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

  def changeProfile() : Unit = {
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
  var onActivationCommands: Set[(Device, CommandMsg)] = Set.empty
  //var onSensorChangeCommands: Set[Set[(Device, CommandMsg)]] = Set.empty
  var sensorRules: List[(String, Double, Device)] = List.empty
  var onSensorChange: List[(List[(String, Double, Device)], Set[(Device, CommandMsg)])] = List.empty
  //private val programmedStuffCommands: Set[(Device, CommandMsg)] = Set.empty
  //private val allRooms = new ComboBox[String](Rooms.allRooms toSeq)
  //private val allDevice = new ComboBox[DeviceType](DeviceType.listTypes toSeq)
  title = "New Profile"
  modal = true

  private val newProfileDialog = this

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
      contents += new Button("Add rules") {
        reactions += {
          case ButtonClicked(_) => AllDevice(Rooms.allRooms, newProfileDialog, null)
        }
      }
    }
    contents += new FlowPanel() {
      contents += new Label("On sensor changed: ")
      contents += new Button("Add rules") {
        reactions += {
          case ButtonClicked(_) =>
            SensorReaction(newProfileDialog)
        }
      }
    }
    /* contents += new FlowPanel() {
       contents += new Label("Programmed Stuff: ")
       contents += new Button("Devices") {
         reactions += {
           case ButtonClicked(_) => AllDevice(Rooms.allRooms, isRoutine = true, programmedStuffCommands)
         }
       }
     }*/
    contents += new FlowPanel() {
      contents += new Button("Confirm") {
        reactions += {
          case ButtonClicked(_) =>
            //println(profileName.text)
            //println(description.text)
            println(onActivationCommands)
            println(sensorRules)
            //println(onSensorChangeCommands)
            val generatedOnActivationCommand: Set[Device => Unit] = CustomProfileBuilder.generateCommandSet(onActivationCommands)
            var generatedSensorCommandsMap: Map[Double => Boolean, Set[Device => Unit]] = Map.empty
            for(rules <- sensorRules) {
              val rul = CustomProfileBuilder.generateCheckFunction(rules._1, rules._2)
              for(command <- onSensorChange.filter(_._1.equals(List(rules)))) {
                val commandSet = CustomProfileBuilder.generateCommandSet(command._2)
                generatedSensorCommandsMap ++= CustomProfileBuilder.generateSensorCommandsMap((rul, commandSet))
              }
            }
            val newProfile = CustomProfileBuilder.generateFromParams(profileName.text, description.text, generatedOnActivationCommand, generatedSensorCommandsMap, DummyUtils.dummyMap,
              DummyUtils.dummyMap, DummyUtils.dummySet, DummyUtils.dummySet, {})
            Profile.addProfile(newProfile)
            close()
        }
      }
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

class SensorReactionDialog(dialog: CreateProfileDialog) extends Dialog {
  modal = true
  title = "Sensor Reaction"
  location = new Point(300,0)
  contents = new ScrollPane() {
    contents = applyTemplate
  }

  var key: List[(String, Double, Device)] = List.empty
  val emptySet: Set[(Device, CommandMsg)] = Set.empty

  def applyTemplate : BoxPanel = {
    val panel = new BoxPanel(Orientation.Vertical)
    for(i <- Coordinator.getDevices) {
      val devicePanel = new BoxPanel(Orientation.Horizontal)
      devicePanel.peer.add(Box.createVerticalStrut(10))
      devicePanel.border = new LineBorder(Color.BLACK, 2)
      if(Device.isSensor(i)) {
        val value = new TextField(10)
        devicePanel.contents += new FlowPanel() {
          contents += new Label(i.name)
          contents += new Label("On: ")
          contents += applyComponent(i, this)
          contents += value
          contents += new Button("Do") {
            reactions += {
              case ButtonClicked(_) =>
                for(sym <- devicePanel.contents(1).asInstanceOf[FlowPanel].contents) yield {
                  if(sym.isInstanceOf[ComboBox[_]]) {
                    //println(giveSymbol(sym))
                    //println(value.text)
                    key = List((giveSymbol(sym), value.text.toDouble, i))
                    dialog.sensorRules ++= key
                    //dialog.sensorRules ++= Set((giveSymbol(sym), value.text.toDouble))
                    println(dialog.sensorRules)
                    //println(dialog.onSensorChange)
                  }
                }
                roomsDevices(i.room)
            }
          }
        }
        panel.contents += devicePanel
      }
    }
    panel
  }

  def applyComponent(dev: Device, panel: FlowPanel) : Component = dev.deviceType match {
    case MotionSensorType => panel.contents+=new Label("Motion ")
      new ComboBox[String](Set("Detecting", "Not detecting") toSeq)
    case HygrometerType =>  panel.contents+=new Label("Humidity ")
      new ComboBox[String](Set("=", ">=", "<=", ">", "<") toSeq)
    case PhotometerType => panel.contents+=new Label("Intensity ")
      new ComboBox[String](Set("=", ">=", "<=", ">", "<") toSeq)
    case ThermometerType => panel.contents+=new Label("Temperature ")
      new ComboBox[String](Set("=", ">=", "<=", ">", "<") toSeq)
    case _ => this.errUnexpected(UnexpectedDeviceType, dev.deviceType.toString)
  }

  def giveSymbol(x: Any): String = x match {
    case p: ComboBox[String] => p.selection.item
    case _ => ""
  }

  def roomsDevices(room: String) : Dialog = {
    AllDevice(Set(room), dialog, key)
  }
  open()
}
object SensorReaction {
  def apply(dialog: CreateProfileDialog): SensorReactionDialog = {
    new SensorReactionDialog(dialog)
  }
}

/*class AddProgrammedStuffDialog extends Dialog {
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
}*/

class AllDeviceDialog(rooms: Set[String], dialog: CreateProfileDialog, sensorRule: List[(String, Double, Device)]) extends Dialog {
  modal = true
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
      if(!Device.isSensor(i) && rooms.contains(i.room)) {
        val applyButton = new Button("Add")
        devPanel.peer.add(Box.createVerticalStrut(10))
        devPanel.border = new LineBorder(Color.BLACK, 2)
        devPanel.contents += new FlowPanel() {
          contents += new Label(i.name)
          MapDeviceCommands.apply(i)
          for(a <- MapDeviceCommands.getCommands) {
            val component = applyComponent(a,i,this)
            applyButton.reactions += {
              case ButtonClicked(_) => addRule(component, i, a)
            }
          }
          /*if(isRoutine) {
            contents += new Label("Start at: ")
            contents += new TextField(8)
            contents += new Label("End at: ")
            contents += new TextField(8)
          }*/
          contents += applyButton
        }
        panel.contents += devPanel
      }
    }

    panel.contents += new FlowPanel() {
      contents += new Button("Confirm") {
        reactions += {
          case ButtonClicked(_) => close()
        }
      }
    }
    panel
  }

  def applyComponent(command: String, device: Device, panel: FlowPanel): Component = command match {
    case Msg.washingType => device.deviceType match {
      case WashingMachineType =>
        val component = new ComboBox[String](Seq(WashingType.MIX, WashingType.RAPID, WashingType.WOOL)map(_.toString))
        panel.contents ++= Seq(new Label("Washing type: "), component)
        component
      case _ => null
    }
    case Msg.setProgram => device.deviceType match {
      case DishWasherType =>
        val component = new ComboBox[String](Seq(DishWasherProgram.DIRTY, DishWasherProgram.FAST, DishWasherProgram.FRAGILE)map(_.toString))
        panel.contents ++= Seq(new Label("Program type: "), component)
        component
      case _ => null
    }
    case Msg.RPM => device.deviceType match {
      case WashingMachineType | DishWasherType =>
        val component = new ComboBox[String](Seq(RPM.SLOW, RPM.MEDIUM, RPM.FAST)map(_.toString))
        panel.contents ++= Seq(new Label("RPM: "), component)
        component
      case _ => null
    }
    case Msg.addExtra => device.deviceType match {
      case WashingMachineType =>
        val component = new ComboBox[String](Seq(WashingMachineExtra.SpecialColors, WashingMachineExtra.SuperDirty, WashingMachineExtra.SuperDry)map(_.toString))
        panel.contents ++= Seq(new Label("Extras: "), component)
        component
      case DishWasherType =>
        val component = new ComboBox[String](Seq(DishWasherExtra.SuperDirty, DishWasherExtra.SuperHygiene, DishWasherExtra.SuperSteam)map(_.toString))
        panel.contents ++= Seq(new Label("Extras: "), component)
        component
      case _ => null
    }
    case Msg.setMode => device.deviceType match {
      case OvenType =>
        val component = new ComboBox[String](Seq(OvenMode.CONVENTIONAL, OvenMode.DEFROSTING, OvenMode.GRILL, OvenMode.LOWER,
          OvenMode.UPPER, OvenMode.VENTILATED)map(_.toString))
        panel.contents ++= Seq(new Label("Working mode: "), component)
        component
      case _ => null
    }
    case Msg.close | Msg.mute | Msg.off =>
      val component = new ToggleButton(command) {
        reactions += {
          case ButtonClicked(_) => this.text=switchStatus(this.text)
        }
      }
      panel.contents ++= Seq(component)
      component
    case _ =>
      val component = new TextField(10)
      panel.contents ++= Seq(new Label(command), component)
      component
  }

  def switchStatus(status: String) : String = status match {
    case "on" => "off"
    case "off" => "on"
    case "close" => "open"
    case "open" => "close"
    case "mute" => "muted"
    case _ => "mute"
  }

  def addRule(component: Component, device: Device, command: String) : Unit = command match {
    case Msg.on | Msg.off | Msg.open | Msg.close | Msg.mute => sensorRule match {
      case null => dialog.onActivationCommands ++= Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))
      case _ => dialog.onSensorChange ++= List((sensorRule, Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))))
        //List((List((giveSymbol(sym), value.text.toDouble, i)), emptySet))
        //List(sensorRule -> Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command)))))
        println(dialog.onSensorChange)
      //dialog.onSensorChangeCommands += Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))
    }
    case _ => sensorRule match {
      case null => dialog.onActivationCommands ++= Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))
      case _ => dialog.onSensorChange ++= List((sensorRule, Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))))
        println(dialog.onSensorChange)
      // dialog.onSensorChangeCommands += Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))
    }
  }

  def getComponentInfo(x: Any, command: String): String = x match {
    case p: TextField =>  command match {
      case Msg.setIntensity | Msg.setTemperature | Msg.setHumidity | Msg.setVolume => p.text
      case _ => ""
    }
    case p: ToggleButton => command match {
      case Msg.on | Msg.off | Msg.close | Msg.open | Msg.mute => p.text
      case _ => ""
    }

    case p: ComboBox[String] => command match {
      case Msg.washingType | Msg.RPM | Msg.addExtra | Msg.setMode | Msg.setProgram => p.selection.item
      case _ => ""
    }
    case _ => ""
  }

  open()
}

object AllDevice {
  def apply(rooms: Set[String], dialog: CreateProfileDialog, sensorRule: List[(String, Double, Device)]): AllDeviceDialog = {
    new AllDeviceDialog(rooms, dialog, sensorRule)
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
  val ON = "ON"
  //OFF is lazy because of Shutter type, allowing to overwrite its value before using it as button text
  lazy val OFF = "OFF"
  private var status = OFF
  private val devType = new Label("DeviceType: "+d.deviceType)
  private val statusButton = new ToggleButton(status) {
    reactions += {
      case ButtonClicked(_) =>
        this.text = switchStatus { case ON => Coordinator.sendUpdate(d.name,Msg.off); status = OFF case _ => Coordinator.sendUpdate(d.name,Msg.on);status = ON }
    }
  }
  /** BASIC TEMPLATE */
  val switchStatus: (String => Unit) => String = (c: String => Unit) => {
    c(status)
    status
  }

  border = new LineBorder(Color.BLACK, 2)
  contents += new myIcon(d.id, d.deviceType.toString)
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
    text=name
    border = new LineBorder(Color.black,1)
    icon = new ImageIcon(getClass.getClassLoader.getResource(iconPath + Constants.IconExt) getPath)

    horizontalTextPosition = Alignment.Center
    verticalTextPosition = Alignment.Bottom
  }
}
class DeviceFeature[A <: Component with EditableFeature](deviceName :String,featureTitle : String, initialValue: String, setterComponent: A ,updateType:String) extends Label {
  text = initialValue
  border = new LineBorder(Color.black,1)
  reactions+={
    case MouseClicked(_,_,_,_,_) => new Dialog(){
      title = featureTitle
      private val value : Label = new Label("Value")

      contents = new BoxPanel(Orientation.Vertical) {
        contents ++= Seq(
          new FlowPanel() {
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
                  case ButtonClicked(_) => setterComponent.update(deviceName,updateType,setterComponent.getVal).onComplete{
                    case Success(_) => setFeatureValue(setterComponent.getVal); close()
                    case _ => //Do nothing
                  }
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
        case ValueChanged(_) => value.text = setterComponent.getVal;
      }
      listenTo(setterComponent)
      open()
    }
  }
  listenTo(mouse.clicks)
  this.visible = true
  private def setFeatureValue(c :String): Unit = text = c
}
object Feature{
  def apply[A<: Component with EditableFeature](devName:String,title:String,text:String,setterComponent:A,updateType:String): DeviceFeature[A] = new DeviceFeature(devName,title,text,setterComponent,updateType)
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

    case HygrometerType => ThermometerPane(Thermometer(device.name,device.room))  //TODO CHANGE
    case MotionSensorType => ThermometerPane(Thermometer(device.name,device.room))  //TODO CHANGE
    case PhotometerType => ThermometerPane(Thermometer(device.name,device.room))
    case _ => this.errUnexpected(UnexpectedDeviceType, device.deviceType.toString)
  }
}
private case class AirConditionerPane(override val d: SimulatedAirConditioner) extends GUIDevice(d){
  require (d.deviceType == AirConditionerType)
  contents++=Seq(new Label("Temperature: "),
    Feature(d.name,"Temperature",d.value toString,SliderFeature(d.minValue,d.maxValue),Msg.setTemperature))
}
private case class DehumidifierPane(override val d: SimulatedDehumidifier) extends GUIDevice(d){
  require (d.deviceType == DehumidifierType)
  contents++=Seq(new Label("Humidity %: "),
    Feature(d.name,"Humidity",d.value toString,SliderFeature(d.minValue,d.maxValue),Msg.setHumidity))
}
private case class DishWasherPane(override val d: SimulatedDishWasher) extends GUIDevice(d){
  require (d.deviceType == DishWasherType)
  contents++= Seq(
    new Label("Washing program: "),
    Feature(d.name,"Washing program",d.getWashingProgram toString,ListFeature(Seq(DishWasherProgram.DIRTY,DishWasherProgram.FAST,DishWasherProgram.FRAGILE)map(_ toString)),Msg.setProgram),
    new Label("Extras: "),
    Feature(d.name,"Extra","Extra",ListFeature(Seq(DishWasherExtra.SuperDirty,DishWasherExtra.SuperHygiene,DishWasherExtra.SuperSteam)map(_ toString)),Msg.addExtra),
  )
}
private case class LightPane(override val d: SimulatedLight) extends GUIDevice(d) {
  require(d.deviceType == LightType)
  contents++=Seq(new Label("Intensity: "),
    Feature(d.name,"Intensity",d.value toString,SliderFeature(d.minValue,d.maxValue),Msg.setIntensity))
}
private case class OvenPane(override val d: SimulatedOven) extends GUIDevice(d){
  require (d.deviceType == OvenType)
  contents++=Seq(
    new Label("Oven temperature: "),
    Feature(d.name,"Oven temperature",d.value toString, SliderFeature(d.minValue,d.maxValue),Msg.setTemperature),
    new Label("Oven Mode: "),
    Feature(d.name,"Oven mode",d.getOvenMode toString, ListFeature(Seq(OvenMode.CONVENTIONAL,OvenMode.DEFROSTING,OvenMode.GRILL,OvenMode.LOWER,
      OvenMode.UPPER,OvenMode.VENTILATED)map(_ toString)),Msg.setMode)
  )
}
private case class ShutterPane(override val d: SimulatedShutter) extends GUIDevice(d){
  require (d.deviceType == ShutterType)
  override val ON = "OPEN"
  override lazy val OFF = "CLOSED"
}
private case class StereoPane(override val d: SimulatedStereoSystem) extends GUIDevice(d){
  contents++=Seq(
    new Label("Volume: "),
    Feature(d.name,"Volume",d.value toString,SliderFeature(d.minValue,d.maxValue),Msg.setVolume)
  )
}
private case class ThermometerPane(override val d: SimulatedThermometer) extends GUIDevice(d){
  require (d.deviceType == ThermometerType)
}
private case class TVPane(override val d: SimulatedTV) extends GUIDevice(d){
  require (d.deviceType == TvType)
  contents++=Seq(
    new Label("Volume: "),
    Feature(d.name,"Volume",d.value toString,SliderFeature(d.minValue,d.maxValue),Msg.setVolume)
  )
}
case class WashingMachinePane(override val d: SimulatedWashingMachine) extends GUIDevice(d){
  require (d.deviceType == WashingMachineType)
  contents++= Seq(
    new Label("Working mode: "),
    Feature(d.name,"Working mode",d.getWashingType toString,ListFeature(Seq(WashingType.RAPID,WashingType.MIX,WashingType.WOOL)map(_ toString)),Msg.washingType),
    new Label("Extras: "),
    Feature(d.name,"Extras","Extra",ListFeature(Seq(WashingMachineExtra.SpecialColors,WashingMachineExtra.SuperDirty,WashingMachineExtra.SuperDry)map(_ toString)),Msg.addExtra),
    new Label("RPM: "),
    Feature(d.name,"RMP",d.getRPM toString,ListFeature(Seq(RPM.FAST,RPM.MEDIUM,RPM.SLOW)map(_ toString)),Msg.RPM)
  )
}


case class SliderFeature(mini : Int, maxi: Int) extends Slider with EditableFeature {
  min = mini
  max = maxi
  override def getVal :String = value toString
  override def setVal(v:String): Unit = value = v toInt
}
case class ListFeature(items: Seq[String]) extends ComboBox(items) with EditableFeature {
  override def getVal : String = selection.item
  override def setVal(v: String): Unit = selection.item = v
}


object LoginPage{
  val id : TextField = new TextField(Constants.LoginTextSize)
  val psw : PasswordField = new PasswordField(Constants.LoginTextSize)

  new Frame(){
    title = "Login to HOME!"
    contents = new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(
        new FlowPanel() {
          contents ++= Seq(
            new Label("Username:"),
            id,
          )
        },
        new FlowPanel() {
          contents ++= Seq(
            new Label("Password:"),
            psw,
          )},
        new FlowPanel() {
          contents ++= Seq(
            new Button("Confirm"),
            new Button("Cancel") {
              reactions += {
                case ButtonClicked(_) => close()
              }
            })
        }
      )
    }
    this.open()
  }

}