package HOME

import java.awt.Color

import HOME.MyClass._
import javax.swing.border.{LineBorder, TitledBorder}
import javax.swing.{Box, ImageIcon}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.language.postfixOps
import scala.swing.Dialog.Result
import scala.swing._
import scala.swing.event.{ButtonClicked, MouseClicked, SelectionChanged, ValueChanged}
import scala.util.Success

sealed trait Room {
  var devices : Set[Device]
  def name : String
}

sealed trait EditableFeature{
  def update(devName : String,cmdMsg :String,newValue:String): Future[Unit] = {
    val p = Promise[Unit]
    Coordinator.sendUpdate(devName, cmdMsg, newValue).onComplete {
      case Success(_) => setVal(newValue); p.success(() => Unit);
      case _ => Dialog.showMessage(title ="Update Error",message = "Something wrong happened while trying to update a device",messageType = Dialog.Message.Error);  p.failure(_)
    }
    p.future
  }
  def getVal : String
  def setVal(v:String) : Unit
}

class GUIRoom(override val name:String, override var devices:Set[Device] = Set.empty) extends ScrollPane with Room {
  var gui_devices : Set[GUIDevice] = devices.map(PrintDevicePane(_))
  val devicePanel = new BoxPanel(Orientation.Vertical)
  //devices.map(_.asInstanceOf[AssociableDevice])
  val adDeviceBtn: Button =
    new Button("Add device") {
      reactions += {
        case ButtonClicked(_) => DeviceDialog()
      }
    }

  name toLowerCase match {
    case "home" => contents = HomePage()
    case _ => val bp: BorderPanel = new BorderPanel {
      add(new FlowPanel() {
          border = new TitledBorder("Sensors")
          contents ++= devices.filter(Device.isSensor).map(PrintDevicePane(_))
        },BorderPanel.Position.North)

      add(devicePanel, BorderPanel.Position.Center)
      add(adDeviceBtn, BorderPanel.Position.South)
    }
      contents = bp
      for (i <- devices.filter(!Device.isSensor(_))) addDevicePane(PrintDevicePane(i))
  }

  def apply(roomName: String,devices:Set[Device]): GUIRoom = new GUIRoom(roomName,devices)
  private def addDevicePane(dev : GUIDevice): Unit ={
    devicePanel.peer.add(Box.createVerticalStrut(Constants.GUIDeviceGAP))
    devicePanel.contents += dev
  }
  def addDevice(dev:GUIDevice): Unit ={
    addDevicePane(dev)
    gui_devices+=dev
  }

}
object GUI extends MainFrame {
  //StartingDemo()
  var rooms: Set[GUIRoom] = Set.empty//Set(new GUIRoom("Home"), new GUIRoom("Kitchen"), new GUIRoom("Bedroom"))
  for(i <- Rooms.allRooms) {
    rooms += new GUIRoom(i, Coordinator.getDevices.filter(_.room equals i))
  }

  var requests : List[Int]  = List()
  protected val tp: TabbedPane = new TabbedPane {
    //Initializing basic rooms
    pages+= new TabbedPane.Page("Home", new GUIRoom("Home"))
    for(i <- rooms) pages += new TabbedPane.Page(i.name,i)
    pages+= new TabbedPane.Page(Constants.AddPane,new BorderPanel())
  }
  //def setRooms(roomList : Set[String]): Unit = roomList foreach( room=> rooms+=new GUIRoom(room,Coordinator.devices.filter(_.room equals room)))
  def top: MainFrame = new MainFrame {
    title = "Home!"
    println("Welcome")
    reactions += {
      case SelectionChanged(_) =>
        for {
          last <- getLastIndex()
          name <- getRoomName
        } yield {
          val devices = Constants.devicesPerRoom(name)
          val newRoom = new GUIRoom(name,devices)
          RegisterDevice(devices.map(_.asInstanceOf[AssociableDevice]))
          val newRoomPane = new TabbedPane.Page(name, newRoom)
          Rooms.addRoom(name)
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
  def removeDevice(device:Device) : Unit = {
    rooms.foreach(_.devices -= device)
  }
  def updateDevice(d: Device,cmdMsg:String,newVal:String):Unit ={
    rooms.find(_.devices.contains(d)).get.gui_devices.find(_.name == d.name).get.updateDevice(d,cmdMsg,newVal)
  }
  override def closeOperation(): Unit = {
    super.closeOperation()
    Application.closeApplication()
  }
}

class AddDeviceDialog extends Dialog {
  private val dimension = WindowSize(WindowSizeType.Dialog)
  //Can't add sensors
  private val deviceType = new ComboBox[DeviceType](DeviceType.listTypes -- Seq(MotionSensorType,ThermometerType,HygrometerType,PhotometerType) toSeq)

  preferredSize = dimension
  title = "Add device"
  contents = new BoxPanel(Orientation.Vertical) {
    private val labels = new FlowPanel() {
      contents ++= Seq(new Label("Device name: "),new Label("Device type: "),deviceType)
    }
    private val buttons = new FlowPanel() {
      contents ++= Seq(
        new Button("Create"){
          reactions += {
            case ButtonClicked(_) =>
              val room = GUI.getCurrentRoom
              val dev = Device(deviceType.selection.item.toString,DeviceIDGenerator(),room).get.asInstanceOf[AssociableDevice]
              RegisterDevice(dev).onComplete {
                    case Success(_) => GUI.rooms.find(_.name equals room).get.addDevice(PrintDevicePane(dev));repaint(); close()
                    case _ => Dialog.showMessage(message = "Couldn't add device, try again", messageType = Dialog.Message.Error)
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
  modal = true
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
            case ButtonClicked(_) => changeProfile(delete)
          }
        }
      case "Delete profile" =>
        this.title = "Delete Profile"
        new Button("Delete") {
          reactions += {
            case ButtonClicked(_) => changeProfile(delete)
          }
        }
    }
  }

  def changeProfile(name: String): Unit = {
    val selectedProfile = profiles.selection.item
    name match {
      case "Change profile" =>
        labelProfile.text = "Current active profile: " + selectedProfile
        Coordinator.setProfile(Profile(selectedProfile))
      case _ => Profile.removeProfile(selectedProfile)
    }
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
  var sensorRules: List[(String, Double, String, Device)] = List.empty
  var thermometerNotificationCommands: List[(List[(String, Double, String, Device)], Set[(Device, CommandMsg)])] = List.empty
  var hygrometerNotificationCommands: List[(List[(String, Double, String, Device)], Set[(Device, CommandMsg)])] = List.empty
  var photometerNotificationCommands: List[(List[(String, Double, String, Device)], Set[(Device, CommandMsg)])] = List.empty
  var motionSensorNotificationCommands: List[(List[(String, String, Device)], Set[(Device, CommandMsg)])] = List.empty
  //private val programmedStuffCommands: Set[(Device, CommandMsg)] = Set.empty
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
            val generatedOnActivationCommand: Set[Device => Unit] = CustomProfileBuilder.generateCommandSet(onActivationCommands)
            var generatedThermometerSensorCommandsMap: Map[(String, Double) => Boolean, Set[Device => Unit]] = Map.empty
            val generatedHygrometerSensorCommandsMap: Map[(String, Double) => Boolean, Set[Device => Unit]] = Map.empty
            val generatedPhotometerSensorCommandsMap: Map[(String, Double) => Boolean, Set[Device => Unit]] = Map.empty
            var generatedMotionSensorCommands: Map[String, Set[Device => Unit]] = Map.empty
            for(rules <- sensorRules) {
              rules._4.deviceType match {
                case MotionSensorType =>
                  for(command <- motionSensorNotificationCommands.filter(_._1.equals(List(rules._1, rules._3, rules._4)))) {
                    val commandSet = CustomProfileBuilder.generateCommandSet(command._2)
                    generatedMotionSensorCommands ++= CustomProfileBuilder.generateMotionSensorCommandsMap((rules._3, commandSet))
                  }
                case _ =>
                  val rul = CustomProfileBuilder.generateCheckFunction(rules._1, rules._2, rules._3)
                  for(command <- thermometerNotificationCommands.filter(_._1.equals(List(rules)))) {
                    val commandSet = CustomProfileBuilder.generateCommandSet(command._2)
                    generatedThermometerSensorCommandsMap ++= CustomProfileBuilder.generateSensorCommandsMap((rul, commandSet))
                  }
                  for(command <- hygrometerNotificationCommands.filter(_._1.equals(List(rules)))) {
                    val commandSet = CustomProfileBuilder.generateCommandSet(command._2)
                    generatedThermometerSensorCommandsMap ++= CustomProfileBuilder.generateSensorCommandsMap((rul, commandSet))
                  }
                  for(command <- photometerNotificationCommands.filter(_._1.equals(List(rules)))) {
                    val commandSet = CustomProfileBuilder.generateCommandSet(command._2)
                    generatedThermometerSensorCommandsMap ++= CustomProfileBuilder.generateSensorCommandsMap((rul, commandSet))
                  }
              }
            }
            val newProfile = CustomProfileBuilder.generateFromParams(profileName.text, description.text, generatedOnActivationCommand, generatedThermometerSensorCommandsMap,
              generatedHygrometerSensorCommandsMap, generatedPhotometerSensorCommandsMap, generatedMotionSensorCommands, DummyUtils.dummySet, {})
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
  preferredSize = new Dimension(900, 400)
  contents = new ScrollPane() {
    contents = applyTemplate
  }

  var key: List[(String, Double, String, Device)] = List.empty
  val emptySet: Set[(Device, CommandMsg)] = Set.empty

  def applyTemplate : BoxPanel = {
    val panel = new BoxPanel(Orientation.Vertical)
    for(i <- Coordinator.getDevices) {
      val devicePanel = new BoxPanel(Orientation.Horizontal)
      devicePanel.peer.add(Box.createVerticalStrut(10))
      devicePanel.border = new LineBorder(Color.BLACK, 2)
      if(Device.isSensor(i)) {
        val comboRooms: ComboBox[String] = new ComboBox[String](Rooms.allRooms toSeq)
        val value = new TextField(10)
        devicePanel.contents += new FlowPanel() {
          contents += new Label(i.name + ": ")
          contents += applyComponent(i, this)
          if(i.deviceType != MotionSensorType) {
            contents += value
          }
          contents += new Label("Select rooms where yuo want to apply rules")
          contents += comboRooms
          contents += new Button("Do") {
            reactions += {
              case ButtonClicked(_) =>
                for(sym <- devicePanel.contents(1).asInstanceOf[FlowPanel].contents) yield {
                  sym match {
                    case x: ComboBox[_] if !x.equals(comboRooms) =>
                      i.deviceType match {
                        case MotionSensorType => key = List((giveSymbol(sym), Double.NaN, comboRooms.selection.item, i))
                        case _ => key = List((giveSymbol(sym), value.text.toDouble, comboRooms.selection.item, i))
                      }
                      dialog.sensorRules ++= key
                      //dialog.sensorRules ++= Set((giveSymbol(sym), value.text.toDouble))
                      println(dialog.sensorRules)
                    case _ =>
                  }
                }
                roomsDevices(comboRooms.selection.item)
            }
          }
        }
        panel.contents += devicePanel
      }
    }
    panel.contents += new Button("Confirm") {
      reactions += {
        case ButtonClicked(_) => close()
      }
    }
    panel
  }

  def applyComponent(dev: Device, panel: FlowPanel) : Component = dev.deviceType match {
    case MotionSensorType => panel.contents+=new Label("Motion ")
      new ComboBox[String](Set("Detecting") toSeq)
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

class AllDeviceDialog(rooms: Set[String], dialog: CreateProfileDialog, sensorRule: List[(String, Double, String, Device)]) extends Dialog {
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
      case _ => sensorRule.head._4.deviceType match {
        case ThermometerType => dialog.thermometerNotificationCommands ++= List((sensorRule, Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))))
          println(dialog.thermometerNotificationCommands)
        case PhotometerType => dialog.photometerNotificationCommands ++= List((sensorRule, Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))))
          println(dialog.photometerNotificationCommands)
        case HygrometerType => dialog.hygrometerNotificationCommands ++= List((sensorRule, Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))))
          println(dialog.hygrometerNotificationCommands)
        case MotionSensorType => dialog.motionSensorNotificationCommands ++= List((List((sensorRule.head._1, sensorRule.head._3, sensorRule.head._4)),
          Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))))
          println(dialog.motionSensorNotificationCommands)
        case _ =>
      }
    }
    case _ => sensorRule match {
      case null => dialog.onActivationCommands ++= Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))
      case _ => sensorRule.head._4.deviceType match {
        case ThermometerType => dialog.thermometerNotificationCommands ++= List((sensorRule, Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))))
        case PhotometerType => dialog.photometerNotificationCommands ++= List((sensorRule, Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))))
        case HygrometerType => dialog.hygrometerNotificationCommands ++= List((sensorRule, Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))))
        case MotionSensorType => dialog.motionSensorNotificationCommands ++= List((List((sensorRule.head._1, sensorRule.head._3, sensorRule.head._4)),
          Set((device, CommandMsg(Msg.nullCommandId, getComponentInfo(component, command), getComponentInfo(component, command))))))
        case _ =>
      }
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
  def apply(rooms: Set[String], dialog: CreateProfileDialog, sensorRule: List[(String, Double, String, Device)]): AllDeviceDialog = {
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
    //contents += new Label("External temperature: ")
  }
  val humidityPanel: FlowPanel = new FlowPanel() {
    hGap = 70
    contents += new Label("Time: " + DateTime.getCurrentTime)
    contents += new Label("Internal humidity: ")
    //contents += new Label("External humidity: ")
  }
  val alarmPanel: FlowPanel = new FlowPanel() {
    hGap = 70
    contents += new Label("Alarm status")
    contents += new ToggleButton()
  }
  val currentProfile = new Label("Current active profile: " + Coordinator.getActiveProfile.name)
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
  private val devType = new Label("DeviceType: "+d.deviceType)

  border = new LineBorder(Color.BLACK, 2)
  contents += new myIcon(d.deviceType.toString)
  private val deviceInfo = new GridPanel(2, 2)
  deviceInfo.contents ++= Seq(
    devType,
    new Label("Consumption: " + d.consumption),
    BinaryFeature(d.name,"ON",Msg.on,"OFF",Msg.off),
    new Button("Delete") {
      reactions +={
        case ButtonClicked(_) => Dialog.showConfirmation(message="Are you sure you want to delete this device? There is no coming back",title ="Delete device") match{
          case Result.Ok =>  Coordinator.sendUpdate(d.name,Msg.disconnect); GUI.removeDevice(d); shutdown()/*.onComplete{
            case Success(_) => println("OK"); GUI.removeDevice(d); this.visible = false
            case _ => println("Something wrong") //TODO: Error dialog
          }*/
          case _ => //Do nothing
        }
      }
    }
  )
  private def shutdown(): Unit ={
    this.visible = false
  }
  if (!Device.isSensor(d)) {
    contents += deviceInfo
  }else contents += new Label("Value:")

  /********/
  private class myIcon(iconName :String) extends Label{
    text=iconName
    border = new LineBorder(Color.black,1)
    icon = new ImageIcon(getClass.getClassLoader.getResource(iconName + Constants.IconExt) getPath)

    horizontalTextPosition = Alignment.Center
    verticalTextPosition = Alignment.Bottom
  }
  def updateDevice(dev: Device, cmdString: String,newVal:String): Unit = {/**Sensor don't need this*/}
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
                    case _ => //TODO: PRINT ERROR DIALOG
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
  def setFeatureValue(c :String): Unit = text = c
}
object Feature{
  def apply[A<: Component with EditableFeature](devName:String,title:String,text:String,setterComponent:A,updateType:String): DeviceFeature[A] = new DeviceFeature(devName,title,text,setterComponent,updateType)
  /* This second apply is used for simulating sensors: they don't need no updateType since, for simulating purposes, updates are being applied directly
   * to them without going through Coordinator.  */
  def apply[A<: Component with EditableFeature](devName:String,title:String,text:String,setterComponent:A): DeviceFeature[A] = new DeviceFeature(devName,title,text,setterComponent,null)
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
    case TvType => TVPane(TV(device.name,device.room))
    case WashingMachineType => WashingMachinePane(WashingMachine(device.name,device.room))
    case BoilerType => BoilerPane(Boiler(device.name,device.room)) //TODO: CHANGE

      //Sensors
    case ThermometerType => ThermometerPane(device.asInstanceOf[SimulatedThermometer])
    case HygrometerType => HygrometerPane(device.asInstanceOf[SimulatedHygrometer])
    case MotionSensorType => MotionSensorPane(device.asInstanceOf[SimulatedMotionSensor])
    case PhotometerType => PhotometerPane(device.asInstanceOf[SimulatedPhotometer])
    case _ => this.errUnexpected(UnexpectedDeviceType, device.deviceType.toString)
  }
}

/* SENSOR PANE*/
private case class HygrometerPane(override val d: SimulatedHygrometer)extends GUIDevice(d){
  require (d.deviceType == HygrometerType)
  private val MAX = 100
  private val MIN = 0
  contents += Feature(d.name,"Humidity",30 toString,new SliderFeature(MIN,MAX){
    override def update(devName : String, cmdMsg :String, newValue:String): Future[Unit] ={
      d.valueChanged(newValue toDouble)
      Promise[Unit].success(() => Unit).future
    }
  })
}
private case class MotionSensorPane(override val d: SimulatedMotionSensor)extends GUIDevice(d){
  require (d.deviceType == MotionSensorType)
  private val EMPTY = "EMPTY"
  private val NOT_EMPTY = "NOT EMPTY"
  private var status = EMPTY
  contents += new BinaryFeature(d.name,"Empty",Msg.motionDetected,"NOT EMPTY",Msg.motionDetected){
    override def update(devName : String, cmdMsg :String, newValue:String): Future[Unit] ={
      status match {
        case EMPTY =>
          d.valueChanged(currentVal = true)
          Promise[Unit].success(() => Unit).future
          status = NOT_EMPTY;
        case _ =>
          d.valueChanged(currentVal = false)
          Promise[Unit].success(() => Unit).future
          status = EMPTY}
      text = status
      Promise[Unit].success(() => Unit).future
    }
  }
}
private case class PhotometerPane(override val d: SimulatedPhotometer)extends GUIDevice(d){
  require (d.deviceType == PhotometerType)
  private val MAX = 100
  private val MIN = 0
  contents += Feature(d.name,"Temperature",22 toString,new SliderFeature(MIN,MAX){
    override def update(devName : String, cmdMsg :String, newValue:String): Future[Unit] ={
      d.valueChanged(newValue toDouble)
      Promise[Unit].success(() => Unit).future
    }
  })
}
private case class ThermometerPane(override val d: SimulatedThermometer) extends GUIDevice(d){
  require (d.deviceType == ThermometerType)
  private val MAX = 50
  private val MIN = -20
  contents += Feature(d.name,"Temperature",22 toString,new SliderFeature(MIN,MAX){
     override def update(devName : String, cmdMsg :String, newValue:String): Future[Unit] ={
       d.valueChanged(newValue toDouble)
       Promise[Unit].success(() => Unit).future
    }
  }) //TODO: MAGIC NUMBERS
}
/* DEVICES PANE*/
private case class AirConditionerPane(override val d: SimulatedAirConditioner) extends GUIDevice(d){
  require (d.deviceType == AirConditionerType)
  private val temp = Feature(d.name,"Temperature",d.value toString,SliderFeature(d.minValue,d.maxValue),Msg.setTemperature)
  contents++=Seq(new Label("Temperature: "),
    temp)
  override def updateDevice(dev: Device, cmdString: String,newVal:String): Unit = cmdString match{
    case Msg.setTemperature => d.setValue(newVal toInt); temp.setFeatureValue(newVal)
    case _ =>this.errUnexpected(UnexpectedMessage,"This device can only receive temperature updates")
  }
}
private case class BoilerPane(override val d: SimulatedBoiler) extends GUIDevice(d){
  require (d.deviceType == BoilerType)
  private val waterTemp = Feature(d.name,"Water temperature",d.value toString,SliderFeature(d.minValue,d.maxValue),Msg.setHumidity)
  contents++=Seq(new Label("Water temperature: "),
    waterTemp)
  override def updateDevice(dev: Device, cmdString: String,newVal:String): Unit = cmdString match{
    case Msg.setTemperature => d.setValue(newVal toInt); waterTemp.setFeatureValue(newVal)
    case _ =>this.errUnexpected(UnexpectedMessage,"This device can only receive temperature updates")
  }
}
private case class DehumidifierPane(override val d: SimulatedDehumidifier) extends GUIDevice(d){
  require (d.deviceType == DehumidifierType)
  private val humidity = Feature(d.name,"Humidity",d.value toString,SliderFeature(d.minValue,d.maxValue),Msg.setHumidity)
  contents++=Seq(new Label("Humidity %: "),
    humidity)
  override def updateDevice(dev: Device, cmdString: String,newVal:String): Unit = cmdString match{
    case Msg.setHumidity => d.setValue(newVal toInt); humidity.setFeatureValue(newVal)
    case _ =>this.errUnexpected(UnexpectedMessage,"This device can only receive humidity updates")
  }
}
private case class DishWasherPane(override val d: SimulatedDishWasher) extends GUIDevice(d){
  require (d.deviceType == DishWasherType)
  private val washProgram = Feature(d.name,"Washing program",d.getWashingProgram toString,ListFeature(Seq(DishWasherProgram.DIRTY,DishWasherProgram.FAST,DishWasherProgram.FRAGILE)map(_ toString)),Msg.setProgram)
  private val extras = Feature(d.name,"Extra","Extra",ListFeature(Seq(DishWasherExtra.SuperDirty,DishWasherExtra.SuperHygiene,DishWasherExtra.SuperSteam)map(_ toString)),Msg.addExtra)
  contents++= Seq(
    new Label("Washing program: "),
    washProgram,
    new Label("Extras: "),
    extras
  )
  override def updateDevice(dev: Device, cmdString: String,newVal:String): Unit = cmdString match{
    case Msg.washingType => d.setWashingProgram(DishWasherProgram(newVal)); washProgram.setFeatureValue(newVal)
    case Msg.addExtra => d.addExtra(DishWasherExtra(newVal)); extras.setFeatureValue(newVal)
    case _ =>this.errUnexpected(UnexpectedMessage,"This device can only receive washing type or extra updates")
  }
}
private case class LightPane(override val d: SimulatedLight) extends GUIDevice(d) {
  require(d.deviceType == LightType)
  private val intensity = Feature(d.name,"Intensity",d.value toString,SliderFeature(d.minValue,d.maxValue),Msg.setIntensity)
  contents++=Seq(new Label("Intensity: "),
    intensity)
  override def updateDevice(dev: Device, cmdString: String,newVal:String): Unit = cmdString match{
    case Msg.setIntensity => d.setValue(newVal toInt); intensity.setFeatureValue(newVal)
    case _ =>this.errUnexpected(UnexpectedMessage,"This device can only receive intensity updates")
  }
}
private case class OvenPane(override val d: SimulatedOven) extends GUIDevice(d){
  require (d.deviceType == OvenType)
  private val ovenTemp = Feature(d.name,"Oven temperature",d.value toString, SliderFeature(d.minValue,d.maxValue),Msg.setTemperature)
  private val ovenMode =Feature(d.name,"Oven mode",d.getOvenMode toString, ListFeature(Seq(OvenMode.CONVENTIONAL,OvenMode.DEFROSTING,OvenMode.GRILL,OvenMode.LOWER,
    OvenMode.UPPER,OvenMode.VENTILATED)map(_ toString)),Msg.setMode)
  contents++=Seq(
    new Label("Oven temperature: "),
    ovenTemp,
    new Label("Oven Mode: "),
    ovenMode
  )
  override def updateDevice(dev: Device, cmdString: String,newVal:String): Unit = cmdString match{
    case Msg.setTemperature => d.setValue(newVal toInt); ovenTemp.setFeatureValue(newVal)
    case Msg.setMode => d.setOvenMode(OvenMode(newVal)); ovenMode.setFeatureValue(newVal)
    case _ =>this.errUnexpected(UnexpectedMessage,"This device can only receive temperature or mode updates")
  }
}
private case class ShutterPane(override val d: SimulatedShutter) extends GUIDevice(d){
  private val mode = BinaryFeature(d.name,"CLOSED",Msg.open,"OPEN",Msg.close)
  require (d.deviceType == ShutterType)
  contents+= mode
  override def updateDevice(dev: Device, cmdString: String,newVal:String): Unit = cmdString match{
    case Msg.open => d.open(); mode.setVal(newVal) //TODO: FIX SETVAL
    case Msg.close => d.close(); mode.setVal(newVal)
    case _ =>this.errUnexpected(UnexpectedMessage,"This device can only receive close or open updates")
  }
}
private case class StereoPane(override val d: SimulatedStereoSystem) extends GUIDevice(d){
  private val volume = Feature(d.name,"Volume",d.value toString,SliderFeature(d.minValue,d.maxValue),Msg.setVolume)
  private val muted = BinaryFeature(d.name,"MUTED",Msg.mute,"NOT MUTED",Msg.mute)
  contents++=Seq(
    new Label("Volume: "),
    volume,
    muted
  )
  override def updateDevice(dev: Device, cmdString: String,newVal:String): Unit = cmdString match{
    case Msg.setVolume => d.setValue(newVal toInt); volume.setFeatureValue(newVal) //TODO: FIX SETVAL
    case Msg.mute => d.setValue(d.minValue)
    case _ =>this.errUnexpected(UnexpectedMessage,"This device can only receive volume updates")
  }
}
private case class TVPane(override val d: SimulatedTV) extends GUIDevice(d){
  require (d.deviceType == TvType)
  private val volume =Feature(d.name,"Volume",d.value toString,SliderFeature(d.minValue,d.maxValue),Msg.setVolume)
    private val muted = BinaryFeature(d.name,"MUTED",Msg.mute,"NOT MUTED",Msg.mute)
  contents++=Seq(
    new Label("Volume: "),
    volume,
    muted
  )
  override def updateDevice(dev: Device, cmdString: String,newVal:String): Unit = cmdString match{
    case Msg.setVolume => d.setValue(newVal toInt); volume.setFeatureValue(newVal)
    case Msg.mute => d.setValue(d.minValue)//TODO: FIX SETVAL
    case _ =>this.errUnexpected(UnexpectedMessage,"This device can only receive volume updates")
  }
}
private case class WashingMachinePane(override val d: SimulatedWashingMachine) extends GUIDevice(d){
  private val workMode =Feature(d.name,"Working mode",d.getWashingType toString,ListFeature(Seq(WashingType.RAPID,WashingType.MIX,WashingType.WOOL)map(_ toString)),Msg.washingType)
  private val extras =Feature(d.name,"Extras","Extra",ListFeature(Seq(WashingMachineExtra.SpecialColors,WashingMachineExtra.SuperDirty,WashingMachineExtra.SuperDry)map(_ toString)),Msg.addExtra)
  private val rpm = Feature(d.name,"RMP",d.getRPM toString,ListFeature(Seq(RPM.FAST,RPM.MEDIUM,RPM.SLOW)map(_ toString)),Msg.RPM)

  require (d.deviceType == WashingMachineType)
  contents++= Seq(
    new Label("Working mode: "),
    workMode,
    new Label("Extras: "),
    extras,
    new Label("RPM: "),
    rpm
  )
  override def updateDevice(dev: Device, cmdString: String,newVal:String): Unit = cmdString match{
    case Msg.setMode => d.setWashingType(WashingType(newVal)); workMode.setFeatureValue(newVal)
    case Msg.addExtra => d.addExtra(WashingMachineExtra(newVal)); extras.setFeatureValue(newVal)
    case Msg.RPM => d.setRPM(RPM(newVal)); rpm.setFeatureValue(newVal)
    case _ =>this.errUnexpected(UnexpectedMessage,"This device can only receive close or open updates")
  }
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

case class BinaryFeature(devName:String,toDisplay:String,cmd1:String,other : String,cmd2:String) extends ToggleButton with EditableFeature {
  override def getVal: String = status
  override def setVal(v:String): Unit = {status = v; text = v}

  text = toDisplay
  private var status = toDisplay
  reactions += {
      case ButtonClicked(_) =>
        status match { case `toDisplay` => update(cmdMsg = cmd1) case _ => update(cmdMsg = cmd2)}
  }

  val switchStatus: (String => Unit) => String = (c: String => Unit) => {
    c(status)
    status
  }
  override def update(devName : String = devName,cmdMsg :String, newValue : String = switchStatus{case `toDisplay` => status = other case _ => status = toDisplay}): Future[Unit] ={
    val p = Promise[Unit]
    Coordinator.sendUpdate(devName,cmdMsg).onComplete {
      case Success(_) => text = status; p.success(()=>Unit)
      case _ => //TODO: print error dialog
    }
    p.future
  }
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