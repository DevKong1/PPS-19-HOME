package HOME

import scala.language.postfixOps
import java.awt.{Dimension, GraphicsEnvironment}
import scala.swing._
import scala.swing.event.{ButtonClicked, SelectionChanged}
import HOME.MyClass._

sealed trait Room {
  def devices : Set[Device]
  def name : String
}



class GUIRoom(override val name:String) extends BoxPanel(Orientation.Vertical) with Room {
   /*always present device in every room*/
   val light: SimulatedLight = Light("A",name)
   val AC: SimulatedAirConditioner = AirConditioner("B",name)
   val dehumidifier: SimulatedDehumidifier = Dehumidifier("C",name)
   override def devices: Set[Device] = Set(light,AC,dehumidifier)
    /* lets users add devices inside a room*/
   private val adDeviceBtn: Button = new Button("Add device") {
    reactions += {
      case ButtonClicked(_) =>  DeviceDialog()
    }
   }
   val devicePanel = new BoxPanel(Orientation.Vertical)

   val bp: BorderPanel = new BorderPanel {
      add(adDeviceBtn, BorderPanel.Position.South)
   }
  contents += devicePanel
  contents += bp
   //Prints every device in this room, starting from the basic 3
   for (i <- devices)yield{printDevice(i)}




  def apply(roomName: String): GUIRoom = new GUIRoom(roomName)

  override def equals(that: Any): Boolean = that match{
    case that: GUIRoom=> this.name equals that.name
    case _ => false
  }
  //First release unpretty print
  object printDevice {
    def apply(d: Device): Unit = {
      devicePanel.contents += new Label() {
        text = {
          "Device name: " + d.name + "\t" + " Device Type: " + d.getSimpleClassName + "\n" + " Consumption: " + d.consumption + " Status: " + d.isOn
        }
      }
      repaint()
    }
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
          if(tp.selection.page.title equals ADD) {
            tp.pages.find(page => page.title equals ADD )
          }else None
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

object WindowSize {
  import WindowSizeType._
  private val SCREEN = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
  private val FRAME_PROP = 0.55
  private val WIN_PROP_H = 0.2
  private val WIN_PROP_W = 0.5

  val height: Int = SCREEN.height * FRAME_PROP toInt
  val width: Int = SCREEN.width * FRAME_PROP toInt

  def apply(windType : WindowSizeType.Value): Dimension = windType match{
    case MainW => new Dimension(width, height)
    case Dialog => new Dimension(width*WIN_PROP_W toInt,height*WIN_PROP_H toInt)
  }
}
object WindowSizeType extends Enumeration {
  type Type = Value
  val MainW, Dialog = Value
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
                  i.printDevice(c)
                  //CoordinatorImpl().addDevice(c)
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

