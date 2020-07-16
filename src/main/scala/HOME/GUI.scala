package HOME


import java.awt.{Dimension, GraphicsEnvironment}
import java.util.concurrent.Future

import javax.swing.Icon

import scala.collection.GenSet
import scala.collection.mutable.ListBuffer
import scala.swing.Dialog.{Message, showInput}
import scala.swing.Swing.EmptyIcon
import scala.swing._
import scala.swing.event.{ButtonClicked, SelectionChanged}

sealed trait Room {
  def devices : ListBuffer[Device]
  def name : String
}

//TODO: RECONSIDER HOW TO MODEL GUIROOM
 class GUIRoom(override val name:String) extends Panel with Room{
  override def devices: ListBuffer[Device] = ???

  /* var a : Device = new SimulatedLight("Lamp1", name, LightType, 1)
  var b : Device = new SimulatedLight("Lamp2", name, LightType, 1)
  devices += a
  devices += b*/

  override def equals(that: Any): Boolean = that match{
    case that: GUIRoom=> name equals that.name
    case _ => false
  }
 /* for( c <- devices) {
    _contents += Button("" + c.name+""+c.room)(println("Clicked device: " + c.name))
    _contents += Swing.HStrut(20)
  }*/
   def apply(roomName: String): GUIRoom =  new GUIRoom(roomName)
}

object SimpleGUI extends SimpleSwingApplication {
  private val ADD = "+"

  private var rooms: Set[GUIRoom] = Set(new GUIRoom("Home"), new GUIRoom("Kitchen"), new GUIRoom("Bedroom"),new GUIRoom(ADD))

  val tp: TabbedPane = new TabbedPane {
    for(i <- rooms) yield {pages += new TabbedPane.Page(i.name,i)}
  }
    def top: MainFrame = new MainFrame {
      title = "Home!"
      println("Welcome")
      reactions += {
        case SelectionChanged(_) => {
          for {
            isLast <- checkLast()
            name <-  getRoomName
          } yield {
            tp.pages.remove(tp.pages.length-1)
            tp.pages+=new TabbedPane.Page(name, new GUIRoom(name))
            tp.pages+=new TabbedPane.Page(ADD,new GUIRoom(ADD))
          }
        }
      }
      //used to set items in the main window inside a vertical BoxPanel
      contents = new BoxPanel(Orientation.Vertical) {
        contents ++= Seq(tp)

      }
      listenTo(tp.selection)
      size = WindowSize()

      object checkLast {
        def apply(): Option[Boolean] = {
          tp.selection.page.title match {
            case "+" => {
              Some(true)
            }
            case _ => None
          }
        }
      }

      def getRoomName: Option[String] = {
        // TODO: SE AGGIUNTA STANZA VIENE CHIUSA DA ANNULLA O X QUALUNQUE SIA IL NOME SPARA ERRORE
        import Dialog._
        val name = showInput(tp,
          "Room name:",
          "Add room",
          Message.Plain,
          Swing.EmptyIcon,
          Nil, "")
        println(name.isDefined)
        if (name.isDefined && !name.get.equals(ADD) && !rooms.contains(new GUIRoom(name.get))) name else {
          showMessage(tp, "Room already existing or incorrect room name", Message.Error toString)
          None
        }

      }
    }
  }

  object WindowSize {
    private val SCREEN = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
    private val FRAME_PROP = 0.55

    val height: Int = SCREEN.height * FRAME_PROP toInt
    val width: Int = SCREEN.width * FRAME_PROP toInt

    def apply(): Dimension = {
      new Dimension(width, height)
    }
  }


