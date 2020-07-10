import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence
import org.eclipse.paho.client.mqttv3.{IMqttDeliveryToken, MqttCallback, MqttClient, MqttConnectOptions, MqttMessage}

import scala.util.{Failure, Success, Try}

package HOME

object Rooms {
  //TODO THIS LIST SHOULD BE MADE IN THE INTERFACE
  private val _allRooms = Array("salotto")

  def allRooms = _allRooms
}

sealed trait Device {
  def name : String
  def room : String
  def device_type : String
  def consumption : Int
}

sealed trait AssociableDevice extends Device {
  //Quality of Service
  private val QoS_0: Int = 0;
  private val QoS_1: Int = 1;
  private val QoS_2: Int = 2;

  private val retained: Boolean = true;
  private val disconnectedTopic: String = "disconnected" //Topic used by the device when the connection is lost
  private val brokerURL: String = "tcp://localhost:1883"
  private val persistence: MemoryPersistence = new MemoryPersistence

  var client: MqttClient = null

  def pubTopic: String  //Topic used by sensors to send data
  def subTopic: String = getSubTopic  //Topic used by actuators to receive orders
  def regTopic: String = "registration" //Topic used by the device to register to the system
  def broadcastTopic: String = "broadcast" //Topic the device listens to for general orders

  def getSubTopic: String = room + "/" + device_type + "/" + name

  def connect: Boolean =
    Try {
      client = new MqttClient(brokerURL, name, persistence)
      val opts = new MqttConnectOptions()
      opts.setCleanSession(true)
      opts.setWill(pubTopic, disconnectedTopic.getBytes, QoS_1, !retained)

      val callback = new MqttCallback {
        override def deliveryComplete(token: IMqttDeliveryToken): Unit = {
          //Do nothing
        }

        override def connectionLost(cause: Throwable): Unit = {
          client.connect(opts) //Auto reconnects*/
        }

        override def messageArrived(topic: String, message: MqttMessage): Unit = {
          topic match {
            case subTopic => onMessageReceived(new String(message.getPayload))
          }
        }
      }
      client.setCallback(callback)
      client.connect(opts)
    } match {
      case Failure(exception) =>
        println(s"ERROR : $exception + ${exception.getCause}")
        client = null
        false
      case Success(_) =>
        println("Connected !")
        true
      case _ =>
        println("Unexpected connection result")
        false
    }

  def subscribe: Boolean = client match {
    case null => false
    case _ => client.subscribe(pubTopic, QoS_1) true
  }

  def onMessageReceived(message: String): Unit

  def publish(message: String): Boolean = client match {
    case null => false
    case _ => client.getTopic(pubTopic).publish(s"$message".getBytes, QoS_1, retained) true
  }
}

sealed trait DeviceFactory {

  def Light(): AssociableDevice

}

//helper object used by various devices to set the output strength
object IntensityChecker {
    def apply(min: Int, max: Int)(value: Int): Int = value match {
      case x if x > max => max
      case x if x < min => min
      case _ => value
    }
}

object Light {
  def apply(name: String, room: String, device_type: String = "Light", consumption: Int = 5) = new SimulatedLight(name,room,device_type,consumption)
}

case class SimulatedLight(override val name: String, override val room: String, override val device_type: String, override val consumption: Int) extends AssociableDevice {

  require(device_type == "Light")
  require(Rooms.allRooms contains room, "Incorrect room")

  override def pubTopic = null  //the light device has no sensor

  private var _on = false

  //min, max value for the intensity
  val minIntensity = 1
  val maxIntensity = 100
  private var intensity = 50

  def getIntensity = intensity
  def isOn = _on

  def turnOn(): Unit = _on = true
  def turnOff(): Unit = _on = false

  private def _mapIntensity = IntensityChecker(minIntensity,maxIntensity)(_)
  def setIntensity(value: Int): Unit = intensity = _mapIntensity(value)

  override def equals(that: Any) = that match {
    case SimulatedLight(name,_,_,_) => this.name == name
    case _ => false
  }

  val intensityMsg = "(setIntensity_)(\\d+)".r
  override def onMessageReceived(message: String): Unit = message match {
    case "on" => turnOn()
    case "off" => turnOff()
    case intensityMsg(_,value) => setIntensity(value.toInt)
    case _ => println("Unexpected message: " + message)
  }
}
