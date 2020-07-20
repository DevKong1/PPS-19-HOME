package HOME

import HOME.CommandMsg.messageSeparator
import HOME.MyClass._
import org.eclipse.paho.client.mqttv3.{IMqttDeliveryToken, MqttCallback, MqttClient, MqttConnectOptions, MqttMessage}
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence

import scala.util.{Failure, Success, Try}

trait MQTTUtils extends JSONUtils {
  val retained: Boolean = true
  val topicSeparator: Char = '/'
  val broadcastTopic: String = "broadcast" //Topic the devices listen to for general messages
  val regTopic: String = "registration" //Topic used by the devices to register/disconnect to/from the system

  //Quality of Service
  //private val QoS_0: Int = 0
  private val QoS_1: Int = 1
  //private val QoS_2: Int = 2

  private var sender: JSONSender = _
  private var client: MqttClient = _

  private val brokerURL: String = "tcp://localhost:1883"
  private val persistence: MemoryPersistence = new MemoryPersistence

  class ConnectionException(message: String) extends Exception(message)

  def connect(_sender: JSONSender, onMessageReceived: (String,String) => Unit): Boolean = client match {
    case null => Try {
      sender = _sender
      client = new MqttClient(brokerURL, sender.name, persistence)
      val opts = new MqttConnectOptions
      opts.setCleanSession(true)
      opts.setWill(sender.lastWillTopic, getMsg(sender.lastWillMessage, sender).getBytes, QoS_1, !retained)

      val callback = new MqttCallback {
        override def deliveryComplete(token: IMqttDeliveryToken): Unit = {
          //Do nothing
        }

        override def connectionLost(cause: Throwable): Unit = {
          client.connect(opts) //Auto reconnects
        }

        override def messageArrived(topic: String, message: MqttMessage): Unit = {
          onMessageReceived(topic, new String(message.getPayload))
        }
      }
      client.setCallback(callback)
      client.connect(opts)
    } match {
      case Failure(exception) =>
        client = null
        throw new ConnectionException(s"ERROR : $exception + ${exception.getCause}")
      case Success(_) => true
      case _ => throw new ConnectionException("Unexpected connection result")
    }
    case c if c.isConnected => true
    case _ => false
  }

  def disconnect: Boolean = client match {
    case null => true
    case _ =>
      publish(sender.lastWillTopic, sender.lastWillMessage, sender)
      client.disconnect()
      client = null
      true
  }

  def subscribe(topic: String): Boolean = client match {
    case null => false
    case _ =>
      if (topic != null && topic != "") client.subscribe(topic, QoS_1)
      true
  }

  def unsubscribe(topic: String): Boolean = client match {
    case null => false
    case _ =>
      if (topic != null && topic != "") client.unsubscribe(topic)
      true
  }

  def publish(pubTopic:String, message: CommandMsg, sender: JSONSender, retained: Boolean): Boolean =
    publish(pubTopic, message.toString, sender, retained)

  def publish(pubTopic:String, message: String, sender: JSONSender, retained: Boolean = !retained): Boolean = client match {
    case null =>
      false
    case _ =>
      client.getTopic(pubTopic).publish(getMsg(message, sender).getBytes, QoS_1, retained)
      true
  }
}

trait CommandMsg {
  def command: String
  def value: String

  override def toString: String = command + (if(value != null) messageSeparator + value else "")
}

object CommandMsg {
  case class CommandMsgImpl(override val command: String, _value: Any = null) extends CommandMsg {
    override val value: String = if(_value != null) _value.toString else null
  }

  private val messageSeparator: Char = '_' //character used to separate data in specific device messages

  def apply(msg: String): CommandMsg = msg.split(messageSeparator).length match {
    case 1 => CommandMsgImpl(msg)
    case 2 => CommandMsgImpl(msg.split(messageSeparator)(0), msg.split(messageSeparator)(1))
    case _ => this.errUnexpected(UnexpectedMessage, msg)
  }

  def apply(msg: String, value: Any): CommandMsg = CommandMsgImpl(msg, value)
}

object Msg {
  val disconnected: String = "disconnected" //Message sent when the connection is lost
  val register: String = "register" //Message sent by the device to register to the system
  val regSuccess: String = "regSuccess" //Message sent by the coordinator to assert the device has registered successfully

  //Commands
  val on: String = "on"
  val off: String = "off"
  val open: String = "open"
  val close: String = "close"
  val setIntensity: String = "setIntensity"
  val setTemperature: String = "setTemperature"
  val setHumidity: String = "setHumidity"
  val setVolume: String = "setVolume"
  val mute: String = "mute"
  val washingType: String = "washingType"
  val RPM: String = "RPM"
  val addExtra: String = "addExtra"
  val removeExtra: String = "removeExtra"
  val setProgram: String = "setProgram"
  val setMode: String = "setMode"
}
