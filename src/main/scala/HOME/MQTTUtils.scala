package HOME

import org.eclipse.paho.client.mqttv3.{IMqttDeliveryToken, MqttCallback, MqttClient, MqttConnectOptions, MqttMessage}
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence

import scala.util.{Failure, Success, Try}

trait MQTTUtils extends JSONUtils {
  //Quality of Service
  val QoS_0: Int = 0
  val QoS_1: Int = 1
  val QoS_2: Int = 2

  var sender: JSONSender = _
  var client: MqttClient = _

  val retained: Boolean = true
  val topicSeparator: String = "/"
  val broadcastTopic: String = "broadcast" //Topic the devices listen to for general messages
  val regTopic: String = "registration" //Topic used by the devices to register/disconnect to/from the system
  val regMsg: String = "register" //Message used by the devices to register to the system
  val regSuccessMsg: String = "registered" //Message used by the devices to register to the system
  val disconnectedMsg: String = "disconnected" //Message used when the connection is lost
  val onMsg: String = "on"
  val offMsg: String = "off"
  val brokerURL: String = "tcp://localhost:1883"
  val persistence: MemoryPersistence = new MemoryPersistence

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

  def subscribe(topic: String): Boolean = client match {
    case null => false
    case _ =>
      if (topic != null) client.subscribe(topic, QoS_1)
      true
  }

  def unsubscribe(topic: String): Boolean = client match {
    case null => false
    case _ =>
      if (topic != null) client.unsubscribe(topic)
      true
  }

  def publish(pubTopic:String, message: String, sender: JSONSender, retained: Boolean = !retained): Boolean = client match {
    case null =>
      false
    case _ =>
      client.getTopic(pubTopic).publish(getMsg(message, sender).getBytes, QoS_1, retained)
      true
  }

  def disconnect: Boolean = client match {
    case null => true
    case _ =>
      publish(sender.lastWillTopic, sender.lastWillMessage, sender)
      client.disconnect()
      client = null
      true
  }
}
