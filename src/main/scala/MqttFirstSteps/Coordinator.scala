package MqttFirstSteps

import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence
import org.eclipse.paho.client.mqttv3._

import scala.util.{Failure, Success, Try}

object Coordinator extends App {
  //MQTT Utils
  val QoS_0 = 0;
  val QoS_1 = 1;
  val QoS_2 = 2;
  val retained = true;
  val notRetained = false;
  val disconnected = "Disconnected"
  val brokerURL = "tcp://localhost:1883"
  val persistence = new MemoryPersistence

  //Coordinator Interface
  val uniqueName = "Coordinator"

  //Pub topics
  val broadcast = "Broadcast"

  //Sub topics
  //TODO
  val subTemperature = "Kitchen/temperature"

  Try {
    val client = new MqttClient(brokerURL, uniqueName, persistence)
    val opts = new MqttConnectOptions()
    opts.setCleanSession(true)
    opts.setWill(broadcast, disconnected.getBytes, QoS_1, retained)

    val callback = new MqttCallback {
      override def deliveryComplete(token: IMqttDeliveryToken): Unit = {
        //Do nothing
      }

      override def connectionLost(cause: Throwable): Unit = {
        /*println(s"ERROR1 : $cause + ${cause.getCause}")
        client.connect(opts)  //Auto reconnects*/
      }

      override def messageArrived(topic: String, message: MqttMessage): Unit = {
        topic match {
          case subTemperature => println(new String(message.getPayload))
        }
      }
    }

    client.setCallback(callback)
    client.connect(opts)

    client.subscribe(subTemperature, QoS_1)

  } match {
    case Failure(exception) => println(s"ERROR : $exception + ${exception.getCause}")
    case Success(_) => println(s"OK !")
  }
}
