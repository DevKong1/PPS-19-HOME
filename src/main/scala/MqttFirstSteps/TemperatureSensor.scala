package MqttFirstSteps

import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence
import org.eclipse.paho.client.mqttv3.{MqttClient, MqttConnectOptions}

import scala.util.{Failure, Random, Success, Try}

object TemperatureSensor extends App {
  //MQTT Utils
  val QoS_0 = 0;
  val QoS_1 = 1;
  val QoS_2 = 2;
  val retained = true;
  val notRetained = false;
  val disconnected = "Disconnected"
  val brokerURL = "tcp://localhost:1883"
  val persistence = new MemoryPersistence

  //Device Interface
  val uniqueName = "KTempSensor1"
  val room = "Kitchen"
  val devType = "tempSens"
  val loopTime = 3000; //ms

  //Device
  val topicTemperature = "temperature"
  val pubTopic = s"$room/$topicTemperature"

  Try {
    val client = new MqttClient(brokerURL, uniqueName, persistence)
    val opts = new MqttConnectOptions()
    opts.setCleanSession(true)
    opts.setWill(pubTopic, disconnected.getBytes, QoS_1, notRetained)
    client.connect(opts)
    val topicTemp = client.getTopic(pubTopic)

    while (true) {
      val temp = Random.nextInt(10) + 20

      println(s"Publish $temp to $topicTemperature")

      topicTemp.publish(s"$temp".getBytes, QoS_1, retained)
      Thread.sleep(loopTime)
    }
  } match {
    case Failure(exception) => println(s"ERROR : $exception + ${exception.getCause}")
    case Success(_) => println(s"OK !")
  }
}