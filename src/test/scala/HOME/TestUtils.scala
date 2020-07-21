package HOME

import org.scalatest.Tag

//A test tagged with BrokerRequired means that it needs the MQTT Broker active and running to be executed
object BrokerRequired extends Tag("HOME.tags.BrokerRequired")

object ConstantsTest {
  val testSleepTime :Int = 50 //millis, time the matcher waits to evaluate 'eventually' expression in tests
}
