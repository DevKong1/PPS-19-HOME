package HOME

import org.scalatest.funsuite.AnyFunSuite

class UtilsTest extends AnyFunSuite {
    import MyClass._

    Rooms.addRoom("Living room")
    val aaa: SimulatedLight = Light("AAA","Living room")

    test("The implicit conversion works as intended") {
        assert(aaa.deviceType.getSimpleClassName == "LightType")
    }

    test("The logger logs correctly") {
        Logger.setTestFile()

        assert(Logger.log(id = "AAA",cmd = Msg.on,consumption = 5))
        assert(Logger.log(id = "AAA",cmd = Msg.off,consumption = 5))

        val fileData = Logger.getLogAsListWithHeader
        val firstRow = fileData.head
        val secondRow = fileData(1)
        assert(firstRow("ID") == "AAA" && firstRow("CMD") == "on" && firstRow("Consumption") == "5")
        assert(secondRow("ID") == "AAA" && secondRow("CMD") == "off" && secondRow("Consumption") == "5")

        Logger.resetFile()
        Logger.unsetTestFile()
    }
}
