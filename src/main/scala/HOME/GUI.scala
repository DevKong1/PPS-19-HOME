import javafx.application.Application
import javafx.geometry.Pos
import javafx.scene.Scene
import javafx.scene.layout.{HBox, StackPane, VBox}
import javafx.stage.Stage
import javafx.scene.control.{Button, Label}

class GUI extends Application {
  println("GUI()")

  override def start(primaryStage: Stage) {
    primaryStage.setTitle("HOME")

    val root = new VBox()
    val title = new HBox(new Label("Welcome to your HOME!"))
    title setAlignment Pos.CENTER

    val btn = new Button("Join")
    root.getChildren.add(title)
    root.getChildren.add(new Label("Hello world!"))
    root.getChildren.add(btn)

    primaryStage.setScene(new Scene(root, 300, 300))
    primaryStage.show()
  }
}

object GUI {
  def main(args: Array[String]) {
    Application.launch(classOf[GUI], args: _*)
  }
}