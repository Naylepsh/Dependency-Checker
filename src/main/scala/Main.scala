import dependencies.Python
import scala.util.Success
import scala.util.Failure
import scala.concurrent.ExecutionContext.Implicits.global

@main def hello: Unit =
  val fileContents = """
  |Django==1.1.1
  |requests==1.0.0
  """.stripMargin
  Python
    .getDependencies(fileContents, Python.Pip.getDependencyVersions)
    .onComplete {
      case Success(data) => println(s"data = $data")
      case Failure(err)  => println(s"error = $err")
    }
  Thread.sleep(10000)
