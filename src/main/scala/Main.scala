import dependencies.Python
import scala.util.Success
import scala.util.Failure
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

@main def hello: Unit =
  val filename =
    "/Users/sebastian.kondraciuk/Documents/code/vsa/vsa-core/requirements.txt"

  val fileContents = Source.fromFile(filename).getLines.mkString("\n")
  Python
    .getDependencies(fileContents, Python.Pip.getDependencyVersions)
    .onComplete {
      case Success(data) => println(s"data = $data")
      case Failure(err)  => println(s"error = $err")
    }
  Thread.sleep(100000)
