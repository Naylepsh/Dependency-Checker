import dependencies.Python
import scala.util.Success
import scala.util.Failure
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import upickle.default.{ReadWriter => RW, macroRW}

def hello: Unit =
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

case class PackageInfo(version: String)
object PackageInfo {
  implicit val rw: RW[PackageInfo] = macroRW
}

case class PypiResponse(info: PackageInfo)
object PypiResponse {
  implicit val rw: RW[PypiResponse] = macroRW
}

@main def run: Unit =
  val r = requests.get("https://pypi.org/pypi/Django/json")

  val output = upickle.default.read[PypiResponse](r.text())

  println(output)

  // println(r.text())
