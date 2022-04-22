import dependencies.Python
import scala.util.{Success, Failure, Try}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.concurrent.Future
import dependencies.Dependency

@main def hello: Unit =
  val repoPaths = List(
    "/Users/sebastian.kondraciuk/Documents/code/vsa/vsa-core",
    "/Users/sebastian.kondraciuk/Documents/code/vsa/vsa-lynx",
    "/Users/sebastian.kondraciuk/Documents/code/vsa/vsa-delegator",
    "/Users/sebastian.kondraciuk/Documents/code/vsa/vsa-pydentifier"
  )

  def appendReqsFile(path: String): String = s"$path/requirements.txt"
  def getLocalFileContents(path: String): String =
    Source.fromFile(path).getLines.mkString("\n")

  val getDeps = Python.getDependencies(
    x => Future { appendReqsFile.andThen(getLocalFileContents)(x) },
    Python.Pypi.getLatestVersion
  )

  val result =
    Future.sequence(repoPaths.map(path => Future { (path, getDeps(path)) }))
  result.onComplete {
    case Success(repoDependencies) =>
      repoDependencies.map {
        case (path, dependenciesFuture) => {
          dependenciesFuture.map { dependencies =>
            println("*" * 10)
            println(path)
            dependencies.foreach(println)
          }
        }
      }

    case Failure(err) => println(s"error = $err")
  }

  Thread.sleep(100000)
