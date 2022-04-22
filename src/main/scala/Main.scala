import dependencies.Python
import scala.util.{Success, Failure}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.concurrent.Future

@main def hello: Unit =
  val repoPaths = List(
    "/Users/sebastian.kondraciuk/Documents/code/vsa/vsa-core",
    "/Users/sebastian.kondraciuk/Documents/code/vsa/vsa-lynx",
    "/Users/sebastian.kondraciuk/Documents/code/vsa/vsa-delegator",
    "/Users/sebastian.kondraciuk/Documents/code/vsa/vsa-pydentifier"
  )

  def appendReqsFile(path: String): String = s"$path/requirements.txt"
  def getFileContents(path: String): String =
    Source.fromFile(path).getLines.mkString("\n")

  val getRepositoryDependencies = appendReqsFile
    .andThen(getFileContents)
    .andThen(fileContents =>
      Python.getDependencies(fileContents, Python.Pypi.getLatestVersion)
    )

  val result =
    Future.sequence(repoPaths.map(x => Future { getRepositoryDependencies(x) }))
  result.onComplete {
    case Success(repoDependencies) =>
      repoDependencies.map(_.foreach { dependencies =>
        {
          println("*" * 10)
          dependencies.foreach(println)
        }
      })
    case Failure(err) => println(s"error = $err")
  }

  Thread.sleep(100000)
