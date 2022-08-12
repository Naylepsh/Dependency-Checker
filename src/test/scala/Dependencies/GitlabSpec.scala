package Dependencies

import org.scalatest._
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import flatspec._
import matchers._
import scala.util.Success
import scala.util.Try

class GitlabSpec extends AnyFlatSpec with should.Matchers {
  import Gitlab._

  "getProjectDependenciesFile" should "indicate missing dependency file" in {
    val tree = List()
    def getProjectTree(projectId: String): Try[RepositoryTree] = Success(tree)
    def getProjectFile(projectId: String)(path: String): Try[RepositoryFile] =
      Try { RepositoryFile("my-content") }
    val props = ProjectDependenciesFileProps(getProjectTree, getProjectFile)

    val result = getProjectDependenciesFile(props)(projectId = "1")
    result.success.value shouldBe None
  }

}
