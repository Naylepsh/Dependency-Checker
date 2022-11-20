package services.sources

import org.scalatest._
import org.scalatest.OptionValues.convertOptionToValuable
import flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalactic.Explicitly._
import cats._
import cats.implicits._
import services.GitlabApi
import services.responses._
import services.sources.GitlabSource.ProjectProps
import domain.dependency.Dependency

class GitlabSourceSpec extends AnyFlatSpec {
  import GitlabSourceSpec._

  "Extract" should "return an empty list if failed to get the project's tree" in {
    GitlabSource
      .make(failingTreeApi)
      .extract(testProjectProps) shouldBe empty
  }

  "Extract" should "return an empty list if failed to get the project's concrete file" in {
    GitlabSource
      .make(failingFileApi, testExtractors)
      .extract(testProjectProps) shouldBe empty
  }

  "Extract" should "return the list of dependencies" in {
    val tree = List(RepositoryTreeFile("foo", "foo"))
    val expectedDependencies = List(
      Dependency("bar", None),
      Dependency("quux", None)
    )
    val file = RepositoryFile("")
    val extractors =
      Map("foo" -> ((content: String) => expectedDependencies))

    val dependencies =
      GitlabSource
        .make(dataGitlabApi(tree, file), extractors)
        .extract(testProjectProps)

    dependencies should contain only (expectedDependencies.head, expectedDependencies.tail.head)
  }
}

object GitlabSourceSpec {
  val testProjectProps = ProjectProps(id = "", branch = "")
  val testFileNames = List("foo", "bar")
  val testTree = testFileNames.map(name => RepositoryTreeFile(name, name))
  val testFiles = testFileNames.map(name => RepositoryFile("c0nt3nt"))
  val testDependencies =
    List(Dependency("baz", None), Dependency("quux", "1.2.3".some))
  val testExtractors = testFileNames
    .map(name => name -> ((x: String) => testDependencies))
    .toMap

  val failingTreeApi = makeApi(
    RuntimeException("Unable to get the tree").asLeft,
    testFiles.head.asRight
  )

  val failingFileApi =
    makeApi(testTree.asRight, RuntimeException("Unable to get the file").asLeft)

  def dataGitlabApi(tree: RepositoryTree, file: RepositoryFile) =
    makeApi(tree.asRight, file.asRight)

  def makeApi(
      treeResult: Either[Throwable, RepositoryTree],
      fileResult: Either[Throwable, RepositoryFile]
  ) = new GitlabApi[Id] {
    override def getFileTree(
        id: String,
        branch: String
    ): Id[Either[Throwable, RepositoryTree]] = treeResult

    override def getFile(
        id: String,
        branch: String,
        filePath: String
    ): Id[Either[Throwable, RepositoryFile]] = fileResult
  }
}
