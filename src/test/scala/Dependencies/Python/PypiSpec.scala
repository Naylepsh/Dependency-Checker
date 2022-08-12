package Dependencies.Python

import org.scalatest._
import org.scalatest.OptionValues.convertOptionToValuable
import flatspec._
import matchers._

class PypiSpec extends AnyFlatSpec with should.Matchers {
  import Pypi._

  "Pypi response" should "be transformable from json-string to appropriate case class" in {
    import Dependencies.Utils.JSON

    val response = """{"info": {"version": "1.2.3"} }"""

    val parsed = JSON.parse[PypiResponse](response)

    parsed.info.version shouldBe "1.2.3"
  }
}
