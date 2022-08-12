package Dependencies.Python

import org.scalatest._
import org.scalatest.OptionValues.convertOptionToValuable
import flatspec._
import matchers._
import Dependencies.Utils.JSON

class PypiSpec extends AnyFlatSpec with should.Matchers {
  import Pypi._

  "Pypi response" should "be transformable from json-string to appropriate case class" in {

    val response = """{
      "info": {"version": "1.2.3"}, 
      "releases": {"0.0.1": [{"upload_time": "2020-10-16T17:37:23", "requires_python": ""}]},
      "vulnerabilities": [
        {"id": "PYSEC-2021-9", "details": "Some desc. here"},
        {"id": "PYSEC-2021-8", "details": "More data here"}
      ] 
    }
    """

    val parsed = JSON.parse[PypiResponse](response)

    parsed.info.version shouldBe "1.2.3"
  }
}
