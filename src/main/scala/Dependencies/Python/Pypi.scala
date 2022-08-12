package Dependencies.Python

import upickle.default.{ReadWriter => RW, macroRW}
import Dependencies.Utils
import scala.util.Try

object Pypi {
  case class PackageInfo(version: String)
  object PackageInfo {
    given rw: RW[PackageInfo] = macroRW
  }

  case class PypiResponse(info: PackageInfo)
  object PypiResponse {
    given RW[PypiResponse] = macroRW
  }

  def getLatestVersion(packageName: String): Try[String] = Try {
    val response = requests.get(s"https://pypi.org/pypi/$packageName/json")
    Utils.JSON.parse[PypiResponse](response.text()).info.version
  }
}
