package Dependencies

import upickle.default.{ReadWriter => RW, macroRW}
import scala.util.Success
import scala.util.Try

object Utils {
  object JSON {
    def parse[T: RW](jsonString: String): T =
      upickle.default.read[T](jsonString)
  }

  def ltrim(s: String): String = s.replaceAll("^\\s+", "")

  def tryToOption[T](tryResult: Try[T]): Option[T] = tryResult match {
    case Success(value) => Some(value)
    case _              => None
  }
}
