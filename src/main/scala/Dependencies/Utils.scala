package Dependencies

import upickle.default.{ReadWriter => RW, macroRW}

object Utils {
  object JSON {
    def parse[T: RW](jsonString: String): T =
      upickle.default.read[T](jsonString)
  }

  def ltrim(s: String): String = s.replaceAll("^\\s+", "")
}
