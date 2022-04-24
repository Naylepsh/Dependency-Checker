package dependencies

import upickle.default.{ReadWriter => RW, macroRW}

object Utils {
  object Requests {
    def parseResponse[T: RW](responseText: String): T =
      upickle.default.read[T](responseText)
  }
}
