package infra

import upickle.default.{ ReadWriter as RW, macroRW }

object json:
  def parse[T: RW](jsonString: String): T =
    upickle.default.read[T](jsonString)
