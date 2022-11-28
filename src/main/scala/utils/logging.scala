package utils

import org.legogroup.woof.{given, *}
import cats.effect.IO

object logging:
  def forConsoleIo(): IO[DefaultLogger[IO]] =
    given Filter = Filter.everything
    given Printer = ColorPrinter()
    DefaultLogger.makeIo(Output.fromConsole)
