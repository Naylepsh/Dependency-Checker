package infra

import cats.effect.IO
import org.legogroup.woof.{ *, given }

object logging:
  def forConsoleIo(): IO[DefaultLogger[IO]] =
    given Filter  = Filter.everything
    given Printer = ColorPrinter()
    DefaultLogger.makeIo(Output.fromConsole)
