import application.cli.*
import cats.effect.{ ExitCode, IO }
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp

object Main extends CommandIOApp(
      name = "sentinel",
      header = "",
      version = "0.3.0"
    ):

  def main: Opts[IO[ExitCode]] =
    (scanOpts orElse listScansOpts orElse exportOpts).map(_.run())
