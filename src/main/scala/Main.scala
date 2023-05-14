import application.cli.*
import cats.effect.{ ExitCode, IO }
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp

object Main extends CommandIOApp(
      name = "sentinel",
      header = "",
      version = "0.4.0"
    ):

  def main: Opts[IO[ExitCode]] =
    (scanOpts
      orElse listScansOpts
      orElse deleteScansOpts
      orElse exportOpts).map(_.run())
