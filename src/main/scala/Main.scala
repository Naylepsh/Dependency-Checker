import cats.effect.{ ExitCode, IO }
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import upkeep.application.UpkeepCli
import scanning.application.ScanningCli

object Main extends CommandIOApp(
      name = "sentinel",
      header = "",
      version = "0.6.3"
    ):

  def main: Opts[IO[ExitCode]] =
    (ScanningCli.allOpts orElse UpkeepCli.allOpts).map(_.run())
