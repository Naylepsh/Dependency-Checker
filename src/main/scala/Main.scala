import cats.effect.{ ExitCode, IO }
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import scanning.application.ScanningCli
import upkeep.application.UpkeepCli

object Main extends CommandIOApp(
      name = "ganyu",
      header = "",
      version = "0.9.2"
    ):

  def main: Opts[IO[ExitCode]] =
    (ScanningCli.allOpts orElse UpkeepCli.allOpts).map(_.run())
