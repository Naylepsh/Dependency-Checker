import cats.effect.{ ExitCode, IO }
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import core.application.cli as CoreCli
import upkeep.application.UpkeepCli

object Main extends CommandIOApp(
      name = "sentinel",
      header = "",
      version = "0.6.0"
    ):

  def main: Opts[IO[ExitCode]] =
    (CoreCli.allOpts orElse UpkeepCli.allOpts).map(_.run())
