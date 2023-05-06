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
    (scanOpts orElse exportOpts).map {
      case command @ ScanRepositories(_)     => ScanRepositories.run(command)
      case command @ ExportScanReports(_, _) => ExportScanReports.run(command)
    }
