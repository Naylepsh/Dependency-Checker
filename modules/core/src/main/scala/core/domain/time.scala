package core.domain

import cats.Show
import cats.effect.kernel.Sync
import org.joda.time.DateTime
import org.joda.time.Duration

trait Time[F[_]]:
  def currentDateTime: F[DateTime]

object Time:
  def apply[F[_]: Time]: Time[F] = summon

  given [F[_]: Sync]: Time[F] with
    def currentDateTime: F[DateTime] = Sync[F].delay(DateTime.now())

  enum DeltaUnit:
    case Seconds, Minutes, Hours, Days, Months, Years

  case class Delta(unit: DeltaUnit, value: Long)
  object Delta:
    def apply(a: DateTime, b: DateTime): Delta =
      val diff    = Duration(a, b)
      val seconds = diff.getStandardSeconds().abs
      val minutes = diff.getStandardMinutes().abs
      val hours   = diff.getStandardHours().abs
      val days    = diff.getStandardDays().abs

      if days > 365
      then new Delta(DeltaUnit.Years, days / 365)
      else if days > 31
      then new Delta(DeltaUnit.Months, days / 30)
      else if days > 0
      then new Delta(DeltaUnit.Days, days)
      else if hours > 0
      then new Delta(DeltaUnit.Hours, hours)
      else if minutes > 0
      then new Delta(DeltaUnit.Minutes, minutes)
      else new Delta(DeltaUnit.Seconds, seconds)

    given Show[Delta] with
      def show(delta: Delta) =
        val unitStr = delta.unit match
          case DeltaUnit.Seconds => "seconds"
          case DeltaUnit.Minutes => "minutes"
          case DeltaUnit.Hours   => "hours"
          case DeltaUnit.Days    => "days"
          case DeltaUnit.Months  => "months"
          case DeltaUnit.Years   => "years"
        s"${delta.value} $unitStr"
