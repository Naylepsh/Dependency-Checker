package domain

import org.joda.time.DateTime
import cats.effect.kernel.Sync

trait Time[F[_]]:
  def currentDateTime: F[DateTime]

object Time:
  def apply[F[_]: Time]: Time[F] = summon

  given [F[_]: Sync]: Time[F] with
    def currentDateTime: F[DateTime] = Sync[F].delay(DateTime.now())
