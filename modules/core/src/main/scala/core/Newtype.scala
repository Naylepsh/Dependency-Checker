package core

import cats.kernel.Order
import io.circe.{ Decoder, Encoder }

abstract class Newtype[A](
    using ord: Order[A],
    enc: Encoder[A],
    dec: Decoder[A]
):
  /**
   * Shamelessly copy-pasted from:
   * https://github.com/gvolpe/trading/blob/main/modules/domain/shared/src/main/scala/trading/Newtype.scala
   */
  opaque type Type = A

  inline def apply(a: A): Type = a

  protected inline final def derive[F[_]](using ev: F[A]): F[Type] = ev

  extension (t: Type) inline def value: A = t

  given Order[Type]   = ord
  given Encoder[Type] = enc
  given Decoder[Type] = dec
