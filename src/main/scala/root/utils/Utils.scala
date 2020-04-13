package root.utils

import cats.data.EitherT
import cats.effect.IO
import doobie.util.transactor.Transactor
import root.errors.CRUDError

object Utils {
  type EitherErr[T] = Either[CRUDError, T]
}
