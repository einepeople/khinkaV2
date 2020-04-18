package crudkhalnaya.utils

import crudkhalnaya.errors.CRUDError

object Utils {
  type EitherErr[T] = Either[CRUDError, T]
}
