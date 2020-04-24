package crudkhalnaya.errors

sealed trait CRUDError

case object ConfigError extends CRUDError
case object DBConnectionError extends CRUDError

case object CommandNotFoundError extends CRUDError
case object ParseError extends CRUDError
