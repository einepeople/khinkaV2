package root.errors

sealed trait CRUDError

case object ConfigError extends CRUDError
case object DBConnectionError extends CRUDError
