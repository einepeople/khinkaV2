package crudkhalnaya.errors

sealed trait CRUDError {
  def toString: String
}

case class ConfigError(msg: String) extends CRUDError {
  override def toString: String = msg
}
case class DBConnectionError(msg: String) extends CRUDError {
  override def toString: String = msg
}

case class CommandNotFoundError(msg: String) extends CRUDError {
  override def toString: String = msg
}
case class ParseError(msg: String) extends CRUDError {
  override def toString: String = msg
}
case object UserNotFound extends CRUDError
case object OrderNotFound extends CRUDError
case object ItemInBucketNotFound extends CRUDError
