package root

import root.utils.Config
import scala.annotation.tailrec
import doobie._
import doobie.implicits._
import cats._
import cats.effect._
import cats.implicits._

trait State

object MainLoop {
  def apply(config: Config): State = {
    val state = new State {}
    loopBody(state)
  }
  @tailrec
  def loopBody(state: State): State = {
    loopBody((state))
  }
}
