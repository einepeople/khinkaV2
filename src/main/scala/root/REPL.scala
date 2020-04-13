package root

import root.utils.Config
import scala.annotation.tailrec
import doobie._
import doobie.implicits._
import cats._
import cats.effect._
import cats.implicits._

object REPL {
  sealed trait Command

  @tailrec
  def runREPL(trs: Transactor[IO]): IO[ExitCode] = {

    runREPL(trs)
  }
}
