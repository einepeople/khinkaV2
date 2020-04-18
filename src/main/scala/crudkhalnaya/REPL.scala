package crudkhalnaya

import java.time.LocalDate

import cats.effect._
import crudkhalnaya.errors.{CRUDError, MalformedCommandError}
import crudkhalnaya.model.Client
import doobie._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object REPL {
  sealed trait Command
  case object Exit extends Command
  sealed trait ClientCommand extends Command

  case class AddClient(client: Client) extends ClientCommand
  case class FetchClient(id: Int) extends ClientCommand
  case class UpdateClient(id: Int, new_data: Client) extends ClientCommand
  case class DeleteClient(id: Int) extends ClientCommand

  private def parseGender(input: String): Either[CRUDError, Boolean] = {
    input.toLowerCase match {
      case "male" ⇒ Right(true)
      case "female" ⇒ Right(false)
      case _ ⇒ Left(MalformedCommandError)
    }
  }

  private def parseBirthdate(input: String): Either[CRUDError, LocalDate] = {
    val t = Try[LocalDate](LocalDate.parse(input))
    t match {
      case Success(value) ⇒ Right(value)
      case Failure(_) ⇒ Left(MalformedCommandError)
    }
  }

  //private def parseInt(input: String):

  private def parseCommand(input: String): Either[CRUDError, Command] = {
    input match {
      case s"add new client $name, $sox, born $date, address $addr" ⇒
        for {
          checkName ← Either.cond(name.isBlank, name, MalformedCommandError)
          checkAddr ← Either.cond(addr.isBlank, addr, MalformedCommandError)
          checkSex ← parseGender(sox)
          checkBirth ← parseBirthdate(date)
        } yield
          AddClient(Client(-1, checkName, checkAddr, checkBirth, checkSex))
      case s"get client with id $maybeId" ⇒
        Try[Int](maybeId.toInt) match {
          case Failure(exception) ⇒ Left(MalformedCommandError)
          case Success(id) ⇒ Right(FetchClient(id))
        }
//      case s"for user $id set $col to $val" ⇒
//        for

    }
  }
  @tailrec
  def runREPL(trs: Transactor[IO]): IO[ExitCode] = {
    runREPL(trs)
  }
}
