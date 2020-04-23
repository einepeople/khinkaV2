package crudkhalnaya

import java.time.LocalDate

import cats.effect._
import crudkhalnaya.errors.{CRUDError, MalformedCommandError, ParseError}
import crudkhalnaya.model.Client
import crudkhalnaya.utils.Utils.EitherErr
import doobie._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object REPL {
  sealed trait Command
  case object Exit extends Command
  sealed trait ClientCommand extends Command

  case class AddClient(client: Client) extends ClientCommand
  case class FetchClient(id: Int) extends ClientCommand
  case class UpdateName(id: Int, newName: String) extends ClientCommand
  case class UpdateAddress(id: Int, newAddress: String) extends ClientCommand
  case class UpdateBirthdate(id: Int, newBD: LocalDate) extends ClientCommand
  case class UpdateSex(id: Int, newSex: Boolean) extends ClientCommand
  case class DeleteClient(id: Int) extends ClientCommand

  private def parseGender(input: String): EitherErr[Boolean] = {
    input.toLowerCase match {
      case "male" ⇒ Right(true)
      case "female" ⇒ Right(false)
      case _ ⇒ Left(MalformedCommandError)
    }
  }

  private def parseBirthdate(input: String): EitherErr[LocalDate] = {
    val t = Try[LocalDate](LocalDate.parse(input))
    t match {
      case Success(value) ⇒ Right(value)
      case Failure(_) ⇒ Left(MalformedCommandError)
    }
  }

  private def parseInt(input: String): EitherErr[Int] =
    Try[Int](input.toInt) match {
      case Failure(exception) ⇒ Left(ParseError)
      case Success(value) ⇒ Right(value)
    }

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
        for {
          checkedInt ← parseInt(maybeId)
        } yield FetchClient(checkedInt)
      case s"for client $maybeId set name to $newName" ⇒
        for {
          checkId ← parseInt(maybeId)
          checkName ← Either.cond(
            newName.isBlank,
            newName,
            MalformedCommandError
          )
        } yield UpdateName(checkId, checkName)

    }
  }
  @tailrec
  def runREPL(trs: Transactor[IO]): IO[ExitCode] = {
    runREPL(trs)
  }
}
