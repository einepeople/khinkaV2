package crudkhalnaya.repl

import java.time.LocalDate

import crudkhalnaya.errors.{CommandNotFoundError, ParseError}
import crudkhalnaya.model.Client
import crudkhalnaya.repl.Commands._
import crudkhalnaya.utils.Utils.EitherErr

import scala.util.{Failure, Success, Try}

object Parser {
  private def parseSex(input: String): EitherErr[Boolean] = {
    input.toLowerCase match {
      case "male" ⇒ Right(true)
      case "female" ⇒ Right(false)
      case _ ⇒ Left(ParseError(s"Cannot parse sex from $input"))
    }
  }

  private def parseBirthdate(input: String): EitherErr[LocalDate] = {
    val t = Try[LocalDate](LocalDate.parse(input))
    t match {
      case Success(value) ⇒ Right(value)
      case Failure(_) ⇒ Left(ParseError(s"Cannot parse date from $input"))
    }
  }

  private def parseInt(input: String): EitherErr[Int] =
    Try[Int](input.toInt) match {
      case Failure(_) ⇒ Left(ParseError(s"Cannot parse int from $input"))
      case Success(value) ⇒ Right(value)
    }

  def parseCommand(input: String): EitherErr[Command] = {
    input match {
      case "exit" ⇒
        Right(Exit)
      case "help" ⇒
        Right(Help)
      case s"add new client $name, $sox, born $date, address $addr" ⇒
        for {
          checkName ← Either.cond(
            !name.isBlank,
            name,
            ParseError("Name cannot be blank: $name")
          )
          checkAddr ← Either.cond(
            !addr.isBlank,
            addr,
            ParseError("Address cannot be blank")
          )
          checkSex ← parseSex(sox)
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
            !newName.isBlank,
            newName,
            ParseError("Name cannot be blank")
          )
        } yield UpdateName(checkId, checkName)
      case s"for client $maybeId set address to $newAddress" ⇒
        for {
          checkId ← parseInt(maybeId)
          checkName ← Either.cond(
            !newAddress.isBlank,
            newAddress,
            ParseError("Name cannot be blank")
          )
        } yield UpdateAddress(checkId, checkName)
      case s"for client $maybeId set birthdate to $newBD" ⇒
        for {
          checkId ← parseInt(maybeId)
          checkBD ← parseBirthdate(newBD)
        } yield UpdateBirthdate(checkId, checkBD)
      case s"for client $maybeId set sex to $newsex" ⇒
        for {
          checkId ← parseInt(maybeId)
          checkSex ← parseSex(newsex)
        } yield UpdateSex(checkId, checkSex)
      case s"delete client $maybeId" ⇒
        for {
          checkId ← parseInt(maybeId)
        } yield DeleteClient(checkId)
      case "show all clients" ⇒
        Right(FetchAllClients)
      case x ⇒ Left(CommandNotFoundError(s"Unknown command $x"))
    }
  }
}
