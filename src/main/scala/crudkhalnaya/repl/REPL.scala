package crudkhalnaya.repl

import java.time.LocalDate

import cats.effect._
import crudkhalnaya.errors.{CommandNotFoundError, ParseError}
import crudkhalnaya.model.Client
import crudkhalnaya.utils.Utils.EitherErr
import doobie._
import crudkhalnaya.repl.REPLCommands._

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object REPL {
  private def parseSex(input: String): EitherErr[Boolean] = {
    input.toLowerCase match {
      case "male" ⇒ Right(true)
      case "female" ⇒ Right(false)
      case _ ⇒ Left(ParseError)
    }
  }

  private def parseBirthdate(input: String): EitherErr[LocalDate] = {
    val t = Try[LocalDate](LocalDate.parse(input))
    t match {
      case Success(value) ⇒ Right(value)
      case Failure(_) ⇒ Left(ParseError)
    }
  }

  private def parseInt(input: String): EitherErr[Int] =
    Try[Int](input.toInt) match {
      case Failure(_) ⇒ Left(ParseError)
      case Success(value) ⇒ Right(value)
    }

  private def checkForExit(value: EitherErr[Command]): Boolean = {
    value match {
      case Right(validCmd) if validCmd == Exit ⇒ true
      case _ ⇒ false
    }
  }
  private def parseCommand(input: String): EitherErr[Command] = {
    input match {
      case "exit" ⇒
        Right(Exit)
      case s"add new client $name, $sox, born $date, address $addr" ⇒
        for {
          checkName ← Either.cond(name.isBlank, name, ParseError)
          checkAddr ← Either.cond(addr.isBlank, addr, ParseError)
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
          checkName ← Either.cond(newName.isBlank, newName, ParseError)
        } yield UpdateName(checkId, checkName)
      case s"for client $maybeId set address to $newAddress" ⇒
        for {
          checkId ← parseInt(maybeId)
          checkName ← Either.cond(newAddress.isBlank, newAddress, ParseError)
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
      case _ ⇒ Left(CommandNotFoundError)
    }
  }
  private def interpret(value: EitherErr[Command],
                        xa: Transactor[IO]): IO[Unit] = {
    value match {
      case Left(err) ⇒
        IO(println(s"Error occured during command parsing: $err"))
      case Right(cmd) ⇒
        interpretCommand(cmd, xa)
    }
  }
  private def interpretCommand(value: Command, xa: Transactor[IO]): IO[Unit] = {
    value match {
      case REPLCommands.Exit ⇒ IO(println("Exiting..."))
      case command: ClientCommand ⇒ ???
    }
  }

  def runREPL(trs: Transactor[IO]): IO[ExitCode] = {
    for {
      input ← IO(StdIn.readLine("> "))
      cmd ← IO(parseCommand(input))
      ??? ← interpret(cmd, trs)
      _ ← IO(println(???))
      res ← if (checkForExit(cmd)) IO.pure(ExitCode.Success) else runREPL(trs)
    } yield res
  }
}
