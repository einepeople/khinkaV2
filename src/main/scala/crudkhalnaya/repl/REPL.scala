package crudkhalnaya.repl

import cats.effect._
import crudkhalnaya.utils.Utils.EitherErr
import doobie._
import crudkhalnaya.repl.Commands._

import scala.io.StdIn
import Parser.parseCommand
import Executor.executeCommand

object REPL {
  def execute(value: EitherErr[Command], xa: Transactor[IO]): IO[Unit] = {
    value match {
      case Left(err) ⇒
        IO(println(s"Error occured during command parsing: $err"))
      case Right(cmd) ⇒
        executeCommand(cmd, xa)
    }
  }
  def checkForExit(value: EitherErr[Command]): Boolean = {
    value match {
      case Right(validCmd) if validCmd == Exit ⇒ true
      case _ ⇒ false
    }
  }

  def runREPL(trs: Transactor[IO]): IO[ExitCode] = {
    for {
      input ← IO(StdIn.readLine("Enter a command\n> "))
      cmd ← IO(parseCommand(input))
      _ ← execute(cmd, trs)
      res ← if (checkForExit(cmd)) IO.pure(ExitCode.Success) else runREPL(trs)
    } yield res
  }
}
