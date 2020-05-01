package crudkhalnaya.repl

import cats.effect._
import crudkhalnaya.utils.Utils.EitherErr
import doobie._
import crudkhalnaya.repl.Commands._

import scala.io.StdIn
import Parser.parseCommand
import Executor.executeCommand
import scala.concurrent.duration.SECONDS

object REPL {
  def execute(value: EitherErr[Command], xa: Transactor[IO]): IO[Unit] = {
    value match {
      case Left(err) ⇒
        IO(println(s"Error occurred during command parsing: $err"))
      case Right(cmd) ⇒
        executeCommand(cmd, xa)
    }
  }
  def checkForExit(value: EitherErr[Command]): Boolean = {
    value match {
      case Right(validCmd) if validCmd.equals(Exit) ⇒ true
      case _ ⇒ false
    }
  }

  def runREPL(trs: Transactor[IO])(implicit clock: Clock[IO]): IO[ExitCode] = {
    for {
      input ← IO(StdIn.readLine("\n> "))
      curTime ← clock.realTime(SECONDS)
      cmd ← IO(parseCommand(input.strip(), curTime))
      _ ← execute(cmd, trs)
      res ← if (checkForExit(cmd)) IO.pure(ExitCode.Success) else runREPL(trs)
    } yield res
  }
}
