package crudkhalnaya

import java.util.concurrent.Executors.newFixedThreadPool

import cats.data.EitherT
import cats.effect._
import cats.implicits._
import crudkhalnaya.errors.{CRUDError, _}
import crudkhalnaya.repl.REPL
import crudkhalnaya.utils.Config
import crudkhalnaya.utils.Utils.EitherErr
import doobie.implicits._
import doobie.util.transactor.Transactor
import doobie.util.ExecutionContexts
import pureconfig._
import pureconfig.generic.auto._

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

object Main extends IOApp {

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutor(newFixedThreadPool(16))

  implicit val syncCtx: ContextShift[IO] =
    IO.contextShift(ec)

  def loadConfig: IO[EitherErr[Config]] = {
    ConfigSource.default.load[Config] match {
      case Left(_)       => IO(Left(ConfigError))
      case Right(config) => IO(Right(config))
    }
  }

  def loadTransactor(driver: String,
                     url: String,
                     user: String,
                     pass: String): Transactor[IO] = {
    Transactor.fromDriverManager[IO](
      driver,
      url,
      user,
      pass,
      Blocker
        .liftExecutionContext(ec)
    )
  }

  def testTransactor(trs: Transactor[IO]): IO[EitherErr[Transactor[IO]]] = {
    sql"SELECT 42"
      .query[Int]
      .unique
      .transact(trs)
      .map[EitherErr[Transactor[IO]]](_ ⇒ Right(trs))
      .recover {
        case NonFatal(_) ⇒ Left(DBConnectionError)
      }
  }

  def runFallible(args: List[String]): EitherT[IO, CRUDError, ExitCode] =
    for {
      cfg ← EitherT(loadConfig)
      trs ← EitherT(
        IO(
          Either.right[CRUDError, Transactor[IO]](
            loadTransactor(
              cfg.db.driver,
              cfg.db.hostname,
              cfg.db.user,
              cfg.db.password
            )
          )
        )
      )
      tested ← EitherT(testTransactor(trs))
      exitCode ← EitherT.liftF(REPL.runREPL(tested))
    } yield exitCode

  def run(args: List[String]): IO[ExitCode] = {
    runFallible(args).value.flatMap {
      case Left(err) =>
        IO(println(s"Error during initialization: $err")).as(ExitCode.Error)
      case Right(value) =>
        IO.pure(value)
    }
  }
}
