package crudkhalnaya

import cats.data.EitherT
import cats.effect._
import cats.implicits._
import crudkhalnaya.errors.{CRUDError, _}
import crudkhalnaya.utils.Config
import crudkhalnaya.utils.Utils.EitherErr
import doobie.implicits._
import doobie.util.transactor.Transactor
import doobie.util.ExecutionContexts
import pureconfig._
import pureconfig.generic.auto._

import scala.util.control.NonFatal

object Main extends IOApp {

  implicit val syncCtx: ContextShift[IO] =
    IO.contextShift(ExecutionContexts.synchronous)

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
        .liftExecutionContext(ExecutionContexts.synchronous)
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

  def run(args: List[String]): IO[ExitCode] = {

    val xaEither: EitherT[IO, CRUDError, Transactor[IO]] = for {
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
    } yield tested
    // btw, how could I take Config out? like yield (tested, cfg). I tried, but there are some typing problems

    val xa = xaEither.value // Don't you ask a single question >_>

    xa.flatMap {
      case Left(err) =>
        IO(println(s"Error during initialization: $err")).as(ExitCode.Error)
      case Right(trs) =>
        REPL.runREPL(trs)
//        val program1 =
//          sql"SELECT * FROM INFORMATION_SCHEMA.USERS "
//            .query[(String, Boolean, String, Int)]
//            .stream
//            .compile
//            .toList
//        for {
//          i ← program1.transact(trs)
//          _ ← IO(println(i))
//        } yield (ExitCode.Success)
    }
  }
}
