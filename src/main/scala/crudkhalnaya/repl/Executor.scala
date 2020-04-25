package crudkhalnaya.repl

import cats.effect.IO
import crudkhalnaya.repl.Commands._
import doobie._
import doobie.implicits._
import cats._
import cats.data._
import cats.effect._
import cats.implicits._
import fs2.Stream
import crudkhalnaya.model.Client
import doobie.Transactor
import doobie.util.compat.FactoryCompat

object Executor {

  val availableCommands = List(
    "exit",
    "help",
    "show all clients",
    "add new client $name, $sox, born $date, address $addr",
    "get client with id $Id",
    "for client $Id set name to $newName",
    "for client $Id set address to $newAddress",
    "for client $Id set birthdate to $newBD",
    "for client $Id set sex to $newsex",
    "delete client $Id"
  )

  def executeCommand(value: Command, xa: Transactor[IO]): IO[Unit] = {

    //IDEA can't detect a factory for .to[List] method due to some reason
    implicit def seqFactoryCompat[A]: FactoryCompat[A, List[A]] =
      FactoryCompat.fromFactor(List.iterableFactory)

    value match {
      case Exit ⇒ IO(println("Exiting..."))
      case Help ⇒
        IO(
          println(
            ("List of available commands:" :: availableCommands).mkString("\n")
          )
        )
      case AddClient(client) ⇒
        Client
          .create(client)
          .withUniqueGeneratedKeys[Int]("id")
          .transact(xa)
          .flatMap(id ⇒ IO(println(s"Created new Client with id $id")))
      case FetchClient(id) ⇒
        Client
          .fetch(id)
          .unique
          .transact(xa)
          .flatMap(cl ⇒ {
            IO(println(cl.toString))
          })
      case UpdateName(id, newName) ⇒
        Client
          .updateName(id, newName)
          .withUniqueGeneratedKeys[Int]("id")
          .transact(xa)
          .flatMap(id ⇒ IO(println(s"User $id now has name $newName")))
      case UpdateAddress(id, newAddress) ⇒
        Client
          .updateAddress(id, newAddress)
          .withUniqueGeneratedKeys[Int]("id")
          .transact(xa)
          .flatMap(id ⇒ IO(println(s"User $id now has address $newAddress")))
      case UpdateBirthdate(id, newBD) ⇒
        Client
          .updateAddress(id, newBD)
          .withUniqueGeneratedKeys[Int]("id")
          .transact(xa)
          .flatMap(id ⇒ IO(println(s"User $id now has birthdate $newBD")))
      case UpdateSex(id, newSex) ⇒
        Client
          .updateSex(id, newSex)
          .withUniqueGeneratedKeys[Int]("id")
          .transact(xa)
          .flatMap(id ⇒ IO(println(s"User $id now has sex $newSex")))
      case DeleteClient(id) ⇒
        Client
          .delete(id)
          .withUniqueGeneratedKeys[Int]("id")
          .transact(xa)
          .flatMap(id ⇒ IO(println(s"User $id has been deleted")))
      case FetchAllClients ⇒
        Client.fetchAll
          .to[List]
          .transact(xa)
          .flatMap(
            list ⇒
              IO(
                println(
                  ("List of current clients:" ::
                    list.map(_.toString)).mkString("\n")
                )
            )
          )
      case _ ⇒ IO(println("Command unknown or not implemented yet"))
    }
  }
}
