package crudkhalnaya.repl

import cats.effect.IO
import crudkhalnaya.repl.Commands._
import doobie._
import doobie.implicits._
import cats.data._
import cats.effect._
import cats.implicits._
import crudkhalnaya.errors.{CRUDError, OrderNotFound, UserNotFound}
import fs2.Stream
import crudkhalnaya.model.{Client, Item, Order}
import crudkhalnaya.utils.Utils.EitherErr
import doobie.Transactor
import doobie.util.compat.FactoryCompat

object Executor {

  val availableCommands = List(
    "exit",
    "help",
    "add new client $name, $sex, born YYYY-MM-DD, address $addr",
    "show client $Id",
    "show all clients",
    "delete client $Id",
    "for client $Id set name to $newName",
    "for client $Id set address to $newAddress",
    "for client $Id set birthdate to YYYY-MM-DD",
    "for client $Id set sex to $newsex",
    "add new item $name, price $price, initial amount $iam, description $descr",
    "show item $id",
    "show all items",
    "delete item $id",
    "for item $maybeId set name to $newName",
    "for item $maybeId set description to $descr",
    "for item $maybeId set price to $maybePrice",
    "for item $maybeId set amount to $maybeAmt",
    "Make new order by client $maybeId, deliver to $place",
    "show order $maybeId",
    "delete order $maybeId",
    "show all orders",
    "show orders for client $maybeId",
    "Add item $mbItem to order $maybeOrd",
    "Add $N units of item $mbItem to order $ord",
    "Remove item $mbItem for order $maybeOrd",
    "Change number of items $mbItem in order $mbOrd to $mbAmt"
  )

  def checkForUser(id: Int, xa: Transactor[IO]): IO[EitherErr[Int]] = {
    Client
      .fetch(id)
      .unique
      .attempt
      .transact(xa)
      .flatMap {
        case Left(_) ⇒ IO(Left(UserNotFound))
        case Right(value) ⇒ IO(Right(value.id))
      }
  }

  def checkForOrder(id: Int, xa: Transactor[IO]): IO[EitherErr[Int]] = {
    Order
      .fetch(id)
      .unique
      .attempt
      .transact(xa)
      .flatMap {
        case Left(_) ⇒ IO(Left(OrderNotFound))
        case Right(value) ⇒ IO(Right(value.id))
      }
  }

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
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(s"Client $id not found"))
            case Right(x) ⇒ IO(println(x.toString))
          }
      case UpdateClientName(id, newName) ⇒
        Client
          .updateName(id, newName)
          .withUniqueGeneratedKeys[Int]("id")
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(s"Client $id not found"))
            case Right(x) ⇒ IO(println(s"Client $x now has name $newName"))
          }
      case UpdateAddress(id, newAddress) ⇒
        Client
          .updateAddress(id, newAddress)
          .withUniqueGeneratedKeys[Int]("id")
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(s"Client $id not found"))
            case Right(x) ⇒
              IO(println(s"Client $x now has address $newAddress"))
          }
      case UpdateBirthdate(id, newBD) ⇒
        Client
          .updateAddress(id, newBD)
          .withUniqueGeneratedKeys[Int]("id")
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(s"Client $id not found"))
            case Right(x) ⇒
              IO(println(s"Client $x now has birthdate $newBD"))
          }
      case UpdateSex(id, newSex) ⇒
        Client
          .updateSex(id, newSex)
          .withUniqueGeneratedKeys[Int]("id")
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(s"Client $id not found"))
            case Right(x) ⇒
              val sex = if (newSex) "male" else "female"
              IO(println(s"Client $x is now $sex"))
          }
      case DeleteClient(id) ⇒
        Client
          .delete(id)
          .withUniqueGeneratedKeys[Int]("id")
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(s"No such client"))
            case Right(x) ⇒
              IO(println(s"Client $x deleted"))
          }
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
      case AddItem(i) ⇒
        Item
          .create(i)
          .withUniqueGeneratedKeys[Int]("id")
          .transact(xa)
          .flatMap(id ⇒ IO(println(s"Created new Item with id $id")))
      case DeleteItem(id) ⇒
        Item
          .delete(id)
          .withUniqueGeneratedKeys[Int]("id")
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(s"No such item"))
            case Right(x) ⇒
              IO(println(s"Item $x deleted"))
          }
      case FetchItem(id) ⇒
        Item
          .fetch(id)
          .unique
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(s"Item $id not found"))
            case Right(x) ⇒ IO(println(x.toString))
          }
      case FetchAllItems ⇒
        Item.fetchAll
          .to[List]
          .transact(xa)
          .flatMap(
            list ⇒
              IO(
                println(
                  ("List of current Items:" ::
                    list.map(_.toString)).mkString("\n\n")
                )
            )
          )
      case UpdateItemName(id, newName) ⇒
        Item
          .updateName(id, newName)
          .withUniqueGeneratedKeys[Int]("id")
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(s"Item $id not found"))
            case Right(x) ⇒
              IO(println(s"Item $x now has name $newName"))
          }
      case UpdateDescription(id, newDescr) ⇒
        Item
          .updateDescription(id, newDescr)
          .withUniqueGeneratedKeys[Int]("id")
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(s"Item $id not found"))
            case Right(x) ⇒
              IO(println(s"Item $x now has description $newDescr"))
          }
      case UpdatePrice(id, newPrice) ⇒
        Item
          .updatePrice(id, newPrice)
          .withUniqueGeneratedKeys[Int]("id")
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(s"Item $id not found"))
            case Right(x) ⇒
              IO(println(s"Item $x now costs $newPrice"))
          }
      case UpdateAmount(id, newAmt) ⇒
        Item
          .updateAmount(id, newAmt)
          .withUniqueGeneratedKeys[Int]("id")
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(s"Item $id not found"))
            case Right(x) ⇒
              IO(println(s"We now have $newAmt of item $x"))
          }
      case AddOrder(order) ⇒
        val res = for {
          _ ← EitherT(checkForUser(order.clientId, xa))
          resId ← EitherT.right(
            Order
              .create(order)
              .withUniqueGeneratedKeys[Int]("id")
              .transact(xa)
          )
        } yield resId
        res.value.flatMap {
          case Left(err) ⇒ IO(println(err.toString))
          case Right(value) ⇒ IO(println(s"Order $value has been created"))
        }
      //case FetchOrder
      case _ ⇒ IO(println("Command unknown or not implemented yet"))
    }
  }
}
