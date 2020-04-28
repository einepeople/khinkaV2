package crudkhalnaya.repl

import cats.effect.IO
import crudkhalnaya.repl.Commands._
import doobie._
import doobie.implicits._
import cats.data._
import cats._
import cats.effect._
import cats.implicits._
import crudkhalnaya.errors.{
  CRUDError,
  ItemInBucketNotFound,
  OrderNotFound,
  UserNotFound
}
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
        case Left(_) ⇒ IO(Left(UserNotFound(s"User $id not found")))
        case Right(value) ⇒ IO(Right(value.id))
      }
  }
  def checkForItem(id: Int, xa: Transactor[IO]): IO[EitherErr[Int]] = {
    Item
      .fetch(id)
      .unique
      .attempt
      .transact(xa)
      .flatMap {
        case Left(_) ⇒ IO(Left(UserNotFound(s"Item $id not found")))
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
        case Left(_) ⇒ IO(Left(OrderNotFound(s"Order $id not found")))
        case Right(value) ⇒ IO(Right(value.id))
      }
  }
  def checkItemInOrder(ordId: Int,
                       itemId: Int,
                       xa: Transactor[IO]): IO[EitherErr[(Int, Int, Int)]] = {
    Order
      .fetchBucketEntry(ordId, itemId)
      .unique
      .attempt
      .transact(xa)
      .flatMap {
        case Left(_) ⇒
          IO(Left(ItemInBucketNotFound(s"No item $itemId in $ordId")))
        case Right(value) ⇒ IO(Right(value))
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
          uid ← EitherT(checkForUser(order.clientId, xa))
        } yield uid
        res.value.flatMap {
          case Left(err) ⇒ IO(println(err.toString))
          case Right(_) ⇒
            Order
              .create(order)
              .withUniqueGeneratedKeys[Int]("id")
              .transact(xa)
              .flatMap { id ⇒
                IO(println(s"Order $id has been created"))
              }
        }
      case FetchOrder(id) ⇒
        Order
          .fetch(id)
          .unique
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(OrderNotFound(s"Order $id not found")))
            case Right(value) ⇒ IO(println(value.toString))
          }
      case DeleteOrder(id) ⇒
        Order
          .delete(id)
          .withUniqueGeneratedKeys[Int]("id")
          .attempt
          .transact(xa)
          .flatMap {
            case Left(_) ⇒ IO(println(OrderNotFound(s"Order $id not found")))
            case Right(deletedId) ⇒
              IO(println(s"Order $deletedId has been deleted"))
          }
      case FetchAllOrders ⇒
        Order.fetchAll
          .to[List]
          .transact(xa)
          .flatMap(
            list ⇒
              IO(
                println(
                  ("List of all orders:" ::
                    list.map(_.toString)).mkString("\n\n")
                )
            )
          )
      case FetchOrdersForClient(id: Int) ⇒
        checkForUser(id, xa).flatMap {
          case Left(_) ⇒ IO(println(s"User $id not found"))
          case Right(_) ⇒
            Order
              .fetchForUser(id)
              .to[List]
              .map(_.toString)
              .attempt
              .transact(xa)
              .flatMap {
                case Left(_) ⇒
                  IO(
                    println(
                      s"Error during fetching. Probably no orders exist for user $id"
                    )
                  )
                case Right(value) ⇒ IO(println(value))
              }
        }
      case AddItemToOrder(itemId, orderId, amount) ⇒
        val maybeOldAmount = for {
          _ ← EitherT(checkForOrder(orderId, xa))
          _ ← EitherT(checkForItem(itemId, xa))
          (_, _, oldAmt) ← EitherT(checkItemInOrder(orderId, itemId, xa))
        } yield oldAmt
        maybeOldAmount.value.flatMap {
          case Left(err: OrderNotFound) ⇒ IO(println(err.toString))
          case Left(_: ItemInBucketNotFound) ⇒
            Order
              .addItem(itemId, orderId, amount)
              .withUniqueGeneratedKeys[Int]("id")
              .transact(xa)
              .flatMap(
                id ⇒
                  IO(println(s"Added item ${amount}xItem#$itemId to order $id"))
              )
          case Right(value) ⇒
            Order
              .changeAmount(orderId, itemId, value + amount)
              .withUniqueGeneratedKeys[Int]("id")
              .transact(xa)
              .flatMap(
                id ⇒
                  IO(
                    println(
                      s"Amount of item $itemId in order $id has been changed from $value to ${value + amount}"
                    )
                )
              )

        }
      case RemoveItemFromOrder(itemId, orderId) ⇒
        val correct = for {
          _ ← EitherT(checkForItem(itemId, xa))
          _ ← EitherT(checkForOrder(orderId, xa))
          (_, _, amt) ← EitherT(checkItemInOrder(orderId, itemId, xa))
        } yield amt
        correct.value.flatMap {
          case Left(err) ⇒ IO(println(err.toString))
          case Right(_) ⇒
            Order
              .removeItem(orderId, itemId)
              .withUniqueGeneratedKeys[Int]("id")
              .transact(xa)
              .flatMap(
                id ⇒ IO(println(s"Item $itemId was removed from order $id"))
              )
        }
      case ChangeItemAmountInOrder(itemId, orderId, newAmount) ⇒
        val oldAmount = for {
          _ ← EitherT(checkForOrder(orderId, xa))
          _ ← EitherT(checkForItem(itemId, xa))
          (_, _, oldAmt) ← EitherT(checkItemInOrder(orderId, itemId, xa))
        } yield oldAmt
        oldAmount.value.flatMap {
          case Left(err) ⇒ IO(println(err.toString))
          case Right(value) ⇒
            Order
              .changeAmount(orderId, itemId, newAmount)
              .withUniqueGeneratedKeys[Int]("id")
              .transact(xa)
              .flatMap(
                id ⇒
                  IO(
                    println(
                      s"For order $id amount of item $itemId has been changed from $value to $newAmount"
                    )
                )
              )
        }
      case _ ⇒ IO(println("Command unknown or not implemented yet"))
    }
  }
}
