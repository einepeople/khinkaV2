package crudkhalnaya.repl

import java.time.LocalDate

import crudkhalnaya.errors.{CommandNotFoundError, ParseError}
import crudkhalnaya.model.{Client, Item, Order}
import crudkhalnaya.repl.Commands._
import crudkhalnaya.utils.Utils.EitherErr

import scala.util.{Failure, Success, Try}

object Parser {
  def parseSex(input: String): EitherErr[Boolean] = {
    input.toLowerCase match {
      case "male" ⇒ Right(true)
      case "female" ⇒ Right(false)
      case _ ⇒ Left(ParseError(s"Cannot parse sex from $input"))
    }
  }

  def parseBirthdate(input: String): EitherErr[LocalDate] = {
    val t = Try[LocalDate](LocalDate.parse(input))
    t match {
      case Success(value) ⇒ Right(value)
      case Failure(_) ⇒ Left(ParseError(s"Cannot parse date from $input"))
    }
  }

  def parseInt(input: String): EitherErr[Int] =
    Try[Int](input.toInt) match {
      case Failure(_) ⇒ Left(ParseError(s"Cannot parse int from $input"))
      case Success(value) ⇒ Right(value)
    }

  def parseDouble(input: String): EitherErr[Double] =
    Try[Double](input.toDouble) match {
      case Failure(_) ⇒ Left(ParseError(s"Cannot parse double from $input"))
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
      case s"show client $maybeId" ⇒
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
        } yield UpdateClientName(checkId, checkName)
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

      case s"add new item $name, price $price, initial amount $iam, description $descr" ⇒
        for {
          nbname ← Either.cond(
            !name.isBlank,
            name,
            ParseError("Name cannot be blank")
          )
          price ← parseDouble(price)
          nnprice ← Either.cond(
            price >= 0,
            price,
            ParseError("Price cannot be negative")
          )
          amt ← parseInt(iam)
          nnamt ← Either.cond(
            amt >= 0,
            amt,
            ParseError("Amount cannot be negative")
          )
          nbdescr ← Either.cond(
            !descr.isBlank,
            descr,
            ParseError("Description cannot be blank")
          )
        } yield AddItem(Item(-1, nbname, nbdescr, nnprice, nnamt))
      case s"delete item $id" ⇒
        for {
          checkId ← parseInt(id)
        } yield DeleteItem(checkId)
      case s"show item $id" ⇒
        for {
          checkId ← parseInt(id)
        } yield FetchItem(checkId)
      case "show all items" ⇒
        Right(FetchAllItems)
      case s"for item $maybeId set name to $newName" ⇒
        for {
          checkId ← parseInt(maybeId)
          checkName ← Either.cond(
            !newName.isBlank,
            newName,
            ParseError("Name cannot be blank")
          )
        } yield UpdateItemName(checkId, checkName)
      case s"for item $maybeId set description to $descr" ⇒
        for {
          checkId ← parseInt(maybeId)
          checkDescr ← Either.cond(
            !descr.isBlank,
            descr,
            ParseError("Name cannot be blank")
          )
        } yield UpdateDescription(checkId, checkDescr)
      case s"for item $maybeId set price to $maybePrice" ⇒
        for {
          checkId ← parseInt(maybeId)
          price ← parseDouble(maybePrice)
          nnprice ← Either.cond(
            price >= 0,
            price,
            ParseError("Price cannot be negative")
          )
        } yield UpdatePrice(checkId, nnprice)
      case s"for item $maybeId set amount to $maybeAmt" ⇒
        for {
          checkId ← parseInt(maybeId)
          amt ← parseInt(maybeAmt)
          nnamt ← Either.cond(
            amt >= 0,
            amt,
            ParseError("Amount cannot be negative")
          )
        } yield UpdateAmount(checkId, nnamt)

      case s"make new order by client $maybeId, deliver to $place" ⇒
        for {
          checkId ← parseInt(maybeId)
          checkPlace ← Either.cond(
            !place.isBlank,
            place,
            ParseError("Delivery destination cannot be blank")
          )
        } yield
          AddOrder(
            Order(-1, java.time.LocalDateTime.now(), checkId, checkPlace)
          )
      case s"show order $maybeId" ⇒
        for {
          checkId ← parseInt(maybeId)
        } yield FetchOrder(checkId)
      case s"delete order $maybeId" ⇒
        for {
          checkId ← parseInt(maybeId)
        } yield DeleteOrder(checkId)
      case s"show all orders" ⇒
        Right(FetchAllOrders)
      case s"show orders for client $maybeId" ⇒
        for {
          checkId ← parseInt(maybeId)
        } yield FetchOrdersForClient(checkId)
      case s"add $mbAmt items $mbItem to order $ord" ⇒
        for {
          amt ← parseInt(mbAmt)
          nnamt ← Either.cond(
            amt >= 0,
            amt,
            ParseError("Amount cannot be negative")
          )
          itemId ← parseInt(mbItem)
          ordId ← parseInt(ord)
        } yield AddItemToOrder(itemId, ordId, nnamt)
      case s"add item $mbItem to order $maybeOrd" ⇒
        for {
          itemId ← parseInt(mbItem)
          ordId ← parseInt(maybeOrd)
        } yield AddItemToOrder(itemId, ordId, 1)
      case s"remove item $mbItem from order $maybeOrd" ⇒
        for {
          itemId ← parseInt(mbItem)
          ordId ← parseInt(maybeOrd)
        } yield RemoveItemFromOrder(itemId, ordId)
      case s"change number of items $mbItem in order $mbOrd to $mbAmt" ⇒
        for {
          itemId ← parseInt(mbItem)
          ordId ← parseInt(mbOrd)
          amt ← parseInt(mbAmt)
          nnamt ← Either.cond(
            amt >= 0,
            amt,
            ParseError("Amount cannot be negative")
          )
        } yield
          if (nnamt > 0) ChangeItemAmountInOrder(itemId, ordId, nnamt)
          else RemoveItemFromOrder(itemId, ordId)
      case x ⇒ Left(CommandNotFoundError(s"Unknown command $x"))
    }
  }
}
