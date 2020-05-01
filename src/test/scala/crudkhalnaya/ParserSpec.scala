package crudkhalnaya
import java.time.{LocalDate, LocalDateTime, ZoneOffset}

import crudkhalnaya.errors.GenericCRUDError
import crudkhalnaya.model.{Client, Item, Order}
import crudkhalnaya.repl.Commands._
import crudkhalnaya.repl.Parser
import crudkhalnaya.utils.Utils.EitherErr
import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {
  def dropLeftErrs[A](a: EitherErr[A]): EitherErr[A] = a match {
    case Left(_) ⇒
      Left(GenericCRUDError) // kostyl for dropping error messages
    case Right(value) ⇒ Right(value)
  }
  "parseCommand" should "correctly produce Command case classes or errors for Client commands" in {
    val qrs = List(
      "add new client DOODKA, male, born 1999-01-10, address Ul. Pushkina",
      "add new client , male, born 1999-01-10, address TukKuk",
      "add new client Doodka, male, born 1999-01-10, address ",
      "add new client Doodka, kok, born 1999-01-10, address asfd",
      "add new client Doodka, male, born 38-01-10, address zxcvzxcv",
      "show client 19",
      "show client nnetheen",
      "for client 19 set name to SOS",
      "for client 0xPOP set name to POP",
      "for client 19 set name to ",
      "for client 19 set address to SOS",
      "for client 0xPOP set address to POP",
      "for client 19 set address to  ",
      "for client 19 set birthdate to 2012-12-12",
      "for client 0xPOP set birthdate to 2012-12-12",
      "for client 19 set birthdate to 19-11-18",
      "for client 19 set sex to male",
      "for client 19 set sex to female",
      "for client asdf set sex to male",
      "for client 19 set sex to HELICOPTA",
      "delete client 190",
      "delete client Kek Kekovich",
      "show all clients"
    )
    val res = List(
      Right(
        AddClient(
          Client(
            -1,
            "DOODKA",
            "Ul. Pushkina",
            LocalDate.parse("1999-01-10"),
            sex = true
          )
        )
      ),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(FetchClient(19)),
      Left(GenericCRUDError),
      Right(UpdateClientName(19, "SOS")),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(UpdateAddress(19, "SOS")),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(UpdateBirthdate(19, LocalDate.parse("2012-12-12"))),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(UpdateSex(19, newSex = true)),
      Right(UpdateSex(19, newSex = false)),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(DeleteClient(190)),
      Left(GenericCRUDError),
      Right(FetchAllClients)
    )
    qrs.map(Parser.parseCommand(_, 0L)).map(dropLeftErrs).zip(res).foreach { t ⇒
      t._1 shouldEqual t._2
    }
  }
  "parseCommand" should "correctly produce Command case classes or errors for Item commands" in {
    val qrs = List(
      "add new item Sos, price 199.95, initial amount 123, description Kek",
      "add new item  , price 199.95, initial amount 123, description Kek",
      "add new item Sos, price $19, initial amount 123, description Kek",
      "add new item Sos, price 199.95, initial amount -5, description Kek",
      "add new item Sos, price 199.95, initial amount 123, description ",
      "delete item 19",
      "delete item sos",
      "show item 19",
      "show item sos",
      "show all items",
      "for item 14 set name to sos",
      "for item 14 set name to  ",
      "for item asdf set name to sos",
      "for item 14 set description to sos",
      "for item 14 set description to ",
      "for item fdas set description to soi",
      "for item 14 set price to 12.3",
      "for item 14 set price to 01dgg",
      "for item zxcv set price to 1234.4",
      "for item 14 set amount to 1234",
      "for item 14 set amount to -5",
      "for item goog set amount to 2",
    )
    val res = List(
      Right(AddItem(Item(-1, "Sos", "Kek", 199.95, 123))),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(DeleteItem(19)),
      Left(GenericCRUDError),
      Right(FetchItem(19)),
      Left(GenericCRUDError),
      Right(FetchAllItems),
      Right(UpdateItemName(14, "sos")),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(UpdateDescription(14, "sos")),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(UpdatePrice(14, 12.3)),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(UpdateAmount(14, 1234)),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
    )
    qrs.map(Parser.parseCommand(_, 0L)).map(dropLeftErrs).zip(res).foreach { t ⇒
      t._1 shouldEqual t._2
    }
  }
  "parseCommand" should "correctly produce Command case classes or errors for Order commands" in {
    val curTime = 1588329766L
    val qrs = List(
      "make new order by client 1234, deliver to Sosovo",
      "make new order by client Kok Kekov, deliver to Sosovo",
      "make new order by client 1234, deliver to  ",
      "show order 12",
      "show order asfd",
      "delete order 12",
      "delete order asdf",
      "show all orders",
      "show orders for client 12",
      "show orders for client cvx1",
      "add 12 items 55 to order 76",
      "add -1 items 55 to order 76",
      "add asfd items 55 to order 76",
      "add 12 items as2 to order 76",
      "add 12 items 55 to order cos",
      "add item 14 to order 55",
      "add item cxvzxcv to order 55",
      "add item 14 to order saassss",
      "remove item 1234 from order 123",
      "remove item dfghdfgh from order 123",
      "remove item 1234 from order ccccccc",
      "change number of items 123 in order 444 to 12",
      "change number of items 123 in order 444 to 0",
      "change number of items asfdasdf in order 444 to 12",
      "change number of items 123 in order vcvc to 12",
      "change number of items 123 in order 444 to 12kok"
    )
    val res = List(
      Right(
        AddOrder(
          Order(
            -1,
            LocalDateTime
              .ofEpochSecond(curTime, 0, ZoneOffset.UTC),
            1234,
            "Sosovo"
          )
        )
      ),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(FetchOrder(12)),
      Left(GenericCRUDError),
      Right(DeleteOrder(12)),
      Left(GenericCRUDError),
      Right(FetchAllOrders),
      Right(FetchOrdersForClient(12)),
      Left(GenericCRUDError),
      Right(AddItemToOrder(55, 76, 12)),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(AddItemToOrder(14, 55, 1)),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(RemoveItemFromOrder(1234, 123)),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(ChangeItemAmountInOrder(123, 444, 12)),
      Right(RemoveItemFromOrder(123, 444)),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Left(GenericCRUDError)
    )

    qrs
      .map(Parser.parseCommand(_, curTime))
      .map(dropLeftErrs)
      .zip(res)
      .foreach { t ⇒
        t._1 shouldEqual t._2
      }
  }
}
