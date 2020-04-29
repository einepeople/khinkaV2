package crudkhalnaya
import java.time.LocalDate

import crudkhalnaya.errors.{GenericCRUDError, ParseError}
import crudkhalnaya.model.{Client, Item}
import crudkhalnaya.repl.Commands.{
  AddClient,
  AddItem,
  ChangeItemAmountInOrder,
  RemoveItemFromOrder,
  UpdatePrice
}
import crudkhalnaya.repl.Parser
import crudkhalnaya.utils.Utils.EitherErr
import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {
  def dropLeftErrs[A](a: EitherErr[A]): EitherErr[A] = a match {
    case Left(_) ⇒
      Left(GenericCRUDError) // kostyl for dropping error messages
    case Right(value) ⇒ Right(value)
  }
  "parseSex" should "conform to laws of biology" in {
    List(
      "female",
      "male",
      "genderqueer",
      "henderfluid helisexual",
      "9M120M 'Ataka-M' Anti-Tank Guided Missile"
    ).map(Parser.parseSex)
      .map(dropLeftErrs)
      .zip(
        List(
          Right(false),
          Right(true),
          Left(GenericCRUDError),
          Left(GenericCRUDError),
          Left(GenericCRUDError)
        )
      )
      .foreach(x ⇒ x._1 shouldEqual x._2)
  }
  "parseBirthdate" should "correctly parse well-formed date and reject ill-formed one" in {
    List(
      "2012-11-10",
      "1990-01-30",
      "2001/7/6",
      "2012.12.12",
      "9 of Januray 2020",
      "shreck"
    ).map(Parser.parseBirthdate)
      .map(dropLeftErrs)
      .zip(
        List(
          Right(LocalDate.parse("2012-11-10")),
          Right(LocalDate.parse("1990-01-30")),
          Left(GenericCRUDError),
          Left(GenericCRUDError),
          Left(GenericCRUDError),
          Left(GenericCRUDError)
        )
      )
      .foreach(x ⇒ x._1 shouldEqual x._2)
  }
  "parseInt" should "correctly parse a well-formed string or report an error" in {
    List("1", "100500", "-1919", "thousand", "kok", "143870asdf")
      .map(Parser.parseInt)
      .map(dropLeftErrs)
      .zip(
        List(
          Right(1),
          Right(100500),
          Right(-1919),
          Left(GenericCRUDError),
          Left(GenericCRUDError),
          Left(GenericCRUDError)
        )
      )
      .foreach(x ⇒ x._1 shouldEqual x._2)
  }
  "parseDouble" should "correctly parse a well-formed string or report an error" in {
    List(
      "10.3",
      "11.0",
      "1488.228",
      "three hundred bucks",
      "two number nines, a  number nine large...",
      "2012-7-10"
    ).map(Parser.parseDouble)
      .map(dropLeftErrs)
      .zip(
        List(
          Right(10.3),
          Right(11.0),
          Right(1488.228),
          Left(GenericCRUDError),
          Left(GenericCRUDError),
          Left(GenericCRUDError)
        )
      )
      .foreach(x ⇒ x._1 shouldEqual x._2)
  }
  "parseCommand" should "correctly produce Command case lasses or errors" in {
    val qrs = List(
      "add new client DOODKA, male, born 1999-01-10, address Ulitsa Pushkina",
      "add new item Turtle, price 109.9, initial amount 149, description Turto-proshutto",
      "add new item Turtle, price 109.9, initial amount -1, description Turto-proshutto",
      "for item 103 set price to -90",
      "for item 103 set price to 5.0",
      "remove item 4 from order 65",
      "change number of items 12 in order 34 to 4",
      "change number of items 12 in order 30 to 0",
      "change number of items 12 in order 1 to -2",
    )
    val res = List(
      Right(
        AddClient(
          Client(
            -1,
            "DOODKA",
            "Ulitsa Pushkina",
            LocalDate.parse("1999-01-10"),
            sex = true
          )
        )
      ),
      Right(AddItem(Item(-1, "Turtle", "Turto-proshutto", 109.9, 149))),
      Left(GenericCRUDError),
      Left(GenericCRUDError),
      Right(UpdatePrice(103, 5.0)),
      Right(RemoveItemFromOrder(4, 65)),
      Right(ChangeItemAmountInOrder(12, 34, 4)),
      Right(RemoveItemFromOrder(12, 30)),
      Left(GenericCRUDError)
    )
    qrs.map(Parser.parseCommand).map(dropLeftErrs).zip(res).foreach { t ⇒
      t._1 shouldEqual t._2
    }
  }
}
