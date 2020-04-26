package crudkhalnaya.model

import doobie.util.query.Query0
import doobie.util.update.Update0
import doobie._
import doobie.implicits._
import doobie.implicits.javatime._
import cats._, cats.data._, cats.implicits._

case class Item(id: Int,
                name: String,
                description: String,
                price: Double,
                amount: Int) {
  override def toString: String =
    s"Item $id: $name\n" +
      s"Description: $description\n" +
      s"Price: $price; $amount at stockhelp"
}

object Item {
  def create(newItem: Item): Update0 = {
    val Item(_, n, d, p, a) = newItem
    sql"""INSERT INTO KHINKA."Items"(name, description, price, amount) VALUES ($n,$d,$p,$a)""".update
  }
  def fetch(id: Int): Query0[Item] = {
    sql"""SELECT * FROM KHINKA."Items" WHERE ID = $id""".query[Item]
  }
  def fetchAll: Query0[Item] = {
    sql"""SELECT * FROM KHINKA."Items"""".query[Item]
  }
  def delete(id: Int): Update0 =
    sql"""DELETE FROM KHINKA."Items" WHERE ID = $id""".update

  def updateName(id: Int, newName: String): Update0 = {
    sql"""UPDATE KHINKA."Items" SET name=$newName where id = $id""".update
  }
  def updateDescription(id: Int, newDescription: String): Update0 = {
    sql"""UPDATE KHINKA."Items" SET description=$newDescription where id = $id""".update
  }
  def updatePrice(id: Int, newPrice: Double): Update0 = {
    sql"""UPDATE KHINKA."Items" SET price=$newPrice where id = $id""".update
  }
  def updateAmount(id: Int, newAmt: Int): Update0 = {
    sql"""UPDATE KHINKA."Items" SET amount=$newAmt where id = $id""".update
  }
}
