package crudkhalnaya.model

import java.time.LocalDateTime

import doobie.util.query.Query0
import doobie.util.update.Update0
import doobie._
import doobie.implicits._
import doobie.implicits.javatime._
import doobie.h2.implicits._
import doobie.h2._
import cats._
import cats.data._
import cats.implicits._

case class Order(id: Int,
                 placed: LocalDateTime,
                 clientId: Int,
                 deliveryAddress: String)

case class OrderView(id: Int,
                     client: String,
                     placed: LocalDateTime,
                     deliveryAddress: String,
                     content: Option[List[(String, Int, Double)]],
                     total: Option[Double]) {
  override def toString: String =
    s"Order #$id\nPlaced by $client at $placed\nDeliver to: $deliveryAddress\n" + {
      content match {
        case None ⇒ "Empty bucket"
        case Some(value) ⇒
          "Order bucket:\n" +
            value
              .mapWithIndex {
                case ((str, i, d), idx) ⇒ s"${idx + 1}: $str, $i x $d"
              }
              .mkString("\n")
      }
    } +
      s"\nTotal sum: ${total.getOrElse(0.0)}"
}

object Order {

  private case class dbOrderView(id: Int,
                                 client: String,
                                 placed: LocalDateTime,
                                 deliveryAddress: String,
                                 cNames: Option[List[String]],
                                 cAmounts: Option[List[Int]],
                                 cPrices: Option[List[Double]],
                                 total: Option[Double])

  private def flatten[A, B, C](t: ((A, B), C)): (A, B, C) =
    (t._1._1, t._1._2, t._2)

  private def dbOrderViewToOrderView(entry: dbOrderView): OrderView = {
    val dbOrderView(id, name, dt, delTo, nms, amts, prcs, total) = entry
    OrderView(id, name, dt, delTo, for {
      someNms ← nms
      someAmts ← amts
      somePrcs ← prcs
    } yield someNms.zip(someAmts).zip(somePrcs).map(flatten), total)
  }
  def create(newOrder: Order): Update0 = {
    val Order(_, placed, clientId, deliverTo) = newOrder
    sql"""INSERT INTO KHINKA."Orders"(PLACED,CLIENT,DELIVERTO) VALUES ($placed,$clientId,$deliverTo)""".update
  }
  def fetch(id: Int): Query0[OrderView] =
    sql"""select o.id,u.name,o.placed,o.deliverTo,ARRAY_AGG(i.name), ARRAY_AGG(b.amount), ARRAY_AGG(i.price),SUM(b.amount*i.price) from KHINKA."Orders" as o
         |left join KHINKA."Buckets" as b
         |on o.id = b.id
         |left join KHINKA."Items" as i 
         |on b.item = i.id
         |left join KHINKA."Users" as u
         |on u.id = o.client
         |where o.id = $id
         |GROUP BY o.id""".stripMargin
      .query[dbOrderView]
      .map(dbOrderViewToOrderView)
  def fetchForUser(userId: Int): Query0[OrderView] =
    sql"""select o.id,u.name,o.placed,o.deliverTo,ARRAY_AGG(i.name), ARRAY_AGG(b.amount), ARRAY_AGG(i.price),SUM(b.amount*i.price) from KHINKA."Orders" as o
         |left join KHINKA."Buckets" as b
         |on o.id = b.id
         |left join KHINKA."Items" as i
         |on b.item = i.id
         |left join KHINKA."Users" as u
         |on u.id = o.client
         |WHERE u.id = $userId
         |GROUP BY o.id""".stripMargin
      .query[dbOrderView]
      .map(dbOrderViewToOrderView)
  def fetchAll: Query0[OrderView] =
    sql"""select o.id,u.name,o.placed,o.deliverTo,ARRAY_AGG(i.name), ARRAY_AGG(b.amount), ARRAY_AGG(i.price),SUM(b.amount*i.price) from KHINKA."Orders" as o
         |left join KHINKA."Buckets" as b
         |on o.id = b.id
         |left join KHINKA."Items" as i 
         |on b.item = i.id
         |left join KHINKA."Users" as u
         |on u.id = o.client
         |GROUP BY o.id""".stripMargin
      .query[dbOrderView]
      .map(dbOrderViewToOrderView)
  def delete(id: Int): Update0 =
    sql"""DELETE FROM KHINKA."Orders" WHERE ID = $id""".update
  def addItem(orderId: Int, itemId: Int, amount: Int): Update0 =
    sql"""INSERT INTO KHINKA."Buckets" values ($orderId, $itemId, $amount)""".update
  def changeAmount(orderId: Int, itemId: Int, amount: Int): Update0 =
    sql"""UPDATE KHINKA."Buckets" SET AMOUNT = $amount where (ID,ITEM)=($orderId,$itemId)  """.update
  def removeItem(orderId: Int, itemId: Int): Update0 =
    sql""" DELETE FROM KHINKA."Buckets" WHERE (ID,ITEM)=($orderId,$itemId) """.update

  def fetchBucketEntry(ordId: Int, itemId: Int): Query0[(Int, Int, Int)] =
    sql"""  SELECT * FROM KHINKA."Buckets" WHERE (ID,ITEM) = ($ordId, $itemId) """
      .query[(Int, Int, Int)]
}
