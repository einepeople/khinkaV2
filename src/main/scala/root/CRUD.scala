package root

import java.util.Date

// type EitherC[T] = Either[CRUDError, T]
//trait ServerContext
//trait CRUD[T, ID] {
//  def create[P]: (P => EitherC[ID])
//  def read: (ID => EitherC[T])
//  def update: ((ID, T) => EitherC[T])
//  def delete: (ID => EitherC[T])
//}

case class Client(id: Int,
                  name: String,
                  address: String,
                  birtdate: Date,
                  gender: Boolean)
case class Item(id: Int,
                name: String,
                description: String,
                price: Double,
                amount: Int)

case class Order(id: Int, placed: Date, cid: Int, content: List[(Int, Int)])
//object ClientCRUD extends CRUD[Int, Client] {
//  override def create[P]: P => EitherC[Client] =
//    (name: String, address: String) =>
//      Client(1 /*some int from ServerCtx*/, name, address)
//
//  override def read: Client => EitherC[Int] = ???
//
//  override def update: (Client, Int) => EitherC[Int] = ???
//
//  override def delete: Client => EitherC[Int] = ???
//}
