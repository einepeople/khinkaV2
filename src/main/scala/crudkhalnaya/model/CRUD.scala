package crudkhalnaya.model

import java.time.LocalDate

import doobie._
import doobie.implicits._
import doobie.implicits.javatime._
import cats._, cats.data._, cats.implicits._

import doobie.util.query.Query0
import doobie.util.update.Update0

trait CRUD[T, ID] {
  def create(obj: T): Update0
  def read(id: ID): Query0[T]
  def update(id: ID, newVals: T): Update0
  def delete(id: ID): Update0
}

case object ClientCRUD extends CRUD[Client, Int] {
  override def create(obj: Client): Update0 = {
    val Client(_, name, address, bd, sex) = obj
    sql"INSERT INTO KHINKA.'Users'(name, address, birthdate, sex) VALUES ($name, $address, $bd, $sex)".update
  }

  override def read(id: Int): Query0[Client] =
    sql"SELECT * FROM KHINKA.'Users' WHERE ID = $id".query[Client]

  override def update(id: Int, newVals: Client): Update0 = {
    val Client(_, name, address, bd, sex) = newVals
    sql"UPDATE KHINKA.'Users' SET NAME=$name, ADDRESS=$address, BIRTHDATE=$bd, SEX=$sex WHERE ID = $id".update
  }

  override def delete(id: Int): Update0 =
    sql"DELETE FROM KHINKA.'Users' WHERE ID = $id".update
}
