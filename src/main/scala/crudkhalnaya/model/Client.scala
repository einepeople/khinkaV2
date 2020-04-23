package crudkhalnaya.model

import java.time.LocalDate

import doobie.util.query.Query0
import doobie.util.update.Update0
import doobie._
import doobie.implicits._
import doobie.implicits.javatime._
import cats._, cats.data._, cats.implicits._

case class Client(id: Int,
                  name: String,
                  address: String,
                  birthdate: LocalDate,
                  gender: Boolean)

object Client {
  def create(obj: Client): Update0 = {
    val Client(_, name, address, bd, sex) = obj
    sql"INSERT INTO KHINKA.'Users'(name, address, birthdate, sex) VALUES ($name, $address, $bd, $sex)".update
  }

  def fetch(id: Int): Query0[Client] =
    sql"SELECT * FROM KHINKA.'Users' WHERE ID = $id".query[Client]

  def updateName(id: Int, newName: String): Update0 = {
    sql"UPDATE KHINKA.'Users' SET NAME=$newName WHERE ID = $id".update
  }

  def updateAddress(id: Int, newAddress: String): Update0 = {
    sql"UPDATE KHINKA.'Users' SET ADDRESS=$newAddress WHERE ID = $id".update
  }

  def updateAddress(id: Int, newBD: LocalDate): Update0 = {
    sql"UPDATE KHINKA.'Users' SET BIRTHDATE=$newBD WHERE ID = $id".update
  }
  def updateSex(id: Int, newSex: Boolean): Update0 = {
    sql"UPDATE KHINKA.'Users' SET SEX=$newSex WHERE ID = $id".update
  }

  def delete(id: Int): Update0 =
    sql"DELETE FROM KHINKA.'Users' WHERE ID = $id".update
}
