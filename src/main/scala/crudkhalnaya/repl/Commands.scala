package crudkhalnaya.repl

import java.time.LocalDate

import crudkhalnaya.model.Client

object Commands {

  sealed trait Command

  case object Exit extends Command

  case object Help extends Command

  case class AddClient(client: Client) extends Command

  case class FetchClient(id: Int) extends Command

  case class UpdateName(id: Int, newName: String) extends Command

  case class UpdateAddress(id: Int, newAddress: String) extends Command

  case class UpdateBirthdate(id: Int, newBD: LocalDate) extends Command

  case class UpdateSex(id: Int, newSex: Boolean) extends Command

  case class DeleteClient(id: Int) extends Command

  case object FetchAllClients extends Command

}
