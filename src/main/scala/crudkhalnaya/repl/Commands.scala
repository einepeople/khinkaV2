package crudkhalnaya.repl

import java.time.LocalDate

import crudkhalnaya.model.{Client, Item}

object Commands {

  sealed trait Command
  case object Exit extends Command
  case object Help extends Command
  case class AddClient(client: Client) extends Command
  case class FetchClient(id: Int) extends Command
  case class DeleteClient(id: Int) extends Command
  case object FetchAllClients extends Command
  case class UpdateClientName(id: Int, newName: String) extends Command
  case class UpdateAddress(id: Int, newAddress: String) extends Command
  case class UpdateBirthdate(id: Int, newBD: LocalDate) extends Command
  case class UpdateSex(id: Int, newSex: Boolean) extends Command

  case class AddItem(i: Item) extends Command
  case class FetchItem(id: Int) extends Command
  case class DeleteItem(id: Int) extends Command
  case object FetchAllItems extends Command
  case class UpdateItemName(id: Int, newName: String) extends Command
  case class UpdateDescription(id: Int, newDescr: String) extends Command
  case class UpdatePrice(id: Int, newPrice: Double) extends Command
  case class UpdateAmount(id: Int, newAmt: Int) extends Command
}
