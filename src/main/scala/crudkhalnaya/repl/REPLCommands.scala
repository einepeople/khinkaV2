package crudkhalnaya.repl

import java.time.LocalDate

import crudkhalnaya.model.Client

object REPLCommands {

  sealed trait Command

  case object Exit extends Command

  sealed trait ClientCommand extends Command

  case class AddClient(client: Client) extends ClientCommand

  case class FetchClient(id: Int) extends ClientCommand

  case class UpdateName(id: Int, newName: String) extends ClientCommand

  case class UpdateAddress(id: Int, newAddress: String) extends ClientCommand

  case class UpdateBirthdate(id: Int, newBD: LocalDate) extends ClientCommand

  case class UpdateSex(id: Int, newSex: Boolean) extends ClientCommand

  case class DeleteClient(id: Int) extends ClientCommand

}
