package crudkhalnaya.model

import java.time.LocalDate

case class Client(id: Int,
                  name: String,
                  address: String,
                  birthdate: LocalDate,
                  gender: Boolean)
