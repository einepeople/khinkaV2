package crudkhalnaya.model

case class Item(id: Int,
                name: String,
                description: String,
                price: Double,
                amount: Int)

object Item {}
