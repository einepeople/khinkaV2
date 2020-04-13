package root

import java.time.{LocalDate, LocalDateTime}

case class Client(id: Int,
                  name: String,
                  address: String,
                  birtdate: LocalDate,
                  gender: Boolean)
case class Item(id: Int,
                name: String,
                description: String,
                price: Double,
                amount: Int)

case class Order(id: Int,
                 placed: LocalDateTime,
                 cid: Int,
                 deliveryAddress: String,
                 content: List[(Int, Int)])
