package crudkhalnaya.model

import java.time.LocalDateTime

case class Order(id: Int,
                 placed: LocalDateTime,
                 cid: Int,
                 deliveryAddress: String,
                 content: List[(Int, Int)])

object Order {}
