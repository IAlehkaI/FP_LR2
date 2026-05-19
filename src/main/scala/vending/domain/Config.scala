package vending.domain

case class Config(
                   prices: Map[String, Int],
                   validCoins: Set[Int],
                   maxInserted: Int,
                   validStudentIds: Set[String],
                   discountPercent: Int
                 )