package vending.domain

case class VendingMachineState(
                                inventory: Map[String, Int],
                                insertedAmount: Int,
                                revenue: Int,
                                coinsTill: Map[Int, Int],
                                usedStudentIds: Set[String],
                                dayCounter: Int,
                                currentSessionCoins: List[Int]
                              )