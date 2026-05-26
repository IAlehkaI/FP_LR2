package vending.interpreters

import vending.algebras.VendingMachine
import vending.domain.VendingMachineState
import vending.monads.VendingEff

import vending.monads.Monad.syntax._
import vending.monads.VendingEff.vendingEffMonad

class LiveVendingMachine extends VendingMachine[VendingEff] {

  def insertCoin(coin: Int): VendingEff[Unit] = {
    for {
      cfg <- VendingEff.askR
      isAccepted = cfg.validCoins.contains(coin)
      _ <- if (isAccepted) {
        for {
          _ <- VendingEff.tellW(s"Coin $coin accepted.")
          _ <- VendingEff.modifyS { s =>
            s.copy(
              insertedAmount = s.insertedAmount + coin,
              coinsTill = s.coinsTill.updated(coin, s.coinsTill.getOrElse(coin, 0) + 1),
              currentSessionCoins = coin :: s.currentSessionCoins
            )
          }
        } yield ()
      } else {
        VendingEff.tellW(s"Coin $coin is not a valid denomination and was returned.")
      }
    } yield ()
  }

  def selectProduct(product: String, studentIdOpt: Option[String]): VendingEff[Either[String, Int]] = {
    for {
      cfg <- VendingEff.askR
      state <- VendingEff.getS
      isStudent = studentIdOpt.isDefined

      basePriceOpt = cfg.prices.get(product)
      effectivePrice = basePriceOpt.map { price =>
        if (isStudent) (price * (100 - cfg.discountPercent) / 100.0).toInt else price
      }

      result <- effectivePrice match {
        case Some(price) =>
          val discountMsg = if (isStudent) " (with student discount)" else ""
          for {
            _ <- VendingEff.tellW(s"Attempting to purchase $product$discountMsg for $price. Inserted: ${state.insertedAmount}.")
            res <- if (state.insertedAmount < price) {
              val err = s"Insufficient funds. Inserted: ${state.insertedAmount}, Price: $price."
              VendingEff.tellW(s"Purchase failed: $err").map(_ => Left(err))
            } else if (state.inventory.getOrElse(product, 0) <= 0) {
              val err = s"Product $product is out of stock."
              VendingEff.tellW(s"Purchase failed: $err").map(_ => Left(err))
            } else {
              val change = state.insertedAmount - price
              for {
                _ <- VendingEff.modifyS(s => updateStateOnSuccess(s, product, price, studentIdOpt))
                _ <- VendingEff.tellW(s"Purchase successful! Change returned: $change. Enjoy your $product!")
              } yield Right(change)
            }
          } yield res

        case None =>
          val err = s"Product '$product' not found in the machine."
          VendingEff.tellW(s"Purchase failed: $err").map(_ => Left(err))
      }
    } yield result
  }

  def cancelPurchase(): VendingEff[(Int, List[Int])] = {
    for {
      state <- VendingEff.getS
      refundAmount = state.insertedAmount
      coinsToReturn = state.currentSessionCoins

      updatedTill = coinsToReturn.foldLeft(state.coinsTill) { (till, coin) =>
        val currentCount = till.getOrElse(coin, 0)
        val newCount = if (currentCount > 0) currentCount - 1 else 0
        till.updated(coin, newCount)
      }

      _ <- VendingEff.modifyS { s =>
        s.copy(
          insertedAmount = 0,
          coinsTill = updatedTill,
          currentSessionCoins = Nil
        )
      }
      coinsStr = if (coinsToReturn.isEmpty) "None" else coinsToReturn.mkString(", ")
      _ <- VendingEff.tellW(s"Refund issued: $refundAmount coins returned. Physically removed from till: [$coinsStr]")
    } yield (refundAmount, coinsToReturn)
  }

  def refillProduct(product: String, amount: Int): VendingEff[Unit] = {
    for {
      _ <- VendingEff.modifyS { s =>
        val currentStock = s.inventory.getOrElse(product, 0)
        s.copy(inventory = s.inventory.updated(product, currentStock + amount))
      }
      _ <- VendingEff.tellW(s"Admin refilled $amount units of $product.")
    } yield ()
  }

  def nextDay(): VendingEff[Int] = {
    for {
      state <- VendingEff.getS
      next = state.dayCounter + 1
      _ <- VendingEff.modifyS(_.copy(dayCounter = next, usedStudentIds = Set.empty))
      _ <- VendingEff.tellW(s"Advanced to Day $next. Student discounts reset.")
    } yield next
  }

  def getRawState(): VendingEff[VendingMachineState] = VendingEff.getS

  def getLogs(): VendingEff[List[String]] = VendingEff.getW

  // Вспомогательный метод для обновления состояния при успешной покупке
  private def updateStateOnSuccess(
                                    state: VendingMachineState,
                                    product: String,
                                    price: Int,
                                    studentIdOpt: Option[String]
                                  ): VendingMachineState = {
    val newUsedIds = studentIdOpt
      .map(id => state.usedStudentIds + id)
      .getOrElse(state.usedStudentIds)

    state.copy(
      inventory = state.inventory.updated(product, state.inventory(product) - 1),
      insertedAmount = 0,
      revenue = state.revenue + price,
      usedStudentIds = newUsedIds,
      currentSessionCoins = Nil
    )
  }
}