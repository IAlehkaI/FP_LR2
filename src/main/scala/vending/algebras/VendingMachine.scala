package vending.algebras

import vending.domain.VendingMachineState

trait VendingMachine[F[_]] {
  def insertCoin(coin: Int): F[Unit]
  def selectProduct(product: String, studentIdOpt: Option[String]): F[Either[String, Int]]
  def cancelPurchase(): F[(Int, List[Int])]
  def refillProduct(product: String, amount: Int): F[Unit]
  def nextDay(): F[Int]
  def getRawState(): F[VendingMachineState]
  def getLogs(): F[List[String]]
}