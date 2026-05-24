package vending

import vending.algebras.{Console, VendingMachine}
import vending.monads.Monad
import vending.monads.Monad.syntax._
import vending.domain.{Config, VendingMachineState}
import vending.interpreters.{LiveConsole, LiveVendingMachine}
import vending.monads.VendingEff
import scala.util.Try

object Main {

  val config = Config(
    prices = Map("Cola" -> 100, "Juice" -> 120, "Water" -> 50, "Chips" -> 80),
    validCoins = Set(1, 2, 5, 10, 50, 100, 200),
    maxInserted = 1000,
    validStudentIds = Set("1111", "7777"),
    discountPercent = 20
  )

  val productList = config.prices.keys.toList.sorted

  val initialState = VendingMachineState(
    inventory = Map("Cola" -> 5, "Juice" -> 3, "Water" -> 10, "Chips" -> 2),
    insertedAmount = 0,
    revenue = 0,
    coinsTill = Map(1 -> 0, 2 -> 0, 5 -> 0, 10 -> 0, 50 -> 0, 100 -> 0, 200 -> 0),
    usedStudentIds = Set.empty,
    dayCounter = 1,
    currentSessionCoins = Nil
  )

  def insertAllCoins[F[_]: Monad](coins: List[Int])(implicit VM: VendingMachine[F]): F[Unit] = coins match {
    case Nil => Monad[F].pure(())
    case head :: tail => VM.insertCoin(head).flatMap(_ => insertAllCoins[F](tail))
  }

  def renderMenu[F[_]: Monad](state: VendingMachineState)(implicit C: Console[F]): F[Unit] = {
    C.putStrLn("\n" + "="*45)
      .flatMap(_ => C.putStrLn(f"      VENDING MACHINE Tagless Final (Day: ${state.dayCounter})"))
      .flatMap(_ => C.putStrLn("="*45))
      .flatMap(_ => C.putStrLn(s" Balance: ${state.insertedAmount}"))
      .flatMap(_ => C.putStrLn("-" * 45))
      .flatMap(_ => C.putStrLn(" Products available:"))
      .flatMap { _ =>
        productList.zipWithIndex.foldLeft(Monad[F].pure(())) { case (acc, (p, idx)) =>
          acc.flatMap { _ =>
            val price = config.prices.getOrElse(p, 0)
            val studentPrice = (price * (100 - config.discountPercent) / 100.0).toInt
            val count = state.inventory.getOrElse(p, 0)
            val status = if (count > 0) s"Qty: $count" else "OUT OF STOCK"
            C.putStrLn(f"  ${idx + 1}. $p%-10s : $price%3d coins ($studentPrice with student ID) [$status]")
          }
        }
      }
      .flatMap(_ => C.putStrLn("-" * 45))
      .flatMap(_ => C.putStrLn(" Actions:"))
      .flatMap(_ => C.putStrLn(s"  1 - Insert coin(s) (Valid: ${config.validCoins.toList.sorted.mkString(", ")})"))
      .flatMap(_ => C.putStrLn("  2 - Buy product"))
      .flatMap(_ => C.putStrLn("  3 - Refund / Cancel"))
      .flatMap(_ => C.putStrLn("  4 - Refill product (Admin)"))
      .flatMap(_ => C.putStrLn("  5 - Next Day (Reset student discounts)"))
      .flatMap(_ => C.putStrLn("  6 - Show internal State (Debug)"))
      .flatMap(_ => C.putStrLn("  7 - Show system Log history"))
      .flatMap(_ => C.putStrLn("  0 - Exit"))
      .flatMap(_ => C.putStrLn("="*45))
      .flatMap(_ => C.putStrLn(" Select action: "))
  }

  def askForStudentId[F[_]: Monad](state: VendingMachineState)(implicit C: Console[F]): F[Option[String]] = {
    for {
      _ <- C.putStrLn(" Do you have a student ID for a discount? (Enter ID or press Enter to skip): ")
      input <- C.readLn()
      idStr = input.trim
      result <- if (idStr.isEmpty) {
        Monad[F].pure(None)
      } else if (!config.validStudentIds.contains(idStr)) {
        C.putStrLn(s"\n[!] Invalid student ID ($idStr). Proceeding without discount.").map(_ => None)
      } else if (state.usedStudentIds.contains(idStr)) {
        C.putStrLn(s"\n[!] Student ID ($idStr) has already been used today. Proceeding without discount.").map(_ => None)
      } else {
        Monad[F].pure(Some(idStr))
      }
    } yield result
  }

  def parseProductSelection(input: String): Option[String] = {
    val trimmed = input.trim
    Try(trimmed.toInt).toOption
      .filter(num => num >= 1 && num <= productList.length)
      .map(num => productList(num - 1))
      .orElse(productList.find(_.equalsIgnoreCase(trimmed)))
  }

  def programLoop[F[_]: Monad](implicit C: Console[F], VM: VendingMachine[F]): F[Unit] = {
    for {
      _ <- C.clear()
      state <- VM.getRawState()
      _ <- renderMenu[F](state)
      actionStr <- C.readLn()
      _ <- handleInput[F](actionStr)
    } yield ()
  }

  def handleInput[F[_]: Monad](input: String)(implicit C: Console[F], VM: VendingMachine[F]): F[Unit] = {
    val actions: Map[String, () => F[Unit]] = Map(
      "1" -> (() => actionInsertCoin[F]),
      "2" -> (() => actionBuyProduct[F]),
      "3" -> (() => VM.cancelPurchase().flatMap(_ => programLoop[F])),
      "4" -> (() => actionRefillProduct[F]),
      "5" -> (() => VM.nextDay().flatMap(_ => programLoop[F])),
      "6" -> (() => actionShowState[F]),
      "7" -> (() => actionShowLog[F]),
      "0" -> (() => actionExit[F]),
      (""  , (() => programLoop[F]))
    )

    // Если ввод не найден в мапе, вызываем actionUnknown
    actions.getOrElse(input.trim, () => actionUnknown[F](input.trim))()
  }

  def actionInsertCoin[F[_]: Monad](implicit C: Console[F], VM: VendingMachine[F]): F[Unit] =
    for {
      _ <- C.putStrLn(s" Enter coin(s) separated by space (e.g. '10 50 10'): ")
      inputStr <- C.readLn()
      coins = inputStr.trim.split("\\s+").toList.flatMap(s => Try(s.toInt).toOption)
      _ <- if (coins.isEmpty) C.putStrLn("\n[!] No valid coins entered.") else Monad[F].pure(())
      _ <- insertAllCoins[F](coins)
      _ <- programLoop[F]
    } yield ()

  def actionBuyProduct[F[_]: Monad](implicit C: Console[F], VM: VendingMachine[F]): F[Unit] =
    for {
      _ <- C.putStrLn("\n Select product by Number (1-4) or Name (e.g. Cola):")
      _ <- C.putStrLn(" Selection: ")
      productInput <- C.readLn()
      state <- VM.getRawState()
      _ <- parseProductSelection(productInput).fold(
        C.putStrLn("\n[!] Invalid product selection.")
          .flatMap(_ => C.readLn())
          .flatMap(_ => programLoop[F])
      )(productName =>
        for {
          studentIdOpt <- askForStudentId[F](state)
          _ <- VM.selectProduct(productName, studentIdOpt)
          _ <- programLoop[F]
        } yield ()
      )
    } yield ()

  def actionRefillProduct[F[_]: Monad](implicit C: Console[F], VM: VendingMachine[F]): F[Unit] =
    for {
      _ <- C.putStrLn(" Enter product name to refill (e.g. Cola): ")
      pInput <- C.readLn()
      _ <- C.putStrLn(" Enter amount to add: ")
      aInput <- C.readLn()
      amount = Try(aInput.trim.toInt).getOrElse(0)
      _ <- parseProductSelection(pInput).filter(_ => amount > 0).fold(
        C.putStrLn("\n[!] Invalid product or amount.")
          .flatMap(_ => C.readLn())
          .flatMap(_ => programLoop[F])
      )(productName =>
        VM.refillProduct(productName, amount).flatMap(_ => programLoop[F])
      )
    } yield ()

  def actionShowState[F[_]: Monad](implicit C: Console[F], VM: VendingMachine[F]): F[Unit] =
    for {
      state <- VM.getRawState()
      _ <- C.putStrLn("\n" + "="*45)
      _ <- C.putStrLn(" CURRENT RAW STATE (Debug Info)")
      _ <- C.putStrLn("="*45)
      _ <- C.putStrLn(s" * Inventory:       ${state.inventory}")
      _ <- C.putStrLn(s" * Inserted Amount: ${state.insertedAmount}")
      _ <- C.putStrLn(s" * Total Revenue:   ${state.revenue}")
      sortedTill = state.coinsTill.toList.sortBy(_._1).map { case (k, v) => s"$k -> $v" }.mkString(", ")
      _ <- C.putStrLn(s" * Coins in Till:   List($sortedTill)")
      _ <- C.putStrLn(s" * Used StudentIDs: ${state.usedStudentIds}")
      _ <- C.putStrLn(s" * Current Day:     ${state.dayCounter}")
      _ <- C.putStrLn(s" * Session Coins:   ${state.currentSessionCoins}")
      _ <- C.putStrLn("="*45)
      _ <- C.putStrLn(" Press Enter to return to menu... ")
      _ <- C.readLn()
      _ <- programLoop[F]
    } yield ()

  def actionShowLog[F[_]: Monad](implicit C: Console[F], VM: VendingMachine[F]): F[Unit] =
    for {
      logs <- VM.getLogs()
      _ <- C.putStrLn("\n" + "="*45)
      _ <- C.putStrLn(" SYSTEM LOG HISTORY")
      _ <- C.putStrLn("="*45)
      _ <- if (logs.isEmpty) C.putStrLn("  [Log is empty.]")
      else logs.foldLeft(Monad[F].pure(()))((acc, log) => acc.flatMap(_ => C.putStrLn(s"  -> $log")))
      _ <- C.putStrLn("="*45)
      _ <- C.putStrLn(" Press Enter to return to menu... ")
      _ <- C.readLn()
      _ <- programLoop[F]
    } yield ()

  def actionExit[F[_]: Monad](implicit C: Console[F], VM: VendingMachine[F]): F[Unit] =
    for {
      state <- VM.getRawState()
      _ <- C.putStrLn("\nShutting down vending machine...")
      _ <- C.putStrLn("Final State Summary:")
      _ <- C.putStrLn(s" Total Revenue: ${state.revenue} coins")
      _ <- C.putStrLn("Goodbye!")
    } yield ()

  def actionUnknown[F[_]: Monad](raw: String)(implicit C: Console[F], VM: VendingMachine[F]): F[Unit] =
    C.putStrLn(s"\n[!] Unknown action '$raw'.")
      .flatMap(_ => C.readLn())
      .flatMap(_ => programLoop[F])

  def main(args: Array[String]): Unit = {
    import vending.monads.VendingEff.vendingEffMonad

    implicit val consoleInterpreter: Console[VendingEff] = new LiveConsole
    implicit val vendingInterpreter: VendingMachine[VendingEff] = new LiveVendingMachine

    val runtime = programLoop[VendingEff]
    runtime.run(config, initialState, Nil).unsafeRun()
  }
}