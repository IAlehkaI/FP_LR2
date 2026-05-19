package vending.interpreters

import vending.algebras.Console
import vending.monads.{VendingEff, IO}

class LiveConsole extends Console[VendingEff] {
  def putStrLn(str: String): VendingEff[Unit] = VendingEff.liftIO(IO.putStrLn(str))
  def readLn(): VendingEff[String] = VendingEff.liftIO(IO.readLn())
  def clear(): VendingEff[Unit] = VendingEff.liftIO(IO.delay {
    print("\u001b[H\u001b[2J")
    System.out.flush()
  })
}