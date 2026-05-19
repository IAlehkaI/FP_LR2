package vending.monads

import vending.domain.{Config, VendingMachineState}

case class VendingEff[A](run: (Config, VendingMachineState, List[String]) => IO[(VendingMachineState, List[String], A)])

object VendingEff {
  implicit val vendingEffMonad: Monad[VendingEff] = new Monad[VendingEff] {
    def pure[A](a: A): VendingEff[A] = VendingEff { (_, s, w) =>
      IO.pure((s, w, a))
    }
    def flatMap[A, B](fa: VendingEff[A])(f: A => VendingEff[B]): VendingEff[B] = VendingEff { (cfg, s, w) =>
      fa.run(cfg, s, w).flatMap { case (nextState, nextLog, a) =>
        f(a).run(cfg, nextState, nextLog)
      }
    }
  }

  def liftIO[A](io: IO[A]): VendingEff[A] = VendingEff { (_, s, w) =>
    io.map(a => (s, w, a))
  }

  def getS: VendingEff[VendingMachineState] = VendingEff { (_, s, w) => IO.pure((s, w, s)) }

  def modifyS(f: VendingMachineState => VendingMachineState): VendingEff[Unit] = VendingEff { (_, s, w) =>
    IO.pure((f(s), w, ()))
  }

  def askR: VendingEff[Config] = VendingEff { (cfg, s, w) => IO.pure((s, w, cfg)) }

  def tellW(log: String): VendingEff[Unit] = VendingEff { (_, s, w) => IO.pure((s, w :+ log, ())) }

  def getW: VendingEff[List[String]] = VendingEff { (_, s, w) => IO.pure((s, w, w)) }
}