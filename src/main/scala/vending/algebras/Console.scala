package vending.algebras

trait Console[F[_]] {
  def putStrLn(str: String): F[Unit]
  def readLn(): F[String]
  def clear(): F[Unit]
}