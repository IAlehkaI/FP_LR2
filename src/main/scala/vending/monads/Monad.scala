package vending.monads

trait Monad[M[_]] {
  def pure[A](a: A): M[A]
  def flatMap[A, B](fa: M[A])(f: A => M[B]): M[B]
  def map[A, B](fa: M[A])(f: A => B): M[B] = flatMap(fa)(a => pure(f(a)))
}

object Monad {
  def apply[M[_]](implicit ev: Monad[M]): Monad[M] = ev

  object syntax {
    extension [M[_]: Monad, A](fa: M[A]) {
      def map[B](f: A => B): M[B] = Monad[M].map(fa)(f)
      def flatMap[B](f: A => M[B]): M[B] = Monad[M].flatMap(fa)(f)
    }
  }
}