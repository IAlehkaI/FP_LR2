package vending.monads

import java.io.{BufferedReader, InputStreamReader}

case class IO[A](unsafeRun: () => A) {
  def map[B](f: A => B): IO[B] = IO(() => f(unsafeRun()))
  def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(unsafeRun()).unsafeRun())
}

object IO {
  def pure[A](a: A): IO[A] = IO(() => a)
  def delay[A](a: => A): IO[A] = IO(() => a)

  def putStrLn(str: String): IO[Unit] = IO(() => {
    val out = new java.io.PrintStream(System.out, true, "UTF-8")
    out.println(str)
  })

  def readLn(): IO[String] = IO(() => {
    val reader = new BufferedReader(new InputStreamReader(System.in, "UTF-8"))
    reader.readLine()
  })
}