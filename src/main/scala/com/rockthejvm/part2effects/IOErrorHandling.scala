package com.rockthejvm.part2effects

import cats.effect.IO

import scala.util.{Try, Success, Failure }

object IOErrorHandling {

  // IO: pure, delay, defer
  // create failed effects

  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("A FAILURE"))
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail"))

  // handle those exceptions.

  val dealWithIt = aFailure.handleErrorWith{
    case _: RuntimeException => IO.delay(println("Im still here"))
    case _ => IO.delay(println("failed, dont know why"))
  }

  // turn into an Either
  val effectAsEither: IO[Either[Throwable,Int]] = aFailure.attempt
  // redeem: transform the failure and the success in one go.
  val resultAsString: IO[String] = aFailure.redeem(ex => s"Fail: $ex", value => s"SUCCESS: $value")
  // redeemWith
  val resultAsEffect: IO[Unit] = aFailure.redeemWith(ex => IO(println(s"Fail: $ex")), value => IO(println(s"SUCCESS: $value")))


  /**
   * Exercises
   *
   * 1. construct potentially failed IOs from standard data types (Option, Try, Either)
   */

  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] =
    option match {
      case Some(value) => IO.pure(value)
      case None => IO.raiseError(ifEmpty)
    }

  def try2IO[A](aTry: Try[A]): IO[A] =
    aTry match {
      case Success(value) => IO.pure(value)
      case Failure(exception) => IO.raiseError(exception)
    }

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] =
    anEither match {
      case Left(exception) => IO.raiseError(exception)
      case Right(value) => IO.pure(value)
     }

//  IO.fromOption()
//  IO.fromTry()
//  IO.fromEither()

  // 2 handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = {
    //io.redeem(ex => handler(ex), value => value)
    io.redeem(handler,identity)
  }

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    //io.redeemWith(ex => handler(ex), value => IO.pure(value))
    io.redeemWith(handler,IO.pure)




  def main(args: Array[String]): Unit = {

    import cats.effect.unsafe.implicits.global
    // aFailedCompute.unsafeRunSync()
    // aFailure.unsafeRunSync() // recommended

    dealWithIt.unsafeRunSync()

    println(resultAsString.unsafeRunSync())

    resultAsEffect.unsafeRunSync()
  }
}
