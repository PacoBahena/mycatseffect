package com.rockthejvm.part2effects

import cats.effect.IO

import scala.io.StdIn



object IOIntroduction {

  // IO
  val ourFirstIO: IO[Int] = IO.pure(42) // arg without side effects.
  val aDelayedIO: IO[Int] = IO.delay {
    println("I'm producing an integer")
    54
  }

//  val shouldNotDoThis: IO[Int] = IO.pure({
//    println("I'm producing an integer")
//    54
//  })

  // map,flatMap
  val improvedMeaningOfLife: IO[Int] = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife: IO[Unit] = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  val aDelayedIO_v2: IO[Int] = IO {  // apply == delay
    println("Im producing an integer")
    54
  }

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  val x = IO(StdIn.readLine())
  val y = IO(StdIn.readLine())
//  val z = IO.delay((x: String,y: String) => x + y)
  val z = IO.delay((x: String) => println(x))

//  z

  val  res: IO[Unit] = x.flatMap(line1 => y.flatMap(line2 => z.map(_ => println(line1 + line2))))


  // cats - mapN combine IO effects as tuples

  import cats.syntax.apply._
  val combineMeaningOfLife: IO[Unit] = (ourFirstIO, improvedMeaningOfLife).mapN(_ + _).map(println)

  def smallProgram_v2() =
    (IO(StdIn.readLine()),IO(StdIn.readLine())).mapN(_ + _).map(println)


  /**
   Exercises
   **/

   // 1. Sequence two ios and take the result of the last one.
   // hint: use flatMap

   def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
     ioa.flatMap(_ => iob)


  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // *> reads as andThen

  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // *> andThen with by-name call

   // 2. sequence two ios and take the result of the first one

  def sequenceTakeFirst[A,B](ioa: IO[A], iob: IO[B]): IO[A] = {
//    for {
//      res <- ioa
//      _ <- iob
//    } yield res
    ioa.flatMap(res => iob.map(_ => res))
   }

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob

  // 3/ repeat a IO effect forever, use flatMap + recursion

  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(_ => forever(io))

  def forever_v2[A](io: IO[A]): IO[A] =
    io >> forever_v2(io)

  def forever_v3[A](io: IO[A]): IO[A] =
    io *> forever_v3(io)

  def forever_v4[A](io: IO[A]): IO[A] =
    io.foreverM // with tail recursion

  // 4 convert an IO to a different type

  def convert[A,B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)

  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] =
    ioa as value

  // 5 - discard value inside IO, just return Unit

  def asUnit[A](ioa: IO[A]): IO[Unit] =
    ioa.map(_ => ())

  def asUnit_v2[A](ioa: IO[A]): IO[Unit] =
    ioa.as(()) // not recommended

  def asUnit_v3[A](ioa: IO[A]): IO[Unit] =
    ioa.void

  // 6 fix stack recursion

  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] = ???


  // 7 write a fibonacci function that does not crash on recursion
  // user recursion, ignore exponential complexity and use flatMap
  def fibo(n: Int): IO[Int] = ???

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    println(aDelayedIO.map(_ * 2).unsafeRunSync())

//    smallProgram().unsafeRunSync()

//    val  res: IO[Unit] = x.flatMap(line1 => y.flatMap(line2 => z.map(_ => println(line1 + line2))))
//    res.unsafeRunSync()

//      smallProgram_v2().unsafeRunSync()

//    forever_v2( IO {
//      println("forever!")
//      Thread.sleep(100)
//    }).unsafeRunSync()

    // stack overflow
    forever_v3( IO {
          println("forever!")
          Thread.sleep(100)
    }).unsafeRunSync()

    // note that with the forever_v2, because of the by-name/lazy,
    // no stack overflow

  }
}

