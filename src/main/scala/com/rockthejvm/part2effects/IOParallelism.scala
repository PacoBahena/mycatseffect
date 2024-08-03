package com.rockthejvm.part2effects

import cats.Parallel
import cats.effect.IO.Par
import cats.effect.{IO, IOApp}





object IOParallelism extends IOApp.Simple {

  // IOS are usually sequential
  val anisIO: IO[String] = IO(s"The current thread is [${Thread.currentThread().getName}] Ani")
  val cameronIO: IO[String] = IO(s"The current thread is [${Thread.currentThread().getName}] Cameron")

  val composedIO: IO[String] = for {
    ani <- anisIO
    cameron <- cameronIO
  } yield s"$ani and $cameron love rockt the JVM"

  import com.rockthejvm.utils.DebugWrapper
  import cats.syntax.apply.catsSyntaxTuple2Semigroupal

  val meaningOfLife: IO[Int] = IO.delay(42)
  val favLang: IO[String] = IO.delay("Scala")

  val goalInLife: IO[String] = (meaningOfLife.debug,favLang.debug).mapN((num, string) => s"my goal in life is $num and $string")

  // how do we go to parallel ? sequential to parallel IO

  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug)

  import cats.effect.implicits.commutativeApplicativeForParallelF
  val goalInLifeParallel: Par[String] = (parIO1, parIO2).mapN((num, string) => s"My goal in life is $num and $string")
  val goalInLife_v2: IO[String] = Parallel[IO].sequential(goalInLifeParallel)

  // shorthand
  import cats.syntax.parallel._

  val goalInLife_v3: IO[String] = (meaningOfLife.debug,favLang.debug).parMapN((num, string) => s"my goal in life is $num and $string")

  // regarding failure

  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I cant do this "))
  val parallelWithFailure: IO[String] = (meaningOfLife.debug, aFailure.debug).parMapN(_ + _)

  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("second failure"))
  val twoFailures: IO[String] = (aFailure.debug,anotherFailure.debug).parMapN(_ + _)

  val twoFailuresDelayed = (IO(Thread.sleep(1000)) >> anotherFailure.debug, aFailure.debug).parMapN(_ + _)

  // note that you get the exception of the one that fails first.

  override def run: IO[Unit] = {
    //composedIO.map(println)
    //goalInLife_v2.debug.void
    //goalInLife.map(println)
    //goalInLife_v2.map(println)
    //goalInLife_v3.debug.void

    //parallelWithFailure.redeem(error => s"failed with $error",value => s"this is the $value").map(println)
    twoFailures.debug.void
    twoFailuresDelayed.debug.void

  }
}
