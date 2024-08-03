package com.rockthejvm.part2effects

import scala.concurrent.Future
import scala.io.StdIn

object Effects {

  // pure functional programming
  // substitution
  def combine(a: Int, b: Int): Int = a + b
  val five = combine(2,3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  // referential transparency = can replace an expression with its value
  // as many times as we want without changing behavior

  val printSomething: Unit = println("Cats Effect")
  val printSomething_v2: Unit = ()  // not the same although they return the same
  // this are impure programs

  // example: change a variable
  var anInt = 0
  val changingVar: Unit = anInt += 1
  val aChangingVar_v2: Unit = () // not the same another impure program.

  // side effects are innetivatable for any program.

  // an effect bridges the necessity of side effect while building pure functional programs.

  // effect type

  /*  PROPERTIES
  * - type signature  describes the kind of calculation
  * - type signature describes the VALUE that will be calculated
  * - when side effects are needed effect construction should be separated from execution.
  * */

  // 1. option
  /**
  * - does describe the kind of calculation: describes a possibly absent value
  * - does describe the value , computes an int if exists.
  * - side effects are not needed
  **/

  val anOption: Option[Int] = Option(42)

  /*
  * 2. Future is not a good effect type because
  * it does not accomplish property 3.
  *
  * - describes asyncronous computation
  * - computest type int if successful
  * - side effect is required, allocating a thread, and it is allocated at construction, executed.
  * */

  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)

  /*
  * 3. example MyIO
  * - describes any computation that has side effects
  * - calculates a value of type A, if its successful.
  * - side effects are required for the evaluation of () => A
  *   - YES - the creation of MYIO does not produce the side effects on construction
  *    check proof below
  * */

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))
    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val aIO: MyIO[Int] = MyIO( () => {
    println("hello im paco ")
    42
  })

  /**
  Exercises
  1. Create an IO which currently returns the current time of the system.
  2. An IO which measures the duartion of a computation. (hint: use exercise number one. )
  3. An IO whioch prints something to the console
  4. An IO which reads a line (a String) from the std input.
  **/

  // 1
  val clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  // 2
  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
    startTime <- clock
    _ <- computation
    finishTime <- clock
  } yield finishTime - startTime




  /*
  clock.flatMap(startTime => computation.flatMap(_ => clock.map(finishTime => finishTime - startTime)))

  clock.map(finishTime => finishTime - startTime) =

    MyIO(() => f(() => System.currentTimeMillis()))
    MyIO(() =>System.currentTimeMillis() - startTime)

  clock.flatMap(startTime => computation.flatMap(_ => MyIO(() => System.currentTimeMillis() - startTime) ) )

  computation.flatMap(_ => MyIO(() => System.currentTimeMillis() - startTime) ) =

    MyIO(() => lambda(__COMP__).unsafeRun())
    MyIO(() => MyIO(() => System.currentTimeMillis() - startTime)).unsafeRun()
    MyIO(() => System.currentTimeMillis() - startTime)

  => clock.flatMap(startTime => MyIO(() => System.currentTimeMillis() - startTime))

  MyIO(() => System.currentTimeMillis()_after_comp - System.currentTimeMillis()_vefore_compr )

  * */

  val mycomp = MyIO(() => 2 + 3)

  val duration: MyIO[Long] =
    clock.flatMap(startTime => mycomp.flatMap(_ => clock.map(finishTime => finishTime - startTime)))

  println(duration.unsafeRun())
  println(measure(mycomp).unsafeRun())

  def testTimeIO(): Unit = {
    val test = measure(MyIO( () => Thread.sleep(1000)))
    println(test.unsafeRun())
  }



  // 3

  def prtLine(line: String): MyIO[Unit] = MyIO(() => println(line))

  // 4

  val read: MyIO[String] = MyIO(() => StdIn.readLine())

  def main(args: Array[String]): Unit = {

  testTimeIO()

    val c: Option[Int] = Some(3).flatMap(x => Some(x+3))

  }
}
