package com.rockthejvm.part2effects

import cats.Traverse
import cats.effect.{IO, IOApp}
import cats.implicits.catsSyntaxParallelTraverse1
import com.rockthejvm.utils.DebugWrapper

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  // define a method calll heacy computation
  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workload: List[String] = List("I quite like CE","Scala is great","loooking forward to some awesome stuff")

  val listTraverse: Traverse[List] = Traverse[List]

  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workload.map(heavyComputation)
    futures.foreach(_.foreach(println))
  }

  // Future[List[Int]] would be hard to obtain handle.

  def traverseFutures(): Unit = {
    import cats.instances.list._

    val singleFuture: Future[List[Int]] = listTraverse.traverse(workload)(heavyComputation)
    singleFuture.foreach(println)
  }

  // traverse for IO
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug

  val ios: List[IO[Int]] = workload.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workload)(computeAsIO)

  // note aggregating results on a list of ios is hard
  // aggregarion results on an io of list is easier and recommended.

  //parallel traversal
  import cats.syntax.parallel.catsSyntaxParallelSequence_
  val parallelSingleIO: IO[List[Int]] = workload.parTraverse(computeAsIO)

  /**
   * Exercises
   */
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = {
    listTraverse.traverse(listOfIOs)(identity)
  }

  // hard version
  def sequence_v2[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(listOfIOs)(identity)

  // parallel version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(identity)

  // hardVersion
  def parSequence_v2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    wrapperOfIOs.parTraverse(identity)


  override def run: IO[Unit] = {

//    singleIO.map(_.sum).debug.void
    parallelSingleIO.void
  }
}
