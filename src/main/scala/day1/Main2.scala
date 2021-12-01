package day1

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.text

object Main2 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val path = Path("src/main/resources/day1.txt")
    val numbers  = Files[IO]
      .readAll(path)
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .flatMap(s => fs2.Stream.eval(
        IO(Integer.parseInt(s))
      ))
      .compile
      .toList
      .unsafeRunSync()
    val n = divvy(3,1,numbers).map(_.sum)
    fs2.Stream
      .fromIterator[IO](n.toIterator,4096)
      .zipWithPrevious
      .filter {
        case (previous, current) => previous.exists(_ < current)
      }
      .compile
      .count
      .flatMap(IO.println)

  }

  private def chop[A,B](f: (List[A] => (B, List[A])), list: List[A]): List[B] = {
    if (list.isEmpty) List.empty[B]
    else {
      val (a, b) = f(list)
      a :: chop(f, b)
    }
  }

  private def divvy(size: Int, offset: Int, list: List[Int]): List[List[Int]] = {
    if (list.isEmpty) List.empty[List[Int]]
    else {
      val choppedL = chop[Int,List[Int]](l => (l.take(size), l.drop(offset)),list)
      choppedL.filter(ws => size == ws.length)
    }
  }
}
