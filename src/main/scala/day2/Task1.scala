package day2

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.text

object Task1 extends IOApp.Simple {

  override def run: IO[Unit] = {
    val path = Path("src/main/resources/day2.txt")
    Files[IO]
      .readAll(path)
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .flatMap(s => fs2.Stream.eval{
        IO {
        val words = s.split(" ")
        (words(0),words(1))
       }
      }
      )
      .flatMap(t => fs2.Stream.eval(
        IO((t._1,Integer.parseInt(t._2)))
      ))
      .flatMap {
        case (s, n) =>  fs2.Stream.eval {
          s match {
            case "forward" => IO.pure(Forward(n))
            case "up" => IO.pure(Up(n))
            case "down" => IO.pure(Down(n))
            case other => IO.raiseError(new RuntimeException(s"can't handle $other"))
          }
        }
      }
      .fold((0,0)) ((acc, move) => {
        move match {
          case Forward(n) => acc.copy(_1 = acc._1 + n)
          case Up(n) => acc.copy(_2 = acc._2 - n)
          case Down(n) => acc.copy(_2 = acc._2 + n)
        }
      })
      .map {
        case (i, i1) => i * i1
      }
      .compile
      .last
      .flatMap(IO.println)
  }


  sealed trait Move
  case class Forward(n: Int) extends Move
  case class Up(n: Int) extends Move
  case class Down(n: Int) extends Move
}
