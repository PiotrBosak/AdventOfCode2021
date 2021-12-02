package day2

package day2

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.text

object Task2 extends IOApp.Simple {

  override def run: IO[Unit] = {
    val path = Path("src/main/resources/day2.txt")
    Files[IO]
      .readAll(path)
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .flatMap(s => fs2.Stream.eval {
        IO {
          val words = s.split(" ")
          (words(0), words(1))
        }
      }
      )
      .flatMap(t => fs2.Stream.eval(
        IO((t._1, Integer.parseInt(t._2)))
      ))
      .flatMap {
        case (s, n) => fs2.Stream.eval {
          s match {
            case "forward" => IO.pure(Forward(n))
            case "up" => IO.pure(Up(n))
            case "down" => IO.pure(Down(n))
            case other => IO.raiseError(new RuntimeException(s"can't handle $other"))
          }
        }
      }
      .fold(Position(0, 0, 0))((acc, move) => {
        move match {
          case Forward(n) => {
            acc.copy(horizontal = acc.horizontal + n,
              depth = acc.depth + (acc.aim * n))
          }
          case Up(n) => acc.copy(aim = acc.aim - n)
          case Down(n) => acc.copy(aim = acc.aim + n)
        }
      })
      .map {
        case Position(_, horizontal, depth) => horizontal * depth
      }
      .compile
      .last
      .flatMap(IO.println)
  }


  case class Position(
                       aim: Int,
                       horizontal: Int,
                       depth: Int
                     )

  sealed trait Move

  case class Forward(n: Int) extends Move

  case class Up(n: Int) extends Move

  case class Down(n: Int) extends Move

}
