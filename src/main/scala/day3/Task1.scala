package day3

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.text

object Task1 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val path = Path("src/main/resources/day3.txt")
    val res = Files[IO]
      .readAll(path)
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(_.trim)
      .fold(Map.empty[Int, Int]) { (map, s) =>
        val digits = s.map(_.toString.toInt).map {
          case 0 => -1
          case 1 => 1
          case _ => throw new RuntimeException("invalid input")
        }
          .zipWithIndex

        digits.foldLeft(map)(
          (map, tuple) => {
            val (value, index) = tuple
            map.updatedWith(index)(_.map(_ + value).orElse(Some(value)))
          })
      }
      .compile
      .last

    res.flatMap {
      case Some(map) => {
        val (gamma, epsilon) = map
          .toSeq
          .sortWith(_._1 < _._1)
          .foldLeft(("", ""))((s, t) => {
          println("index is")
          println(t._1)
          if (t._2 > 0) (s._1 + "1", s._2 + "0")
          else (s._1 + "0", s._2 + "1")
        })
        IO.println(
          Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
        )
      }
      case None => IO.raiseError(new RuntimeException("something went bad"))
    }

  }


}
