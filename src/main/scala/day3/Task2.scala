package day3

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.text

object Task2 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val path = Path("src/main/resources/day3.txt")
    val input = Files[IO]
      .readAll(path)
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(_.trim)
      .compile
      .toList
      .unsafeRunSync()

    val carbonString = input
      .indices
      .foldLeft(input)((l,n) => {
        filterList(n,l,false)
      })
    val oxygenString = input
      .indices
      .foldLeft(input)((l,n) => {
        filterList(n,l,true)
      })


    IO.println(
      Integer.parseInt(oxygenString.head, 2) * Integer.parseInt(carbonString.head, 2)
    )
  }



  private def filterList(bitPosition : Int, list: List[String], isOxygen: Boolean) = {
    if(list.size == 1) list
    else {
      val sum = list
        .map(_.charAt(bitPosition).toString.toInt)
        .map {
          case 1 => 1
          case 0 => -1
        }
        .sum

      val mostCommon = (sum, isOxygen) match {
        case (n, _) if n > 0 => 1
        case (n, _) if n < 0 => 0
        case (0,_) => 1
      }
      val res = (mostCommon,isOxygen) match {
        case (n, true) => n
        case (1, false) => 0
        case (0, false) => 1
      }

      list.filter(_.charAt(bitPosition).toString.toInt == res)
    }
  }


}
