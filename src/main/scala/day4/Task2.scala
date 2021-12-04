package day4

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import cats.implicits._
import fs2.text

object Task2 extends IOApp.Simple {
  override def run: IO[Unit] = {
    val path = Path("src/main/resources/day4.txt")
    val input = Files[IO]
      .readAll(path)
      .through(text.utf8.decode)


    val winningsNumbers =
      input
        .through(text.lines)
        .map(_.trim)
        .head
        .compile
        .last
        .flatMap {
          case Some(value) => IO.pure(value)
          case None => IO.raiseError(new RuntimeException("something went wrong"))
        }
        .map(_.split(",").toList)
        .map(_.map(_.toInt))

    val bingoBoards =
      input
        .flatMap(s => fs2.Stream.fromIterator[IO](s.split("\\n\\n").toIterator, 4096))
        .tail
        .map { s =>
          BingoBoard(
            s
              .split("\n")
              .map(_.trim)
              .map(u => {
                u.split(" ").toList
              })
              .map(_.filterNot(_.isEmpty))
              .map(t =>
                t.map(s => Cell(s.toInt, isSelected = false)))
              .toList
          )

        }
        .compile
        .toList



    def func(numbers: List[Int], boards: List[BingoBoard]) = {
      numbers.foldLeft((Option.empty[(BingoBoard,Int)],boards))((t,n) => {
        val (boardAndInt, boards) = t
        if(boards.size == 1) {
          val newBoards = boards.map(_.mark(n))
          (boardAndInt.orElse {
            newBoards.find(_.isDone).tupleRight(n)
          }, newBoards)
        }
        else {
          val leftoutBoards = boards.map(_.mark(n)).filterNot(_.isDone)
          (Option.empty[(BingoBoard,Int)],leftoutBoards)
        }
      })

    }

    val a = (for {
      numbers <- winningsNumbers
      boards <- bingoBoards
    } yield func(numbers,boards))
      .unsafeRunSync()
      ._1
      .get

    IO.println(
      a._1.rows.flatMap(_.filterNot(_.isSelected).map(_.number)).sum * a._2
    )
  }

  case class BingoBoard(
                         rows: List[List[Cell]]
                       ) {
    def mark(n: Int): BingoBoard = {
      BingoBoard(
        rows.map(_.map(c =>
          if (c.number == n)
            Cell(n, isSelected = true)
          else c
        ))
      )
    }

    def isDone: Boolean = {
      val byRows = rows.exists(l => {
        l.forall(_.isSelected)
      })
      val byColumns = rows
        .transpose
        .exists(l => {
          l.forall(_.isSelected)
        })

      byRows || byColumns
    }
  }

  case class Cell(
                   number: Int,
                   isSelected: Boolean
                 )
}
