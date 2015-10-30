
import scala.util.Try
import scalaz.effect.IO
import scalaz.effect.IO._


object Game {
  type Pos = (Int, Int)
  type Label = Int
  type Board = List[(Pos, Label)]

  trait Move

  case object Up extends Move
  case object Down extends Move
  case object Left extends Move
  case object Right extends Move

  class Game(emptyField: Pos, gameBoard: Board)
}


object Main {
  import Game._


  trait Query
  case object Quit extends Query
  case class NewGame(difficulty: Int)
  case class PlayMove(move: Move)


  def play(): IO[Unit] = ???

  def greetings(): IO[Unit] = ???

  def gameLoop(): IO[Unit] = ???

  def setup(): IO[Game] = for {
    _ <- putStrLn("Start new game?")
    _ <- putStrLn("Choose difficulty: ")
    in <- readLn
    game <- readInt(in).fold(setup())(diff => shuffle(diff))
  } yield game

  def readInt(str: String): Option[Int] = Try(str.toInt).toOption
  def shuffle(diff: Int): IO[Game] = ???
}
