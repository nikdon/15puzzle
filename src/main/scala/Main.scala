
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

  class Game(emptyField: Pos, gameBoard: Board) {
    def show(): String = ???
  }
}


object Main {
  import Game._


  trait Query
  case object Quit extends Query
  case class NewGame(difficulty: Int)
  case class PlayMove(move: Move)


  def play(): IO[Unit] = ???

  def greetings(): IO[Unit] = ???

  def showResults(game: Game): IO[Unit] = for {
    _ <- showGame(game)
    _ <- putStrLn("Gave over")
  } yield ()

  def showGame(game: Game): IO[Unit] = putStrLn(game.show())

  def gameLoop(game: Game): IO[Unit] = {
    if (isGameOver(game)) for {
      _ <- showResults(game)
      game <- setup()
    } yield gameLoop(game)
    else for {
      _ <- showGame(game)
      move <- askForMove()
    } yield reactOnMove(game, move)
  }

  def askForMove(): IO[Query] = ???

  def reactOnMove(game: Game, query: Query): IO[Unit] = ???

  def isGameOver(game: Game): Boolean = ???

  def setup(): IO[Game] = for {
    _ <- putStrLn("Start new game?")
    _ <- putStrLn("Choose difficulty: ")
    in <- readLn
    game <- readInt(in).fold(setup())(diff => shuffle(diff))
  } yield game

  def readInt(str: String): Option[Int] = Try(str.toInt).toOption
  def shuffle(diff: Int): IO[Game] = ???
}
