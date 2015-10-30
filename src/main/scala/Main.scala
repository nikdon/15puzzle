
import scala.util.Try
import scalaz.Scalaz._
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
  case class NewGame(difficulty: Int) extends Query
  case class Play(m: Move) extends Query


  def play(): IO[Unit] = for {
    _ <- greetings()
    game <- setup()
  } yield gameLoop(game)

  def greetings(): IO[Unit] = ???

  def showResults(game: Game): IO[Unit] = for {
    _ <- showGame(game)
  } yield putStrLn("Gave over")

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

  def parseQuery(input: String): Option[Query] = input match {
    case "up"    => Play(Up).some
    case "u"     => Play(Up).some
    case "down"  => Play(Down).some
    case "d"     => Play(Down).some
    case "left"  => Play(Left).some
    case "l"     => Play(Left).some
    case "right" => Play(Right).some
    case "r"     => Play(Right).some
    case "quit"  => Quit.some
    case "q"     => Quit.some

    case str if str.startsWith("new") =>
      str.split(" ").tail.headOption
        .flatMap(n => readInt(n))
        .map(NewGame)
  }

  def showAsk(): IO[Unit] = ???

  def remindMoves(): IO[Unit] = {
    val reminder =
      """Possible moves of the empty cell:
        | left  or l  -- move on the left
        | right or r  -- move on the right
        | up    or u  -- move up
        | down  ot d  -- move down
        |
        | Other actions:
        |   new [Int] -- start new game with difficulty [Int]
        |   quit or q -- quit the game
      """.stripMargin

    putStrLn(reminder)
  }

  def wrongMove(): IO[Unit] = for {
    _ <- putStrLn("Can't recognize move.")
  } yield remindMoves()

  def askAgain(): IO[Query] = ???

  def askForMove(): IO[Query] = for {
    _ <- showAsk()
    in <- readLn
    move <- parseQuery(in).fold(wrongMove().flatMap(_ => askAgain()))(q => IO(q))
  } yield move

  def quit(): IO[Unit] = putStrLn("See you again, stranger!")

  def doMove(m: Move, g: Game): Game = ???

  def reactOnMove(game: Game, query: Query): IO[Unit] = query match {
    case Quit                  => quit()
    case g@NewGame(difficulty) => shuffle(difficulty) flatMap gameLoop
    case p@Play(move)          => gameLoop(doMove(move, game))
  }

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
