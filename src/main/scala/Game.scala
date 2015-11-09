import scalaz.Scalaz._
import scalaz.effect.IO


sealed trait Move
case object Up extends Move
case object Down extends Move
case object Left extends Move
case object Right extends Move


sealed trait Query
case object Quit extends Query
case class NewGame(difficulty: Int) extends Query
case class Play(m: Move) extends Query


class Game(val emptyField: Game.Pos, val gameBoard: Game.Board) {
  import Game._

  def show(): String = {
    gameBoard.map(_ + 1).grouped(WIDTH).foldLeft("+---+---+---+---+\n")((acc, row) => {
      acc + "|" + row.mkString("\t|") +"\t|" + "\n+---+---+---+---+\n"
    }).replace("16", "  ")
  }
}


object Game {
  type Pos = (Int, Int)
  type Label = Int
  type Board = (Vector[Label])
  type Vec = (Int, Int)

  val EMPTY_LABEL = 15
  val WIDTH = 4

  def to1D(pos: Pos): Int = {
    val (x, y) = pos
    WIDTH * x + y
  }

  def to2D(idx: Int): Pos = {
    val x = idx / WIDTH
    val y = idx % WIDTH
    (x, y)
  }

  val initGame: Game = {
    val empty: Pos = to2D(WIDTH * WIDTH)
    val board: Board = Vector.tabulate(WIDTH*WIDTH)(lbl => lbl)
    new Game(empty, board)
  }

  def doMove(m: Move, g: Game): Game = {

    def orient(move: Move): Vec = move match {
      case Up    => (-1, 0)
      case Down  => (1, 0)
      case Left  => (0, -1)
      case Right => (0, 1)
    }

    def shift(vec: Vec, pos: Pos): Pos = {
      val (va, vb) = vec
      val (pa, pb) = pos
      (va + pa, vb + pb)
    }

    def within(pos: Pos): Boolean = {
      val check = (p: Int) => p >= 0 && p <= (WIDTH - 1)
      val (a, b) = pos
      check(a) && check(b)
    }

    val currEmpPos = g.emptyField
    val nextEmpPos = shift(orient(m), currEmpPos)

    if (within(nextEmpPos)) {
      val currBoard = g.gameBoard
      val nextBoard = currBoard
                      .updated(to1D(currEmpPos), currBoard(to1D(nextEmpPos)))
                      .updated(to1D(nextEmpPos), EMPTY_LABEL)

      new Game(nextEmpPos, nextBoard)
    } else g
  }

  def shuffle(diff: Int): IO[Game] = {
    val nextStage = initGame.gameBoard.permutations.drop(42).next()
    val emptyIdx = nextStage.indexOf(EMPTY_LABEL)
    new Game(to2D(emptyIdx), nextStage).point[IO]
  }

  def isGameOver(game: Game): Boolean = game == initGame
}