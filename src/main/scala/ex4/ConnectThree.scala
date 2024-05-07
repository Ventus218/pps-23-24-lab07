package ex4

import java.util.OptionalInt
import scala.util.Random
import scala.annotation.targetName

// Optional!
object ConnectThree extends App:
  val bound = 3
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] = 
    board.find(d => d.x == x && d.y == y).map(_.player)

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    board
      .filter(_.x == x)
      .sortBy(_.y)
      .lastOption
      .map(_.y) match
        case None => Some(0)
        case Some(y) => if y < bound then Some(y + 1) else None

  def isWinningBoard(board: Board): Option[Player] =
    (for
      x <- 0 to bound
      y <- 0 to bound
      if !((x == 0 || x == bound) && (y == 0 || y == bound)) // corners can be ignored
      disk <- board.find(disk => disk.x == x && disk.y == y)
    yield (disk)).find(
      (disk) =>
        val x = disk.x
        val y = disk.y
        val player = disk.player
        // clearly a Hashed Map would be better
        // using functions allows to work lazylly
        val top = () => board.find(_ == Disk(x, y + 1, player))
        val bottom = () => board.find(_ == Disk(x, y - 1, player))
        val left = () => board.find(_ == Disk(x - 1, y, player))
        val right = () => board.find(_ == Disk(x + 1, y, player))
        val topLeftCorner = () => board.find(_ == Disk(x - 1, y + 1, player))
        val topRightCorner = () => board.find(_ == Disk(x + 1, y + 1, player))
        val bottomLeftCorner = () => board.find(_ == Disk(x - 1, y - 1, player))
        val bottomRightCorner = () => board.find(_ == Disk(x + 1, y - 1, player))

        (top().isDefined && bottom().isDefined) ||
        (left().isDefined && right().isDefined) ||
        (topLeftCorner().isDefined && bottomRightCorner().isDefined) ||
        (topRightCorner().isDefined && bottomLeftCorner().isDefined)
    ).map(_.player)

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y <- firstAvailableRow(board, x)
    yield
      board :+ Disk(x, y, player)

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] =
    def _computeAnyGame(player: Player, moves: Int, games: LazyList[Game]): LazyList[Game] = moves match
      case 1 => LazyList.from(placeAnyDisk(Seq(), player).map(Seq(_)))
      case _ =>
        for
          game <- _computeAnyGame(player.other, moves - 1, games)
          possibleMove <- placeAnyDisk(game.last, player)
        yield if isWinningBoard(game.last).isDefined then game else game :+ possibleMove
    
    val lastPlayer = if moves % 2 == 0 then player.other else player
    _computeAnyGame(lastPlayer, moves, LazyList())

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None
  println()

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 1)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  println()

  // Exercise 3: implement placeAnyDisk such that..
  println("EX 4: ")
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  
  // Exercise 4 (ADVANCED!): implement computeAnyGame such that..
  // Exercise 5 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
  println("EX 4-5: ")
  val anygame = computeAnyGame(O, 4)
  anygame.foreach { g =>
    printBoards(g)
    isWinningBoard(g.last) match
      case Some(player) => println(s"Won by $player!")
      case None => ()
    println()
  }
  println(s"\ntotal games computed: ${anygame.length}")

  // Exercise 6 (Random AI)
  println("EX 6 (Random AI): ")

  trait AI:
    def makeMove(board: Board): Board

    @targetName("makeMoveGame") // Found on the internet
    def makeMove(game: Game): Game =
      game appended makeMove(game.lastOption.getOrElse(Seq()))

  class RandomAI(seed: Int, player: Player) extends AI:
    var randomGenerator = Random(seed)
    def makeMove(board: Board): Board =
      require(board.lastOption.map(_.player) != Some(player))
      val possibleMoves = placeAnyDisk(board, player)
      require(possibleMoves.length > 0)
      possibleMoves(randomGenerator.nextInt(possibleMoves.length))

  // To test two games will be played and they should be different.
  val xRandomAI1 = RandomAI(1234, Player.X)
  val oRandomAI1 = RandomAI(5678, Player.O)
  var game1: Game = Seq()
  for i <- 0 until (bound*bound)/2 do
    game1 = xRandomAI1.makeMove(game1);
    game1 = oRandomAI1.makeMove(game1);
  
  val xRandomAI2 = RandomAI(5678, Player.X)
  val oRandomAI2 = RandomAI(1234, Player.O)
  var game2: Game = Seq()
  for i <- 0 until (bound*bound)/2 do
    game2 = xRandomAI2.makeMove(game2);
    game2 = oRandomAI2.makeMove(game2);
  
  printBoards(game1)
  printBoards(game2)

  println(s"RandomAI test ${if game1 != game2 then "passed" else "not passed"}")


