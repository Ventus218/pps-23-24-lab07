package ex4

import java.util.OptionalInt

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

  def find(board: Board, x: Int, y: Int): Option[Player] = board match
    case Nil => None
    case disk :: next => if disk.x == x && disk.y == y then Some(disk.player) else find(next, x, y)

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    board
      .filter(_.x == x)
      .sortBy(_.y)
      .lastOption
      .map(_.y) match
        case None => Some(0)
        case Some(y) => if y < bound then Some(y + 1) else None

  def isWinningBoardForPlayer(board: Board, player: Player): Boolean =
    (for
      x <- 0 to bound
      y <- 0 to bound
      if !((x == 0 || x == bound) && (y == 0 || y == bound)) // corners can be ignored
      if board.find(_ == Disk(x, y, player)).isDefined
    yield (x, y)).exists(
      (x, y) =>
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
    )

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
        yield if isWinningBoardForPlayer(game.last, player) || isWinningBoardForPlayer(game.last, player.other) then game else game :+ possibleMove
    
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
  // Exercise 2: implement placeAnyDisk such that..
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
  println("EX 4: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  val anygame = computeAnyGame(O, 9)
  anygame.foreach { g =>
    printBoards(g)
    if isWinningBoardForPlayer(g.last, X) then
      println("Won by X!")
    if isWinningBoardForPlayer(g.last, O) then
      println("Won by O!")
    println()
  }
  println(s"\ntotal games computed: ${anygame.length}")
//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
