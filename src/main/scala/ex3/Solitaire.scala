package ex3

object Solitaire extends App:
  case class Placement(x: Int, y: Int)
  type Solution = Seq[Placement]

  def render(solution: Solution, width: Int, height: Int): String =
    // val reversed = solution.toSeq.reverse
    val reversed = solution
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf(Placement(x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def placementsFromLastPlacement(width: Int, height: Int, lastPlacement: Placement): Iterable[Placement] =
    var res = Seq[Placement]()
    for
      x <- lastPlacement.x - 3 to lastPlacement.x + 3
      if x >= 0 && x < width
      y <- lastPlacement.y - 3 to lastPlacement.y + 3
      if y >= 0 && y < height
    do
      val dx = Math.abs(x - lastPlacement.x)
      val dy = Math.abs(y - lastPlacement.y)
      if (dx + dy == 3 && (dx == 0 || dy == 0)) || (dx + dy == 4 && dx == 2) then
        res = res :+ Placement(x, y)
    res

  def solve(width: Int, height: Int, currentSolutions: Seq[Solution] = Seq(), nToBePlaced: Int = width*height): Seq[Solution] =
    if nToBePlaced == 0 then
      currentSolutions
    else
      currentSolutions match
      case Nil => solve(width, height, Seq(Seq(Placement(width / 2, height / 2))), nToBePlaced - 1)
      case _ =>
        val newSolutions =
          for
            sol <- currentSolutions
            possiblePlacement <- placementsFromLastPlacement(width, height, sol.last)
            if sol.find(_ == possiblePlacement).isEmpty
          yield
            sol :+ possiblePlacement
        if newSolutions.isEmpty then
          Seq()
        else
          solve(width, height, newSolutions, nToBePlaced - 1)

  val width = 7
  val height = 5
  val solutions = solve(width, height)
  solutions.foreach:
    (s) => println(render(solution = s, width = width, height = height)); println()
  println(s"${solutions.size} solutions found!")
