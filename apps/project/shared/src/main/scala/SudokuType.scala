package project

import cs214.webapp.UserId

import scala.util.{Failure, Success, Try}

/** Stores all information about the current game. */
case class SudokuState(board : Board, currentPlayer: UserId, isFinished: Boolean, slectedCell : Option[(Int, Int)], nbSelected: Option[Int], level : Option[Int]) // Change this type to hold actual information (use an enum, class, …) 

/** There is only one eventa in tic-tac-toe: clicking a cell. */
enum SudokuEvent:
  /** User clicked cell (x, y) */
  case Select(x: Int, y: Int)
  case Input(nb : Int)
  case Highlight(nb : Int)
  case Beginning(level: Int)
  case AnnotateWith(nb: Int)
  case Refresh 

/** Client views reflect the state of the game: playing or finished. */
enum SudokuView:
  /** Game in progress. */
  case Playing(board: Board)

  case Begin

  case Highlighted(nb: Int, board: Board)

  /** Game over. [[winner]] is [[None]] if the game ended in a tie. */
  case Finished


// Change this class definition to store board states.
// The boolean represent whether the number has been fixed at the beginning (true) or added by the user 
case class Board(cells: Array[Array[ Option[(Int, Boolean, List[Int])]]]):
  
  /** Get the value in the cell at (r, c). */
  def get(r : Int, c: Int) : Option[(Int, Boolean, List[Int])] =
    cells(r)(c)

  def apply(r: Int, c: Int): Option[Int] =
    //require(r >= 0 && c >= 0 && r < 9 && c < 9) // Add an appropriate precondition
    cells(r)(c) match
      case None => None
      case Some(value) => Some(value._1)
  
  def isFixed(r: Int, c: Int): Boolean =
    cells(r)(c) match
      case None => false
      case Some(value) => value._2
    
  def numberOnly() : Array[Array[Option[Int]]] =
    cells.map(row => row.map(col => Some(col.get._1)))


  // another solution which would have been much cleaner was using an immutable structure (such as Vector, Lists...) in order to avoid this weak comparison for equals 
  override def equals(other: Any): Boolean = other match {
    case that: Board => that.toString() == this.toString()
    case _ => false
  }
  
  override def toString(): String = 
    var s = ""
    cells.foreach(x => x.foreach(y => s + y.toString))
    s




