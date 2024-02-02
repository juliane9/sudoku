package project

import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}
import scala.util.Random
import scala.util.boundary

import cs214.webapp.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer
import cs214.webapp.messages.Action

import project.*
import scala.util.boundary


/**
  * The state machine that handles the logic of the sudoku game between the different states.
  */
object SudokuStateMachine extends cs214.webapp.StateMachine[SudokuEvent, SudokuState, SudokuView]:
    val name: String = "sudoku"

    // Used to encode/ decode with json values the transitions between the state machine and the server 
    val wire = SudokuWire

    /**
      * Given a level, initialize a valid sudoku grid. This will be done by first generating a completed sudoku grid
      * and then removing a number of elements depending on the level to ensure the sudoku can always be completed,
      * though possibly in several ways.
      *
      * @param user : the user id 
      * @param level : the level wished by the player 
      * @return a sudoku state with a valid initialized board, corresponding to the desire level
      */
    def initializeAll(user: UserId, level: Int): SudokuState =
        
        // Creates a board adapated to the level by generating a completed sudoku with random numbers and then removing some numbers
        def initializeBoard() : Array[Array[Option[(Int, Boolean, List[Int])]]] =
            // Generate the random numbers for the grid
            def generateCompletedBoard(): Array[Array[Int]] =
                val board = Array.ofDim[Int](9, 9)

                for (i <- 0 until 9; j <- 0 until 9) {
                    board(i)(j) = (i * 3 + i / 3 + j) % 9 + 1
                }

                // Creates the randomness  
                shuffleBoard(board)
                board
        

            def shuffleBoard(board: Array[Array[Int]]): Unit =
                // Shuffle rows within each band of 3 rows
                for (band <- 0 until 9 by 3) 
                    val rows = (band until band + 3).toList
                    Random.shuffle(rows).zip(rows).foreach { case (srcRow, destRow) =>
                        swapRows(board, srcRow, destRow)
                    }
            

                // Shuffle columns within each band of 3 columns
                    for (band <- 0 until 9 by 3) 
                        val cols = (band until band + 3).toList
                        Random.shuffle(cols).zip(cols).foreach { case (srcCol, destCol) =>
                            swapColumns(board, srcCol, destCol)
                        }
                    
            // To create the randomess 
            def swapRows(board: Array[Array[Int]], row1: Int, row2: Int): Unit = 
                val temp = board(row1)
                board(row1) = board(row2)
                board(row2) = temp
            

            // To create randomness
            def swapColumns(board: Array[Array[Int]], col1: Int, col2: Int): Unit =
                for (row <- board.indices)
                    val temp = board(row)(col1)
                    board(row)(col1) = board(row)(col2)
                    board(row)(col2) = temp
            
            
            // Erase a given number of numbers from the board
            def eraseNumbers(board: Array[Array[Int]], count: Int): Array[Array[Int]] =
                val allPositions = 
                    for
                        row <- 0 until 9
                        col <- 0 until 9
                    yield (row, col)

                Random.shuffle(allPositions).take(count).foreach {
                case (row, col) => board(row)(col) = 0
                }

                board
            

            // Depending on the level, create the corresponding board 
            level match 
                case 1 => eraseNumbers(generateCompletedBoard(), 25).map(row => row.map(col => if col == 0 then Some(0, false, Nil) else Some(col, true, Nil)))
                case 2 => eraseNumbers(generateCompletedBoard(), 35).map(row => row.map(col => if col == 0 then Some(0, false, Nil) else Some(col, true, Nil)))
                case 3 => eraseNumbers(generateCompletedBoard(), 45).map(row => row.map(col => if col == 0 then Some(0, false, Nil) else Some(col, true, Nil)))


            
        
        SudokuState(Board(initializeBoard()), user, false, None, None, Some(level))

        

    /**
      * To output the initial state when the sudoku starts.
      *
      * @param clients : the clients that play, in a sudoku there is only one
      * @return the initial state,  
      */
    override def init(clients: Seq[UserId]): SudokuState =
        require(clients.size == 1)

        SudokuState(Board(Array.fill(9, 9)(Some(0, false, Nil))), clients.head, false, None, None, None)


    // Failures in the Try must hold instances of AppException
    // (from Exceptions.scala under lib/shared/)
    /**
      * To handle the logic between states.
      *
      * @param state : the current state 
      * @param uid : the current user
      * @param event : the event that may change the state
      * @return the next state 
      */
    override def transition(state: SudokuState)(uid: UserId, event: SudokuEvent): Try[Seq[Action[SudokuState]]] =
    
        // This check if a game is won, meaning each row, each column and each 3x3 subgrid contains each opf the number between 1 and 9 (once) 
        def won(board : Array[Array[Option[Int]]]) : Boolean =
            val validSet = (1 to 9).toSet.map(i => Some(i))

            def checkRowsAndColumns(): Boolean = {
                (0 until 9).forall { i =>
                val rowSet = board(i).toSet
                val colSet = (0 until 9).map(board(_)(i)).toSet
                rowSet == validSet && colSet == validSet
                }
            }

            def checkSubgrids(): Boolean = {
                (0 until 9 by 3).forall { row =>
                (0 until 9 by 3).forall { col =>
                val subgrid = (0 until 3).flatMap { dr =>
                    (0 until 3).map { dc =>
                    board(row + dr)(col + dc)
                }
                }.toSet
                subgrid == validSet
            }
            }
            }
            checkRowsAndColumns() && checkSubgrids()

        
        // Handles the logic transition between the states
        Try{

            if won(state.board.numberOnly()) then throw IllegalMoveException("The game is finished")
            else 
                if !state.isFinished then 
                    if state.currentPlayer == uid then // Unecessary because in a sudoku there is only one player
                        event match
                            case SudokuEvent.Beginning(level) => 
                                List(Action.Render(initializeAll(uid, level)))
                            
                            case SudokuEvent.Select(x, y) => 
                                if x <  0 || y < 0 || x >=  9 || y >= 9 then throw IllegalMoveException("The cell does not exist")
                                else 
                                    List(Action.Render(SudokuState(state.board, uid, false, Some(x,y), None, state.level)))

                            case SudokuEvent.AnnotateWith(nb) => 
                                if nb < 1 || nb > 9 then throw IllegalMoveException("Number has to be between 1 and 9")

                                else 
                                    state.slectedCell match
                                        case Some(value) => 
                                            if state.board.isFixed(value._1, value._2) then throw IllegalMoveException("You cannot change the given numbers.")
                                            else 
                                                // Update the board 
                                                val selectedCell = state.board.get(value._1, value._2)
                                                val boardY = state.board.cells(value._1).updated(value._2, Some(selectedCell.get._1, selectedCell.get._2, (selectedCell.get._3 ++ (nb :: Nil)))) // Nil beacuse when put a number, erase all annotations, toSet to avoid duplications
                                                val boardU = state.board.cells.updated(value._1, boardY)
                                            
                                                List(Action.Render(SudokuState(Board(boardU), uid, false, None, None, state.level)))
                                        
                                        case None => throw IllegalMoveException("You should select a case first")


                                
                            case SudokuEvent.Input(nb) =>
                                if nb < 0 || nb > 9 then throw IllegalMoveException("Number has to be between 1 and 9")
                                else 
                                    state.slectedCell match
                                        case Some(value) => 
                                            if state.board.isFixed(value._1, value._2) then throw IllegalMoveException("You cannot change the given numbers.")

                                            else 

                                                // Update the board 
                                                val boardY = state.board.cells(value._1).updated(value._2, Some(nb, false, Nil)) // Nil beacuse when put a number, erase all annotations
                                                val boardU = state.board.cells.updated(value._1, boardY)

                                                if won(Board(boardU).numberOnly()) then List(Action.Render(SudokuState(Board(boardU), uid, true, None, None, state.level)))
                                                else List(Action.Render(SudokuState(Board(boardU), uid, false, None, None, state.level)))

                                        case None => throw IllegalMoveException("You should select a case first")


                            case SudokuEvent.Highlight(nb) => 
                                state.nbSelected match
                                    case None => List(Action.Render(SudokuState(state.board, uid, false, None, Some(nb), state.level)))
                                    case Some(value) => 
                                        if value != nb then List(Action.Render(SudokuState(state.board, uid, false, None, Some(nb), state.level))) 
                                        else List(Action.Render(SudokuState(state.board, uid, false, None, None, state.level)))
                                    
                            // This is a blank state, which is solely used for synchronous refresh when buttons are pressed  
                            case SudokuEvent.Refresh => List(Action.Render(state)) 
                                
                                


                    else throw NotYourTurnException()
            
                else List(Action.Alert("The game is finished"))
            
        
        }
    

    /**
      * This will output the corresponding user view based on the current state for the user
      *
      * @param state : the current state
      * @param uid : the user id 
      * @return the corresponding view
      */
    override def project(state: SudokuState)(uid: UserId): SudokuView =
        if state.isFinished then SudokuView.Finished
        else 
            state.level match 
                case None => SudokuView.Begin
                case Some(value) => 
                    state.nbSelected match
                        case None => SudokuView.Playing(state.board)
                        case Some(value) => SudokuView.Highlighted(value, state.board)
        
    

// Server registration magic
class register: 
    WebServer.register(SudokuStateMachine)

