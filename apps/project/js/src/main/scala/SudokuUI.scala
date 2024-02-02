package project

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

import org.scalajs.dom
import scalatags.JsDom.all.*

import cs214.webapp.*
import cs214.webapp.client.*
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import cs214.webapp.exceptions.IllegalMoveException
import javax.swing.text.Highlighter.Highlight



object SudokuClientApp extends WSClientApp:
  def name: String = "sudoku"

  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element): ClientAppInstance =
    SudokuClientAppInstance(userId, sendMessage, target)



class SudokuClientAppInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element)
    extends StateMachineClientAppInstance[SudokuEvent, SudokuView](userId, sendMessage, target):
  def name: String = "sudoku"

  // Used to encode and decode with json between the state machine and the server 
  val wire = SudokuWire

  // Create the title of the page
  def render(userId: UserId, view: SudokuView): Frag =
    frag(
      h2(b("Sudoku: Fill in the cases with numbers 1 to 9!")),
      renderView(userId, view)
    )
    
  // Transform a grid coordinate (row, column) to an index
  private def subgridIndex(row: Int, col: Int): Int =
    (row / 3) * 3 + (col / 3)


  private var annotationMode: Boolean = false

// Color matcher for cells given an index in the grid (to hghlight the 3 by 3 subgrid of the sudoku)
  private def colorForSubgrid(index: Int): String = index match 
    case 0 => "lightblue"
    case 1 => "paleturquoise"
    case 2 => "cornflowerblue"
    case 3 => "dodgerblue"
    case 4 => "lightpink"
    case 5 => "lightgray"
    case 6 => "lavender"
    case 7 => "powderblue"
    case 8 => "lightcyan"
    case _ => "white"
  

  /**
    * This creates the layout for the 9 buttons and the sudoku grid and handles the input in the cells as well as the selction of buttons
    *
    * @param wasNumberSelected : whether the current event is the selction of one of the 9 buttons 
    * @param board : the current sudoku grid 
    * @param nbSelected : which number button was selected, if any (else None)
    * @return the layout of the 9 buttons and the sudoku grid 
    */
  private def buttonsAndGridLayout(wasNumberSelected: Boolean, board: Board, nbSelected: Option[Int]) : Frag =
    // Styles 
      val gridStyle = "display: grid; grid-template-columns: repeat(9, 1fr); gap: 1px; max-width: 450px; margin: auto;"

      val buttonsContainerStyle = "display: flex; justify-content: center; max-width: 450px; margin: auto;"
      val buttonStyle = "flex: none; width: 51px; height: 51px; " +
                    "font-size: 16px; text-align: center; line-height: 51px; margin: -4px;" 

      val annotateButtonStyle = "margin: 10px; padding: 5px 10px;" // Example style
      val colorAnnotateButton = if annotationMode then " background-color: gray;" else " background-color: gainsboro;" 

  
      
      // Page layout with the 9 clikeable buttons and the sudoku grid 
      div(
        p(" "),
        p(i("You need to double-click on a cell before typing a number to annotate or place a number in a cell.")), 
        p(" "),
        
        // Buttons layout
        div(
          cls := "number-buttons-container",
          style := buttonsContainerStyle,
          (1 to 9).map(num =>
            button(
              num.toString,
              style := buttonStyle,
              onclick := (() =>
                sendEvent(SudokuEvent.Highlight(num))
              )
            )
          )
        ),

        p(" "), // to make space between the buttons and the grid 

      // Sudoku grid layout and handler for inputs
      div(
        style := gridStyle,

        (0 until 9).flatMap (row =>
          (0 until 9).map (col =>

            // Style for each cell 
            val isCellFixed = board.isFixed(row, col) // Check whether a cell is fixed or is the result of the user's input
            val cellStyle = 
              val baseStyle =
                if isCellFixed then "width: 50px; height: 50px; text-align: center; line-height: 50px; font-weight: bold; color: black;"
                else 
                  if ! board.get(row, col).get._3.isEmpty then "width: 50px; height: 50px; text-align: center; line-height: 50px; font-weight: normal; color: green; font-size: 0.7em;"
                  else "width: 50px; height: 50px; text-align: center; line-height: 50px; font-weight: normal; color: blue;"

              // Create the color of the 3x3 subgrids
              val color = 
                if wasNumberSelected then 
                  if board(row, col).get == nbSelected.get then "yellow" else colorForSubgrid(subgridIndex(row, col))
                else colorForSubgrid(subgridIndex(row, col))

              baseStyle + s" background-color: $color;"

            // Handles the possible input in the cells of the grid
            input(
              `type` := "text",
              style := cellStyle, 
              maxlength := "1",
              value := board(row, col).map(nb => 
                if nb == 0 then
                  if 
                    ! board.get(row, col).get._3.isEmpty then board.get(row, col).get._3.mkString(" ")
                  else " " 
                else nb).fold(" ")(_.toString),

              oninput := { (e: dom.Event) =>
              val inputElem = e.target.asInstanceOf[dom.html.Input]
              Try(inputElem.value.toInt) match {
                case Success(num) if num >= 0 && num <= 9 =>
                  if (annotationMode) then 
                    sendEvent(SudokuEvent.Select(row, col))
                    sendEvent(SudokuEvent.AnnotateWith(num))
                  else
                    sendEvent(SudokuEvent.Select(row, col))
                    sendEvent(SudokuEvent.Input(num))
                  
                case Failure(exception) =>
                  inputElem.value = "" // Do nothing if the input is not a valid number
                case _ =>
                  inputElem.value = "" // Do nothing if the input is not a valid number
              }
            }  
            )
          )
        )

      ), 
      p(" "), 
      p(" "), 

        button(
          "Annotate",
          onclick := { () =>
            annotationMode = !annotationMode
            sendEvent(SudokuEvent.Refresh)
          },
          style := 
            annotateButtonStyle + colorAnnotateButton,
          ), 
          p(" ")
      )



  // Handles the different view based on the current view
  def renderView(userId: UserId, view: SudokuView): Frag = view match


    // First phase with the selection of the level
    case SudokuView.Begin => 

      // Style creation for level buttons
      val levelButtonStyle = "flex: none; width: 100px; height: 50px; " +
                           "font-size: 16px; text-align: center; line-height: 50px; margin: 10px;"

      val levelButtonsContainerStyle = "display: flex; justify-content: center; padding: 20px;"

      // Choosing-level page layout : creates 3 clickeable buttons that represent the 3 levels 
      div(
        h4(b("Choose your difficulty level : ")), 
        div(
          cls := "level-buttons-container",
          style := levelButtonsContainerStyle,
          (1 to 3).map(level =>
            button(
              s"Level $level",
              style := levelButtonStyle + 
                (level match 
                  case 1 => "background-color: green;"
                  case 2 => "background-color: orange;"
                  case 3 => "background-color: red;"
                  case _ => "" 
                ),
              onclick := ( () =>
                sendEvent(SudokuEvent.Beginning(level))
              )
            )
          )
        )
      )
      
    // Playing phase : contains a sudoku grid and 9 buttons which can be used to highlight an already placed number in the grid
    case SudokuView.Playing(board) =>
      buttonsAndGridLayout(false, board, None)


    // Phase when one of the number button is selected
    case SudokuView.Highlighted(nb, board)=> 
      buttonsAndGridLayout(true, board, Some(nb))
      
    // Phase when the game is finished and the player has finished the sudoku
    case SudokuView.Finished =>
      p(cls := "finished", "Congratulations! The sudoku is solved 🥳.")

   



// Scala.js magic to register our application from this file
@JSExportAll
object SudokuRegistration:
  @JSExportTopLevel("SudokuExport")
  val registration = WebClient.register(SudokuClientApp)
