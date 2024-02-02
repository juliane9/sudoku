package project

import ujson.*
import scala.util.{Failure, Success, Try}

import cs214.webapp.wires.*
import cs214.webapp.exceptions.DecodingException
import cs214.webapp.{AppWire, WireFormat, UserId}
import scala.collection.mutable.ArrayBuffer

object SudokuWire extends AppWire[SudokuEvent, SudokuView]:
  import SudokuEvent.*
  import SudokuWire.*

  override object eventFormat extends WireFormat[SudokuEvent]:
    override def encode(t: SudokuEvent): Value = t match
      case Select(x: Int, y: Int) => ujson.Arr(ujson.Str("Select"), ujson.Num(x), ujson.Num(y))
      case Input(nb: Int) => ujson.Arr(ujson.Str("Input"), ujson.Num(nb))
      case Highlight(nb: Int) => ujson.Arr(ujson.Str("Highlight"), ujson.Num(nb))
      case Beginning(level) => ujson.Arr(ujson.Str("Beginning"), ujson.Num(level))
      case AnnotateWith(nb) => ujson.Arr(ujson.Str("Annotate"), ujson.Num(nb))
      case Refresh => ujson.Arr(ujson.Str("Refresh"))

    override def decode(json: Value): Try[SudokuEvent] =
      Try{
        val arr = json.arr
        val ty = arr(0).str
        if ty == "Select" then 
            val x = arr(1).num.toInt
            val y = arr(2).num.toInt

            if !x.isValidInt || !y.isValidInt then throw IllegalArgumentException()
            else Select(x, y)

        else if ty == "Input" then 
            val fst = (arr(1).num)
            //println(fst)
            if !fst.isValidInt then
                throw IllegalArgumentException()
            Input(fst.toInt)
        
        else if ty == "Highlight" then 
          val fst = (arr(1).num)

          if  !fst.isValidInt then 
            throw IllegalArgumentException()
          Highlight(fst.toInt)

        else if ty == "Beginning" then 
          val fst = (arr(1).num)

          if  !fst.isValidInt then 
            throw IllegalArgumentException()
          Beginning(fst.toInt)

        else if ty == "Annotate" then 
          val fst = (arr(1).num)
          
          if  !fst.isValidInt then 
            throw IllegalArgumentException()
          AnnotateWith(fst.toInt)
      
        else if ty == "Refresh" then Refresh

        else throw IllegalArgumentException()
      }
  

  override object viewFormat extends WireFormat[SudokuView]:
    extension (that: SudokuView)
      def tag =
        that match
          case SudokuView.Playing(board: Board) => "Playing"
          case SudokuView.Highlighted(nb, board) => "High"
          case SudokuView.Finished => "Finished"
          case SudokuView.Begin => "Begin"

    def encode(t: SudokuView): Value =
      ujson.Obj("tag" -> ujson.Str(t.tag), 
      "value" -> { t match
        case SudokuView.Playing(board) => ujson.Obj(
           "board" -> ujson.Arr(board.cells.map(row => ujson.Arr(row.map( col => ujson.Arr(
            ujson.Num(col.get._1), ujson.Bool(col.get._2), 
            ujson.Arr(col.get._3.map(ujson.Num(_)): _*)
            ))
              : _*) ): _* )
    )
        case SudokuView.Finished => ujson.Str("Finished")

        case SudokuView.Begin => ujson.Str("Begin")



        case SudokuView.Highlighted(nb, board) => ujson.Obj(
           "board" -> ujson.Arr(board.cells.map(row => ujson.Arr(row.map( col => ujson.Arr(
            ujson.Num(col.get._1), ujson.Bool(col.get._2), ujson.Arr(col.get._3.map(ujson.Num(_)): _*))
            )
              : _*) ): _* ), "nb" -> ujson.Num(nb)
    )
      })

  
    def decode(json: Value): Try[SudokuView] =
      Try{
       

        val obj = json.obj
        val v = obj("value")
        obj("tag").str match 
          case "Playing" => 
            val board = v("board")
            val c = board.arr.toArray
            val d = c.map(col => col.arr.toArray.map(el => 
              val e = el.arr.toArray
              val list = e(2).arr.toList.map(nb => nb.num.toInt)
              if e(0).num.isValidInt then Some(e(0).num.toInt, e(1).bool, list) else None))
            SudokuView.Playing(Board(d))
        
          
          case "High" => 
            val board = v("board")
            val c = board.arr.toArray
            val d = c.map(col => col.arr.toArray.map(el => 
              val e = el.arr.toArray
              val list = e(2).arr.toList.map(nb => nb.num.toInt)
              if e(0).num.isValidInt then Some(e(0).num.toInt, e(1).bool, list) else None))
            val nb = v("nb").num
            SudokuView.Highlighted(nb.toInt, Board(d))

          case "Finished" => SudokuView.Finished

          case "Begin" => SudokuView.Begin
        }
