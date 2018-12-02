import Tables._
import scala.io.Source
import scala.util.control.Breaks._

package LexicalAnalyzer{

  import scala.collection.mutable.ListBuffer
  import java.io.FileNotFoundException

  case class Token(key: Int, row: Int, column: Int)

  class LexicalAnalyzer(filename: String) {


    private var analyzeRes = ListBuffer[Token]()
    private var file: String = ""
    try {
      file = Source.fromFile(filename) getLines() mkString "\n"

      var state = State(0, 0, 0, 1)

      // tailrec
      while (state.iterator < file.length) {
        val ch = file.charAt(state.iterator)
        state = ch match {
          //if first symbol of the Token is digit
          case _ if isDigit(ch) =>
            asDigit(state)
          //if the first symbol of the Token is letter
          case _ if isInAlphabet(ch) =>
            asLet(state)
          case _ if ch == '(' =>
            asOpenedParenthenes(state)
          case _ if isDivider(ch) =>
            asDivider(state)
          case _ if ch == ' ' =>
            state.copy(iterator = state.iterator + 1)
          case _ if ch == '\n' =>
            state.copy(iterator = state.iterator + 1, row = state.row + 1, column = state.iterator + 1)
          case _ =>
            Error = true
            println("Unresolved symbol: (" + state.row + ", " + (state.iterator - state.column) + ")")
            state.copy(iterator = file.length)
        }
      }
    } catch {
      case ex: FileNotFoundException => Error = true
        println ("Missing file exception")
    }

    def getAnalyzeRes: List[Token] = analyzeRes.toList

    def printToken(t: Token): Unit = {
      println("<" + t.row.toString + ", " + t.column.toString +
        ">\t\t"+ t.key.toString+":       " + findInTables(t.key))
    }

    private def asDigit(state: State): State = {
      var i = state.iterator
      val column = state.column
      val currColumn = i - column
      val row = state.row

      var num: String = ""
      num += file.charAt(i)
      i += 1
      while (i < file.length && isDigit(file.charAt(i))) {
        num += file.charAt(i)
        i += 1
      }
      analyzeRes += Token(addConstant(num.toInt), row, currColumn)

      State(i, column, currColumn, row)
    }

    private def asLet(state: State): State = {
      var i = state.iterator
      val column = state.column
      val currColumn = i - column
      val row = state.row

      var word: String = ""
      word += file.charAt(i)
      i+=1
      while (i< file.length && (isInAlphabet(file.charAt(i)) || isDigit(file.charAt(i)))) {
        word += file.charAt(i)
        i += 1
      }
      if (isReservedWord(word)) {
        analyzeRes += Token(keyForResWord(word), row, currColumn)
      } else {
        analyzeRes += Token(addIdentifier(word), row, currColumn)
      }

      State(i, column, currColumn, row)
    }

    private def asDivider(state: State): State = {
      var i = state.iterator
      val column = state.column
      val currColumn = i - column
      val row = state.row
      analyzeRes += Token(getDividerIndex(file.charAt(i)), row, currColumn)
      i += 1
      State(i, column, currColumn, row)
    }

    private def asComment(state: State) : State = {
      var i = state.iterator
      val column = state.column
      val currColumn = i - column
      var row = state.row

      i += 1
      breakable{
        while (i+1 < file.length ) {
          if ('*' == file.charAt(i) && ')' == file.charAt(i+1))
            break
          if ('\n' == file.charAt(i))
            row += 1
          i += 1
        }
      }
      if (i+1 >= file.length){
        println("EOF found in comment block")
        Error = true
      }
      i += 2

      State(i, column, currColumn, row)
    }

    private def asAsm(state: State) : State = {

      var i = state.iterator
      val column = state.column
      val currColumn = i - column
      val row = state.row

      i += 1

      if (i<file.length && ' ' == file.charAt(i)){
        i += 1
        analyzeRes += Token(6, row, currColumn-2)
        if (isInAlphabet(file.charAt(i).toUpper)){
          var ident: String = ""
          breakable{
            while (i+2 < file.length &&
              " $)" != file.charAt(i).toString + file.charAt(i+1).toString + file.charAt(i+2).toString){
              if (isInAlphabet(file.charAt(i).toUpper) || isDigit(file.charAt(i)) || file.charAt(i) == '.')
                ident += file.charAt(i)
              else {
                Error = true
                println("Assembly file identifier error: invalid ident. (" + row + ", " + currColumn + ")")
                i = file.length
                break()
              }
              i += 1
            }
          }
          if(!Error && " $)" != file.charAt(i).toString + file.charAt(i+1).toString + file.charAt(i+2).toString){
            Error = true
            println("Assembly file identifier error: ' $)' expected. (" + row + ", " + currColumn + ")")
          } else {
            analyzeRes += Token(addIdentifier(ident), row, currColumn)
            analyzeRes += Token(7, row, i-column)
            i += 3
          }
        } else {
          Error = true
          println("Assembly file identifier error: invalid ident. (" + row + ", " + currColumn + ")")
          i = file.length
        }
      } else {
        Error = true
        println("Assembly file declaration error: no whitespace (" + row + ", " + currColumn + ")")
        i = file.length
      }
      State(i, column, currColumn, row)
    }

    private def asOpenedParenthenes(state: State): State = {
      var curState = state.copy(iterator = state.iterator + 1)
      (curState.iterator, file.charAt(curState.iterator)) match {
        case (i, ch) if i < file.length && '*' == ch =>
          curState = asComment(curState)
        case (i, ch) if i < file.length && '$' == ch =>
          curState = asAsm(curState)
        case _ =>
          // if there is only parentheses
          analyzeRes += Token(4, curState.row, curState.iterator - curState.column)
      }
      curState
    }

    def getFile: String = file

    def printAnalyzeRes(): Unit = {
      println("*"*50)
      println("\t\tTOKENS FOR ROW\\COLUMN")
      println("*"*50)
      analyzeRes.foreach(printToken)
      println("\n"+"*"*50)
    }
  }

  case class State(iterator: Int, column: Int, curColumn: Int, row: Int)
}

