import scala.annotation.tailrec

object RunLengthEncoding {

  def encode(str: String): String = {
    @tailrec
    def auxEncode(str:String, result: String): String = {
      str.toList match {
        case Nil => result
        case head :: tail => {
          val spannedStr = str.span(_ == str.head)
          auxEncode(spannedStr._2, result + (spannedStr._1.length match {
            case 1 => ""
            case x => x.toString
          }) + str.head )
        }
      }
    }
    auxEncode(str, "")
  }



  def decode(str: String): String = {
    @tailrec
    def auxDecode(str:String, result: String): String = {
      str.toList match {
        case Nil => result
        case head :: tail => {
          val spannedStr =str.span(_.isDigit)
          auxDecode(spannedStr._2.tail, result + (spannedStr._2.head.toString() * (spannedStr._1 match {
            case "" => 1
            case x => x.toInt
          })))
        }
      }
    }
    auxDecode(str, "")
  }
}