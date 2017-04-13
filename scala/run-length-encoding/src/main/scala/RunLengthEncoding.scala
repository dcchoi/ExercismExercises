import scala.annotation.tailrec

object RunLengthEncoding {

  def encode(str: String): String = {
    @tailrec
    def auxEncode(str:String, result: String): String = {
      str.toList match {
        case Nil => result
        case head :: tail => {
          auxEncode(str.span(_ == str.head)._2, result + (str.span(_ == str.head)._1.length match {
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
          auxDecode(str.span(_.isDigit)._2.tail, result + (str.span(_.isDigit)._1 match {
            case "" => str.span(_.isDigit)._2.head.toString()
            case x => str.span(_.isDigit)._2.head.toString() * x.toInt
          }))
        }
      }
    }
    auxDecode(str, "")
  }
}