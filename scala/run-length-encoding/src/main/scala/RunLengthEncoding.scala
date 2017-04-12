import scala.annotation.tailrec

object RunLengthEncoding {

  def encode(str: String): String = {

    @tailrec
    def auxEncode(str: String, currentLetter: String, count: Int,result: String): String = {
      if(str == ""){
        if(count == 1) return result + currentLetter
        else return result + count.toString + currentLetter
      }
      if(str.head.toString == currentLetter) return auxEncode(str.tail,currentLetter,count + 1,result)
      else if(str.head == ' ') return auxEncode(str.tail,"", 1 ,result + count.toString + currentLetter + " ")
      else{
        if(count == 1) return auxEncode(str.tail,str.head.toString,1,result + currentLetter)
        else return auxEncode(str.tail,str.head.toString,1,result + count.toString + currentLetter)
      }
    }
    auxEncode(str, "", 1, "")
  }

  def decode(str: String): String = {

    @tailrec
    def auxDecode(str: String, currentDigit: String, result: String): String ={
      if(str == "") result
      else if(str.head.isDigit){
        auxDecode(str.tail,currentDigit + str.head.toString, result)
      }else{
        if(currentDigit == "") auxDecode(str.tail, "", result + str.head)
        else {
          auxDecode(str.tail, "", result + (str.head.toString() * currentDigit.toInt))
        }
      }
    }
    auxDecode(str,"","")
  }
}
