/**
  * Created by dccho on 3/28/2017.
  */
class Bob {
  def hey(str: String): String = {
    if(str.trim.isEmpty)
      "Fine. Be that way!"
    else if(str.toUpperCase() == str && str.filter(_.isLetter).length > 0)
      "Whoa, chill out!"
    else if(str.endsWith("?"))
      "Sure."
    else
      "Whatever."
  }
}
