object LengthOfLastWord extends App {
  /**
   * Problem statement:
   * Given a string s consists of upper/lower-case alphabets and empty space characters ' ',
   * return the length of last word (last word means the last appearing word if we loop from left to right) in the string.
   */

  println(lengthOfLastWord2("hi look some words"))

  def lengthOfLastWord1(s: String): Int = s.foldRight(0) {
    // this return seems really dirty to me
    // it terminates the foldRight early
    // are we only supposed to return in one place in Scala (and at the end of the function)?
    case (character, count) if character == ' ' && count > 0 => return count
    case (character, count) if character != ' ' => count + 1
    case (_, count) => count
    case _ => throw new RuntimeException
  }

  def lengthOfLastWord2(s: String): Int = s.trim.reverse.takeWhile(_ != ' ').foldRight(0) {
    // after reading
    // https://stackoverflow.com/questions/12892701/abort-early-in-a-fold
    // this seems the most elegant to me
    case (_, count) => count + 1
    case _ => throw new RuntimeException
  }

  def lengthOfLastWord3(s: String): Int = {
    // this solution seems really straight forward, but seems quite slow
    val sentence = s.split("\\s+")
    // is this type of return okay (two cases)?
    if (sentence.isEmpty) 0
    else sentence.last.length
  }

  def lengthOfLastWord4(s: String): Int = {
    // a one-liner that deals with whitespace differently than solution 2
    s.trim.split(' ').last.length
  }
}
