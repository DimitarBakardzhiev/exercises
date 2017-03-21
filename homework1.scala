object HelloWorld {
  def main(args: Array[String]): Unit = {
    val arr = List(1, 2, 3, 4, 5)
    val chars = List('a', 's', 'd')
    // println(length(arr, 0))
    // println(ifelse(1 > 2, 5, 10))
    // println(exist(arr, x => x > 2))
    // println(forall(arr, x => x > 3))
    // println(filter(arr, x => x > 1))
    // println(toUpperCase(chars))
    val expression = List('(', 's', ')')
    // println(balance(expression))
    println(pascal(2, 3))
  }

  def length(data: List[Int], count: Int): Int = {
    if (data.isEmpty) count
    else length(data.tail, count+1)
  }

  def ifelse(cond: Boolean, onTrue: => Int, onFalse: => Int) = {
    if (cond) onTrue
    else onFalse
  }

  def exist(data: List[Int], f: (Int) => Boolean): Boolean = {
    if (data.isEmpty) false
    else
      if (f(data.head)) true
      else exist(data.tail, f)
  }

  def forall(data: List[Int], f: (Int) => Boolean): Boolean = {
    if (data.isEmpty) true
    else
      if (!f(data.head)) false
      else exist(data.tail, f)
  }

  def filter(data: List[Int], f: (Int) => Boolean): List[Int]  = {
    if (data.isEmpty) data
    else
      if (f(data.head)) data.head :: filter(data.tail, f)
      else filter(data.tail, f)
  }

  def toUpperCase(chars: => List[Char]): List[Char] = {
    if (chars.isEmpty) chars
    // else Character.toUpperCase(chars.head)  :: toUpperCase(chars.tail)
    else {
      val ascii = chars.head - 32
      val char = ascii.toChar
      char :: toUpperCase(chars.tail)
    }
  }

  def balance(chars: List[Char]): Boolean = {
    var openBrackets: Int = 0
    _balance(chars, openBrackets)
  }

  def _balance(chars: List[Char], openBrackets: Int): Boolean = {
    if (openBrackets < 0) false
    if (chars.isEmpty) openBrackets == 0
    else {
      if (chars.head == '(') _balance(chars.tail, openBrackets + 1)
      else if (chars.head == ')') _balance(chars.tail, openBrackets - 1)
      else _balance(chars.tail, openBrackets)
    }
  }

  def pascal(c: Int, r: Int): Int = {
    factorial(r) / (factorial(c) * factorial(r - c))
  }

  def factorial(n: Int): Int = {
    if (n == 0 || n == 1) 1
    else n * factorial(n - 1)
  }
}
