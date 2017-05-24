package ex1

object Functions {

  // Връща големината на масив (без да ползва data.length!!!)
  def length(data: List[Int]) = {
    def _length(data: List[Int], count: Int): Int = {
      if (data.isEmpty) count
      else _length(data.tail, count + 1)
    }

    _length(data, 0)
  }

  // Ако cond е true връща onTrue
  def ifelse(cond: Boolean, onTrue: => Int, onFalse: => Int) = {
    if (cond) onTrue
    else onFalse
  }

  // Проверява дали скобите в даден масив от символи са балансирани.
  // Коректно: (a)asda(b)(v) | (((a))) | ()(()асдасд)
  // Грешно: )() | ((д) | ((das) (d)(
  def balance(chars: List[Char]): Boolean = {
    def _balance(chars: List[Char], openBrackets: Int): Boolean = {
      if (openBrackets < 0) false
      if (chars.isEmpty) openBrackets == 0
      else {
        if (chars.head == '(') _balance(chars.tail, openBrackets + 1)
        else if (chars.head == ')') _balance(chars.tail, openBrackets - 1)
        else _balance(chars.tail, openBrackets)
      }
    }

    _balance(chars, 0)
  }

  def map(chars: List[Char], f: Any) =  ???

  def toUpperCase(chars: List[Char]): List[Char] = {
    def upperCase(char: Char) = {
      val ascii = char - 32
      ascii.toChar
    }

    if (chars.isEmpty) chars
    else {
      upperCase(chars.head) :: toUpperCase(chars.tail)
    }
  }

  // Проверява дали съществува елемент отговарящ на f
  def exist(data: List[Int], f: (Int) => Boolean): Boolean = {
    if (data.isEmpty) false
    else
      if (f(data.head)) true
      else exist(data.tail, f)
  }

  // Връща масив съдържащ само елементите отговарящи на f
  def filter(data: List[Int], f: (Int) => Boolean): List[Int]  = {
    if (data.isEmpty) data
    else
      if (f(data.head)) data.head :: filter(data.tail, f)
      else filter(data.tail, f)
  }

  // Проверява дали всички елементи отговарят на f
  def forall(data: List[Int], f: (Int) => Boolean): Boolean = {
    if (data.isEmpty) true
    else
      if (!f(data.head)) false
      else exist(data.tail, f)
  }

  // Връща числото от триъгълника на Паскал отговарящо на съответния ред/колона
  def pascal(c: Int, r: Int): Int = {
    def factorial(n: Int): Int = {
      if (n == 0 || n == 1) 1
      else n * factorial(n - 1)
    }

    factorial(r) / (factorial(c) * factorial(r - c))
  }
}
