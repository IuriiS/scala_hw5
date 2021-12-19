package module1

object functions {


  /**
   * Функции
   */

  // SAM

   def sum(x: Int, y: Int): Int = x + y

   val sum2: (Int, Int) => Int = (x ,y) => x + y

  sum(1 ,2) // 3

  sum2(1, 2) // 3

  val sum3: (Int, Int) => Int = sum _

  def foo(f: (Int, Int) => Int): Int = ???

  foo(sum)

  val list = List(sum3, sum2)

  sum3(1, 2)

  list.head(1, 2)





  // SAM Single Abstract Method


  trait Printer{
    def apply(s: String): Unit
  }

  val p: Printer = (s: String) => println(s)
  val p2: Printer = new Printer {
    override def apply(s: String): Unit = println(s)
  }

  p("sss")

  // Currying

  def foo: (Int, Int) => Int = (x, y) => x + y

  val f2: Int => Int => Int = foo.curried

  def fooCur: Int => Int => Int = x => y => x + y



  foo(1, 2)

  val ff = fooCur(1)


  /**
   *  Задание 1. Написать ф-цию метод isEven, которая будет вычислять является ли число четным
   */


  /**
   * Задание 2. Написать ф-цию метод isOdd, которая будет вычислять является ли число нечетным
   */



  /**
   * Задание 3. Написать ф-цию метод filterEven, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются четными
   */



  /**
   * Задание 3. Написать ф-цию метод filterOdd, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются нечетными
   */


  /**
   * return statement
   *
   */



}
