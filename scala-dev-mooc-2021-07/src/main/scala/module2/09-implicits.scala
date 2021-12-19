package module2

object implicits {


  // implicit conversions

  object implicit_conversions{
    /**
     * Расширить возможности типа String, методом trimToOption, который возвращает Option[String]
     * если строка пустая или null, то None
     * если нет, то Some от строки со всеми удаленными начальными и конечными пробелами
     *
     */

    //  "str".trimToOption //Some("str")
    //  "".trimToOption // None
    //  " ".trimToOption // None


    //  implicit def strToStringOps(str: String): StringOps = ???

    //  class StringOps(str: String){
    //      def trimToOption: Option[String] = 
    //         Option(str)
    //             .map(_.trim())
    //             .filter(_.nonEmpty)
    //  }





    // implicit conversions ОПАСНЫ

    implicit def strToInt(str: String): Int = Integer.parseInt(str)

     "foo" / 42

    implicit val seq = Seq("a", "b", "c") // f: Int => String

    def log(str: String) = println(str)

    log(42)




    // view bounds

    // Создать класс Ordering, который позволит нам сравнивать различные типы

    type Ordering
  }



  object implicit_scopes {


    trait A

    trait B[T] extends A {
      def print: Unit
    }

    object A {
     implicit val v: B[Bar] = new B[Bar]{
       override def print: Unit = println("companion object A")
     }
    }

    // companion object B
    object B{
      implicit val v: B[Bar] = new B[Bar]{
        override def print: Unit = println("companion object B")
      }
    }


    case class Bar()

    // companion object Bar
    object Bar{
    //   implicit val v: B[Bar] = new B[Bar] {
    //     override def print: Unit = println("Bar companion")
    //   }
    }

    // some arbitrary object
    object wildcardImplicits {
      implicit val v: B[Bar] = new B[Bar] {
        override def print: Unit = println("from wildcard import")
      }
    }


    def foo(b: Bar)(implicit m: B[Bar]) = m.print

    //import wildcardImplicits._

    // implicit val v: B[Bar] = new B[Bar] {
    //     override def print: Unit = println("from local scope")
    //   }


    // 1. Local scope
    // 2. Wildcard import 1 | 2
    // 3. package object
    // 4. Bar
    // 5. B Bar | B
    // 6. A

    foo(Bar())

  }

}