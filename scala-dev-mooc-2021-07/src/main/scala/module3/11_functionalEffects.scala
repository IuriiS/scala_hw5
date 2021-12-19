package module3

import scala.io.StdIn
import javax.naming.spi.DirStateFactory.Result

object functional_effects {


  object simpleProgram {

    val greet = {
      println("Как тебя зовут?")
      val name = StdIn.readLine()
      println(s"Привет, $name")
    }

    val askForAge = {
      println("Сколько тебе лет?")
      val age = StdIn.readInt()
      if (age > 18) println("Можешь проходить")
      else println("Ты еще не можешь пройти")
    }


    def greetAndAskForAge = ???


  }


  object functionalProgram {

    /**
     * Executable encoding and Declarative encoding
     */

    object executableEncoding {

      /**
       * 1. Объявить исполняемую модель Console
       */
      case class Console[A](unsafeRun: () => A){ self =>
            def map[B](f: A => B): Console[B] = 
                flatMap(a => Console.console(f(a)))

            def flatMap[B](f: A => Console[B]): Console[B] = 
                Console.console(f(self.unsafeRun()).unsafeRun())
      }


      /**
       * 2. Объявить конструкторы
       */

       object Console{
           def console[A](a: => A): Console[A] = Console( () => a)
           def printLine(str: String): Console[Unit] = Console(() => println(str))
           def readLine(): Console[String] = Console(() => StdIn.readLine()) 
       }



      /**
       * 3. Описать желаемую программу с помощью нашей модель
       */

       val p1: Console[Unit] = for{
        _ <- Console.printLine("Как тебя зовут?")
        str <- Console.readLine
        _ <- Console.printLine(s"Привет, $str")
       } yield ()

       p1.unsafeRun()


      /**
       * 4. Написать операторы
       */

    }


    object declarativeEncoding {

      /**
       * 1. Объявить декларативную модель Console
       */

      sealed trait Console[A]
      case class Println[A](str: String, rest: Console[A]) extends Console[A]
      case class ReadLine[A](f: String => Console[A]) extends Console[A]
      case class Return[A](v: () => A) extends Console[A]


    //   val p1 = Println(
    //       "Привет, как тебя зовут?",
    //       ReadLine()
    //   )

      /**
       * 2. Написать конструкторы
       * 
       */

       object Console{

            def succeed[A](v: => A): Console[A] = Return(() => v)
            def printLine(str: String): Console[Unit] = Println(str, succeed(()))
            def readLine: Console[String] = ReadLine(str => succeed(str))
       }



      /**
       * 3. Описать желаемую программу с помощью нашей модели
       */



      /**
       * 4. Написать операторы
       *
       */

       object consoleOps {
           implicit class ConsoleOps[A](console: Console[A]){
               def map[B](f: A => B): Console[B] = 
                flatMap(v => Console.succeed(f(v)))
                //flatMap(f.andThen(Console.succeed(_)))

               def flatMap[B](f: A => Console[B]): Console[B] = console match {
                   case Println(str, rest) => 
                    Println(str, rest.flatMap(f))
                   case ReadLine(ff) =>
                    ReadLine(str => ff(str).flatMap(f))
                   case Return(v) => f(v())
               }
           }
       }

       import consoleOps._


        // val greet = {
        //     println("Как тебя зовут?")
        //     val name = StdIn.readLine()
        //     println(s"Привет, $name")
        // }
       
       val p1: Console[Unit] = for{
        _ <- Console.printLine("Как тебя зовут?")
        str <- Console.readLine
        _ <- Console.printLine(s"Привет, $str")
       } yield ()



      /**
       * 5. Написать интерпретатор для нашей ф-циональной модели
       *
       */

       def interpret[A](console: Console[A]): A = console match {
           case Println(str, rest) => 
            println(str)
            interpret(rest)
           case ReadLine(f) =>
            interpret(f(StdIn.readLine()))
           case Return(v) => v()
       }



       interpret(p1)

      /**
       * Реализуем туже прошрамму что и в случае с исполняемым подходом
       */

    }

  }

}