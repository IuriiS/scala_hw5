package module3

import zio.ZIO
import zio.console.{getStrLn, putStrLn}
import zio.duration.durationInt
import zio.random.Random
import zio.test.Assertion.{anything, equalTo, hasSize, isSubtype, throws}
import zio.test.{DefaultRunnableSpec, ZSpec}
import zio.test._
import zio.test.environment._
import zio.test.TestAspect._

import scala.language.postfixOps
import java.io.IOException


object BasicZIOSpec extends DefaultRunnableSpec{

  val greeter: ZIO[zio.console.Console, IOException, Unit] = for{
    _ <- putStrLn("Как тебя зовут")
    name <- getStrLn
    _ <- putStrLn(s"Привет, $name")
  } yield ()


  val intGen: Gen[Random, Int] = Gen.anyInt

  override def spec = suite("Basic")(
    suite("Arithmetic")(
      test("2 * 2 = 4")(
        assert(2 * 2)(equalTo(4))
      ),
      test("division by zero"){
        assert(2 / 0)(throws(isSubtype[ArithmeticException](anything)))
      }
    ),
    suite("Property based testing"){
      testM("int addition is associative"){
        check(intGen, intGen, intGen){ (x, y, z) =>
          val left = (x + y) + z
          val right = x + (y + z)
          assert(left)(equalTo(right))
        }
      }
    },
    suite("Effect testing")(
      testM("simple effect")(
        assertM(ZIO.succeed(2 * 2))(equalTo(4))
      ),
       
      testM("test console")(
        for{
          _ <- TestConsole.feedLines("Alex")
          _ <- greeter
          value <- TestConsole.output
        } yield {
          assert(value)(hasSize(equalTo(2))) && assert(value(1))(equalTo("Привет, Alex\n"))
        }
      ),
      testM("test clock")(
        for{
          fiber <- (ZIO.sleep(3 seconds) *> putStrLn("Hello, world!")).fork
          _ <- TestClock.adjust(3 seconds)
          _ <- fiber.join
        } yield assertCompletes
      )
    )

  )

}
