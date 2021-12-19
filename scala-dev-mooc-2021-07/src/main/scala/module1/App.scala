package module1
import type_system.Rectangle
import module2.implicits
import zio.ZIO
import zio.ExitCode
import zio.ZEnv
import zio.URIO
import module3.zioOperators
import zio.Cause
import zio.console._
import zio.IO
import zio.duration._
import scala.language.postfixOps
import module3.zioConcurrency
import module3.zioDS



object App {

  def main(args: Array[String]): Unit = {


  

    zio.Runtime.default.unsafeRun(zioDS.ref.updateCounterRef.flatMap(c => putStrLn(c.toString())))
  }
}


object AppZIO extends zio.App{
  def run(args: List[String]): URIO[ZEnv, ExitCode] = ZIO.effect(println("Hello")).exitCode
}
