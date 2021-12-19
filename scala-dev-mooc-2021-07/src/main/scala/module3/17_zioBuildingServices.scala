package module3

import _root_.module3.userService.UserID
import _root_.module3.emailService.EmailService
import _root_.module3.userService.UserService
import zio.{Has, ZIO}
import _root_.module3.userDAO.UserDAO
import zio.ZLayer

object buildingZIOServices{

  lazy val app: ZIO[UserService with EmailService with zio.console.Console, Throwable, Unit] = UserService.notifyUser(UserID(1))
  lazy val appEnv: ZLayer[Any, Nothing, UserService with EmailService] = UserDAO.live >>> UserService.live ++ EmailService.live

  def main(args: Array[String]): Unit = {
     zio.Runtime.default.unsafeRun(app.provideSomeLayer[zio.console.Console](appEnv))
  }

}