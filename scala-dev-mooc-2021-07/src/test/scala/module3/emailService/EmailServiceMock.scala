package module3.emailService

import zio.test.mock.mockable
import zio.test.mock
import zio.{URLayer, Has}
import zio.ZLayer
import zio.URIO
import zio.UIO

object EmailServiceMock extends mock.Mock[EmailService]{

    object SendMail extends Effect[Email, Nothing, Unit]
    val compose: URLayer[Has[mock.Proxy], EmailService] = ZLayer.fromService{ proxy =>
            new EmailService.Service {
                def sendMail(email: Email): URIO[zio.console.Console, Unit] = proxy(SendMail, email)
            }
    }
}