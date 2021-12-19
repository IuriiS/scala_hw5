package module3

import zio.Has
import zio.Task
import userService.{User, UserID}
import zio.{ZLayer, ULayer}

package object userDAO {

    type UserDAO = Has[UserDAO.Service]

    object UserDAO{
        trait Service{
            def list(): Task[List[User]]
            def findBy(id: UserID): Task[Option[User]]
        }

        val live: ZLayer[Any, Nothing, UserDAO] = ???
    }
  
}
