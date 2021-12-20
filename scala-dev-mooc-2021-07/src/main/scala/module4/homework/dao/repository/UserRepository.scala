package module4.homework.dao.repository

import zio.Has
import doobie.quill.DoobieContext
import io.getquill.CompositeNamingStrategy2
import io.getquill.Escape
import io.getquill.Literal
import module4.phoneBook.db.DBTransactor
import zio.Task
import module4.homework.dao.entity.User
import zio.macros.accessible
import zio.{ZLayer, ULayer}
import module4.homework.dao.entity.{Role, UserToRole}
import module4.homework.dao.entity.UserId
import module4.homework.dao.entity.RoleCode

object UserRepository {


  val dc: DoobieContext.Postgres[CompositeNamingStrategy2[Escape.type, Literal.type]] = DBTransactor.doobieContext

  import dc._

  type UserRepository = Has[Service]

  trait Service {
    def findUser(userId: UserId): Result[Option[User]]

    def createUser(user: User): Result[User]

    def createUsers(users: List[User]): Result[List[User]]

    def updateUser(user: User): Result[Unit]

    def deleteUser(user: User): Result[Unit]

    def findByLastName(lastName: String): Result[List[User]]

    def list(): Result[List[User]]

    def userRoles(userId: UserId): Result[List[Role]]

    def insertRoleToUser(roleCode: RoleCode, userId: UserId): Result[Unit]

    def listUsersWithRole(roleCode: RoleCode): Result[List[User]]

    def findRoleByCode(roleCode: RoleCode): Result[Option[Role]]
  }

  class ServiceImpl extends Service {

    val userSchema = quote {
      querySchema[User](""""User"""")
    }

    def batchInsertUsers(users: List[User]) = quote {
      liftQuery(users).foreach(u => query[User].insert(u).returning(u => u))
    }

    val roleSchema = quote {
      querySchema[Role](""""Role"""")
    }

    val userToRoleSchema = quote {
      querySchema[UserToRole](""""UserToRole"""")
    }

    def findUser(userId: UserId): Result[Option[User]] = dc.run(userSchema.filter(_.id == lift(userId.id)))
      .map(_.headOption)

    def createUser(user: User): Result[User] = dc.run(userSchema.insert(lift(user)).returning(u => u))

    def createUsers(users: List[User]): Result[List[User]] = dc.run(batchInsertUsers(users))

    def updateUser(user: User): Result[Unit] =
      dc.run(userSchema.filter(_.id == lift(user.id)).update(lift(user))).map(_ => ())

    def deleteUser(user: User): Result[Unit] =
      dc.run(userSchema.filter(_.id == lift(user.id)).delete).map(_ => ())

    def findByLastName(lastName: String): Result[List[User]] =
      dc.run(userSchema.filter(_.lastName == lift(lastName)))

    def list(): Result[List[User]] = dc.run(userSchema)

    def userRoles(userId: UserId): Result[List[Role]] = dc.run(
      userToRoleSchema.join(roleSchema).on(_.roleId == _.code)
        .filter(_._1.userId == lift(userId.id))).map(_.map(_._2))

    def insertRoleToUser(roleCode: RoleCode, userId: UserId): Result[Unit] =
      dc.run(userToRoleSchema.insert(lift(UserToRole(roleCode.code, userId.id)))).map( _ => ())

    def listUsersWithRole(roleCode: RoleCode): Result[List[User]] = dc.run(
      userToRoleSchema.filter(_.roleId == lift(roleCode.code)).join(userSchema).on(_.userId == _.id)).map(_.map(_._2))

    def findRoleByCode(roleCode: RoleCode): Result[Option[Role]] =
      dc.run(roleSchema.filter(_.code == lift(roleCode.code))).map(_.headOption)

  }

  val live: ULayer[UserRepository] = ZLayer.succeed(new ServiceImpl)
}