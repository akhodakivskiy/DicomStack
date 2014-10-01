package code.model

import code.lib._

import org.joda.time.DateTime
import scala.xml.Text

import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.util.Helpers._

import code.lib.validation._

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import Slick._

trait ResourceAccess

object NoAccess extends ResourceAccess
object AllAccess extends ResourceAccess

case class UserId(val value: Long) extends MappedTo[Long] with GenericId

object UserId {
  val empty = UserId(-1)
}

case class UserT(
  email: String
, password: String = ""
, first: String = ""
, last: String = ""
, active: Boolean = false
, createdAt: DateTime = DateTime.now
, id: UserId = UserId.empty
)

class UsersTable(tag: Tag) extends Table[UserT](tag, "users") {
  def email = column[String]("email")
  def password = column[String]("password")
  def first = column[String]("first")
  def last = column[String]("last")
  def active = column[Boolean]("is_active")
  def createdAt = column[DateTime]("created_at", O.DBType("TIMESTAMP WITH TIME ZONE"))

  def id = column[UserId]("id", O.PrimaryKey, O.AutoInc)

  def email_idx = index("user_email__idx", email, unique = true)

  def * = (email, password, first, last, active, createdAt, id) <>
          (UserT.tupled, UserT.unapply)
}

object Users extends TableQuery(new UsersTable(_)) with Validatable[UserT] {
  val queryForInsert = Users.returning(map(_.id))

  def insert(user: UserT) = {
    val id = queryForInsert.insert(user)
    user.copy(id = id)
  }

  def update(user: UserT) = Users.filter(_.id === user.id).update(user)

  val byId = for {
    id <- Parameters[UserId]
    u <- Users if u.id === id
  } yield u

  val byIdAndActive = for {
    (id, active) <- Parameters[(UserId, Boolean)]
    u <- Users if u.id === id && u.active === active
  } yield u

  def find(userId: UserId): Option[UserT] = byId(userId).firstOption
  def findInactive(userId: UserId) = byIdAndActive(userId, false).firstOption
  def findActive(userId: UserId) = byIdAndActive(userId, true).firstOption

  val byEmail = for {
    email <- Parameters[String]
    u <- Users if u.email === email
  } yield u

  val byEmailAndActive = for {
    (email, active) <- Parameters[(String, Boolean)]
    u <- Users if u.email === email && u.active === active
  } yield u

  def findByEmail(email: String) = byEmail(email).firstOption
  def findInactiveByEmail(email: String) = byEmailAndActive(email, false).firstOption
  def findActiveByEmail(email: String) = byEmailAndActive(email, true).firstOption

  object password extends Validatable[UserT] {
    def validators = List(
      valMinLength(_.password, 6, "password",
        Text("Password must containt at least 6 characters")),
      valMaxLength(_.password, 32, "password",
        Text("Password must containt no more than 32 characters"))
    )
  }

  object email extends Validatable[UserT] {
    def validators = List(
      valEmail(_.email, "email", Text("Not a valid email address"))
    )
  }

  def validators = List(
    valRequired(_.first, "first", Text("Please enter your Fist name"))
  , valRequired(_.last, "last", Text("Please enter your Last name"))
  ) ::: password.validators ::: email.validators

  private val logRounds = 10
  def checkpw(plain: String, password: String): Boolean =
    tryo(BCrypt.checkpw(plain, password)).openOr(false)
  def hashpw(plain: String): String =
    tryo(BCrypt.hashpw(plain, BCrypt.gensalt(logRounds))) openOr plain
}
