package code.model

import code.lib._

import org.joda.time.DateTime

import scala.xml.Text

import net.liftweb.util.Helpers._
import net.liftweb.common._

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import Slick._

object TokenType extends Enumeration {
  case class TokenType(name: String) extends Val(name)
  val Unknown = TokenType("unknown")
  val Activate = TokenType("activate")
  val ResetPassword = TokenType("reset-password")

  def findOption(s: String) : Option[TokenType] = {
    List(Unknown, Activate, ResetPassword).find(_.name == s)
  }

  implicit def userTokenColumnType = {
    MappedColumnType.base[TokenType, String](
      tokenType => tokenType.name,
      str => findOption(str).getOrElse(Unknown))
  }
}
import TokenType._

case class UserTokenId(val value: Long) extends MappedTo[Long] with GenericId

object UserTokenId {
  val empty = UserTokenId(-1)
}

case class UserTokenT(
  userId: UserId
, tokenType: TokenType
, active: Boolean = true
, token: String = UserTokens.nextToken
, createdAt: DateTime = DateTime.now
, id: UserTokenId = UserTokenId.empty
)

class UserTokensTable(tag: Tag) extends Table[UserTokenT](tag, "user_tokens") {
  def userId = column[UserId]("user_id")
  def tokenType = column[TokenType]("token_type")
  def active = column[Boolean]("is_active")
  def token = column[String]("token")
  def createdAt = column[DateTime]("created_at", O.DBType("TIMESTAMP WITH TIME ZONE"))

  def id = column[UserTokenId]("id", O.PrimaryKey, O.AutoInc)

  def userId_idx = index("user_tokens_user_id__idx", userId)
  def token_idx = index("user_tokens_token__idx", token)

  def user = foreignKey("user_tokens_user_id__fk", userId, Users)(_.id,
      onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

  def * = (userId, tokenType, active, token, createdAt, id) <>
          (UserTokenT.tupled, UserTokenT.unapply)
}

object UserTokens extends TableQuery(new UserTokensTable(_)) {
  val forInsert = this.returning(map(_.id))

  def insert(user: UserTokenT) = {
    val id = forInsert.insert(user)
    user.copy(id = id)
  }

  def update(token: UserTokenT) = {
    UserTokens.filter(_.id === token.id).update(token)
  }

  val byUserIdAndType = for {
    (userId, tokenType) <- Parameters[(UserId, TokenType)]
    t <- UserTokens if t.userId === userId && t.tokenType === tokenType && t.active
  } yield t

  def find(id: UserId, tokenType: TokenType) = {
    byUserIdAndType(id, tokenType).firstOption
  }

  val byTokenAndType = for {
    (token, tokenType) <- Parameters[(String, TokenType)]
    t <- UserTokens if t.token === token && t.tokenType === tokenType && t.active
  } yield t

  def find(token: String, tokenType: TokenType) = {
    byTokenAndType(token, tokenType).firstOption
  }

  def nextToken: String = hashHex(nextFuncName).take(32)

  def deactivateByUserId(id: UserId, tokenType: TokenType) = {
    UserTokens.filter(_.userId === id)
              .filter(_.tokenType === tokenType)
              .map(_.active).update(false)
  }
}
