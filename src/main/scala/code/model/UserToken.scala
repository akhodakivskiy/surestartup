package code.model

import net.liftweb.util.{Helpers, BCrypt}
import org.joda.time.DateTime

import scala.slick.driver.PostgresDriver.simple._
import code.lib.Slick._
import Database.dynamicSession

object TokenType extends Enumeration {
  case class TokenType(name: String) extends Val(name)
  val Unknown = TokenType("unknown")
  val Activate = TokenType("activate")
  val ResetPassword = TokenType("reset-password")

  def findOption(s: String) : Option[TokenType] = {
    List(Unknown, Activate, ResetPassword).find(_.name == s)
  }

  implicit def userTokenColumnType: BaseColumnType[TokenType] = {
    MappedColumnType.base[TokenType, String](
      tokenType => tokenType.name,
      str => findOption(str).getOrElse(Unknown))
  }
}
import TokenType._

case class UserTokenId(value: Long) extends MappedTo[Long] with GenericId

object UserTokenId {
  val empty = UserTokenId(-1)
}

case class UserToken(
  userId: UserId
, tokenType: TokenType
, active: Boolean = true
, token: String = UserToken.nextToken
, createdAt: DateTime = DateTime.now
, updatedAt: DateTime = DateTime.now
, id: UserTokenId = UserTokenId.empty) {
  def touch: UserToken = {
    val now = DateTime.now
    id match {
      case UserTokenId.empty => copy(createdAt = now, updatedAt = now)
      case _ => copy(updatedAt = now)
    }
  }
}

object UserToken {
  val logRounds = 10
  def nextToken: String = Helpers.hashHex(BCrypt.hashpw(Helpers.nextFuncName, BCrypt.gensalt(10)))
}

class UserTokensTable(tag: Tag) extends Table[UserToken](tag, "user_tokens") {
  val userId = column[UserId]("user_id")
  val tokenType = column[TokenType]("token_type")
  val active = column[Boolean]("is_active")
  val token = column[String]("token")
  val createdAt = column[DateTime]("created_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  val updatedAt = column[DateTime]("updated_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  val id = column[UserTokenId]("id", O.PrimaryKey, O.AutoInc)

  def userId_idx = index("idx__user_tokens__user_id", userId)
  def token_idx = index("idx__user_tokens__token", token)

  def user = foreignKey("fk__user_tokens__user_id", userId, Users)(_.id,
    onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

  def * = (userId, tokenType, active, token, createdAt, updatedAt, id) <>
    ((UserToken.apply _).tupled, UserToken.unapply)
}

object UserTokens extends TableQuery(new UserTokensTable(_)) {
  val insertQuery = this returning this.map(_.id) into ((t, id) => t.copy(id = id))

  def insertOrUpdate(token: UserToken): UserToken = {
    val newToken = token.touch
    newToken.id match {
      case UserTokenId.empty => insertQuery.insert(newToken)
      case id =>
        byId(id).update(newToken)
        newToken
    }
  }

  val byId = for {
    id <- Parameters[UserTokenId]
    t <- UserTokens if t.id === id
  } yield t

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

  def find(token: String, tokenType: TokenType): Option[UserToken] = {
    byTokenAndType(token, tokenType).firstOption
  }

  def deactivate(id: UserId, tokenType: TokenType) = {
    UserTokens.filter(_.userId === id)
      .filter(_.tokenType === tokenType)
      .map(t => (t.updatedAt, t.active)).update((DateTime.now, false))
  }

  def newToken(userId: UserId, tokenType: TokenType): UserToken = {
    this.deactivate(userId, tokenType)
    insertOrUpdate(UserToken(userId, tokenType))
  }
}
