package code.model

import org.joda.time.DateTime

import scala.slick.driver.PostgresDriver.simple._
import code.lib.Slick._
import Database.dynamicSession

case class UserAuthInfoId(value: Long) extends MappedTo[Long] with GenericId

object UserAuthInfoId {
  val empty = UserAuthInfoId(-1)
}

object AuthInfoProvider extends Enumeration {
  case class AuthInfoProvider(name: String) extends Val(name)
  val Unknown = AuthInfoProvider("unknown")
  val Facebook = AuthInfoProvider("facebook")
  val Twitter = AuthInfoProvider("twitter")

  def findOption(s: String) : Option[AuthInfoProvider] = {
    List(Unknown, Facebook, Twitter).find(_.name == s)
  }

  implicit def userTokenColumnType: BaseColumnType[AuthInfoProvider] = {
    MappedColumnType.base[AuthInfoProvider, String](
      provider => provider.name,
      str => findOption(str).getOrElse(Unknown))
  }
}
import AuthInfoProvider._

case class UserAuthInfo(
  provider: AuthInfoProvider
, uid: String
, nickname: Option[String]
, userId: UserId
, createdAt: DateTime = DateTime.now
, updatedAt: DateTime = DateTime.now
, id: UserAuthInfoId = UserAuthInfoId.empty) {
  def touch: UserAuthInfo = {
    val now = DateTime.now
    id match {
      case UserAuthInfoId.empty => copy(createdAt = now, updatedAt = now)
      case _ => copy(updatedAt = now)
    }
  }
}

class UserAuthInfosTable(tag: Tag) extends Table[UserAuthInfo](tag, "user_auth_infos") {
  val provider = column[AuthInfoProvider]("provider", O.Length(32, varying = true))
  val uid = column[String]("uid", O.Length(128, varying = true))
  val nickname = column[Option[String]]("nickname")
  val userId = column[UserId]("user_id")
  val createdAt = column[DateTime]("created_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  val updatedAt = column[DateTime]("updated_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  val id = column[UserAuthInfoId]("id", O.PrimaryKey, O.AutoInc)

  def userId_idx = index("idx__user_auth_infos__user_id", userId)

  def user = foreignKey("fk__user_auth_infos__user_id", userId, Users)(_.id,
    onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

  def * = (provider, uid, nickname, userId, createdAt, updatedAt, id) <>
    ((UserAuthInfo.apply _).tupled, UserAuthInfo.unapply)
}

object UserAuthInfos extends TableQuery(new UserAuthInfosTable(_)) {
  val insertQuery = this returning this.map(_.id) into ((t, id) => t.copy(id = id))

  def insertOrUpdate(authInfo: UserAuthInfo): UserAuthInfo = {
    val newAuthInfo = authInfo.touch
    newAuthInfo.id match {
      case UserAuthInfoId.empty => insertQuery.insert(newAuthInfo)
      case id =>
        byId(id).update(newAuthInfo)
        newAuthInfo
    }
  }

  val byId = Compiled { id: ConstColumn[UserAuthInfoId] =>
    this.filter(_.id === id)
  }

  val byUidAndProvider = Compiled { (uid: ConstColumn[String], provider: ConstColumn[AuthInfoProvider]) =>
    this.filter(a => a.uid === uid && a.provider === provider)
  }

  def findByUidAndProvider(uid: String, provider: AuthInfoProvider): Option[UserAuthInfo] = {
    byUidAndProvider(uid, provider).list.headOption
  }

  def findByUserIds(userIdSet: Traversable[UserId]): List[UserAuthInfo] = {
    this.filter(_.userId inSetBind userIdSet).list
  }
}
