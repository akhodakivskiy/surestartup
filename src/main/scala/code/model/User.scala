package code.model

import code.lib.Validator
import net.liftweb.util.{Helpers, BCrypt}
import org.joda.time.DateTime

import scala.slick.driver.PostgresDriver.simple._
import code.lib.Slick._
import Database.dynamicSession

import scala.xml.Text

case class UserId(value: Long) extends MappedTo[Long] with GenericId

object UserId {
  val empty = UserId(-1)
}

case class User(
  email: Option[String]
, password: String = ""
, first: String = ""
, last: String = ""
, createdAt: DateTime = DateTime.now
, updatedAt: DateTime = DateTime.now
, isActive: Boolean = false
, isRemoved: Boolean = false
, isAdmin: Boolean = false
, id: UserId = UserId.empty) {
  def withPassword(plain: String): User = copy(password = User.hash(plain))
  def checkPassword(plain: String): Boolean = User.check(plain, password)

  def name = s"$first $last".trim

  def touch: User = {
    val now = DateTime.now
    id match {
      case UserId.empty => copy(createdAt = now, updatedAt = now)
      case _ => copy(updatedAt = now)
    }
  }
}

object User {
  val logRounds = 10
  def hash(plain: String): String = BCrypt.hashpw(plain, BCrypt.gensalt(logRounds))
  def check(plain: String, hash: String): Boolean = BCrypt.checkpw(plain, hash)

  object validate {
    val email = Validator.email[User]("email", Text("Email address is not valid"))(_.email.getOrElse(""))
    val password = {
      Validator.minLength[User]("password", Text("Password should contain at least 6 characters"))(6, _.password) &
      Validator.maxLength[User]("password", Text("Password should contain no more than 32 characters"))(32, _.password)
    }
    val first = Validator.notEmpty[User]("first", Text("Please enter your first name"))(_.first)
    val last = Validator.notEmpty[User]("first", Text("Please enter your last name"))(_.last)

    val user = email & password & first & last
  }
}

class UsersTable(tag: Tag) extends Table[User](tag, "users") {
  val email = column[Option[String]]("email")
  val password = column[String]("password")
  val first = column[String]("first")
  val last = column[String]("last")
  val createdAt = column[DateTime]("created_at")
  val updatedAt = column[DateTime]("updated_at")
  val isActive = column[Boolean]("is_active")
  val isRemoved = column[Boolean]("is_removed")
  val isAdmin = column[Boolean]("is_admin")
  val id = column[UserId]("id", O.PrimaryKey, O.AutoInc)

  def emailIndex = index("idx__users__email", email, unique = true)

  def * = (email, password, first, last, createdAt, updatedAt, isActive, isRemoved, isAdmin, id) <>
    ((User.apply _).tupled, User.unapply)
}

object Users extends TableQuery(new UsersTable(_)) {
  val insertQuery = this returning this.map(_.id) into ((u, id) => u.copy(id = id))

  def insertOrUpdate(user: User): User = {
    val newUser = user.touch
    newUser.id match {
      case UserId.empty => insertQuery.insert(newUser)
      case id =>
        byId(id).update(newUser)
        newUser
    }
  }

  val byId = for {
    id <- Parameters[UserId]
    u <- Users if u.id === id && !u.isRemoved
  } yield u

  def find(id: UserId): Option[User] = byId(id).firstOption
  def find(id: String): Option[User] = Helpers.asLong(id).map(UserId.apply).toOption.flatMap(find)

  val byEmail = for {
    email <- Parameters[String]
    u <- Users if u.email.toLowerCase === email.toLowerCase && !u.isRemoved
  } yield u

  def findByEmail(email: String): Option[User] = {
    Option(email).filter(!_.isEmpty).flatMap(e => byEmail(email).firstOption)
  }

  def findByToken(token: String): Option[User] = None

  val byEmailAndActive = for {
    (email, active) <- Parameters[(String, Boolean)]
    u <- Users if u.email.toLowerCase === email.toLowerCase && !u.isRemoved && u.isActive === active
  } yield u

  def findByEmailAndActive(email: String, active: Boolean): Option[User] = {
    // handle empty string
    Some(email).filter(!_.isEmpty).flatMap(e => byEmailAndActive(e, active).firstOption)
  }

  def count: Long = Users.filter(!_.isRemoved).length.run

  def page(pageSize: Long, curPage: Long, sort: UsersTable => Column[_], asc: Boolean): Seq[User] = {
    Users.filter(!_.isRemoved)
      .sortBy(t => if (asc) sort(t).asc else sort(t).desc)
      .drop(pageSize * curPage).take(pageSize).list
  }
}
