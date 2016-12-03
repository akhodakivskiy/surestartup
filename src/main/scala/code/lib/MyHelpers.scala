package code.lib

import java.util.Properties
import javax.naming.{NameNotFoundException, Context}
import javax.naming.directory.InitialDirContext

import net.liftweb.common._
import net.liftweb.http.js.{JsCmds, JE}
import net.liftweb.http.{SHtml, S, SessionVar, SortedPaginatorSnippet}
import net.liftweb.util.Helpers._
import net.liftweb.util.{ClearNodes, PassThru}
import org.joda.time.DateTimeZone

import scala.xml._

object MyHelpers {
  lazy val idc = {
    val env = new Properties()
    env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.dns.DnsContextFactory")
    new InitialDirContext(env)
  }

  def dnsLookup(domain: String, recordType: String): Option[String] = {
    try {
      val attrs = idc.getAttributes(domain, Array(recordType))
      val attrOpt = Option(attrs.get("CNAME"))
      attrOpt.filter(_.size == 1).map(_.get(0).asInstanceOf[String])
    } catch {
      case e: NameNotFoundException => None
    }
  }
}

trait MySortedPaginatorSnippet[T, C] extends SortedPaginatorSnippet[T, C] {
  override def firstXml: NodeSeq = Unparsed("&laquo;")
  override def lastXml: NodeSeq = Unparsed("&raquo;")
  override def pageXml(newFirst: Long, ns: NodeSeq): NodeSeq = {
    <li>{super.pageXml(newFirst, ns)}</li>
  }
  override def zoomedPages = (
    List(curPage - 120, curPage - 20) ::: (curPage - 7 to curPage + 7).toList ::: List(curPage + 20, curPage + 120)
  ).filter(n => n >= 0 && n < numPages)

  override def paginate(xhtml: NodeSeq): NodeSeq = {
    def hintHtml(asc: Boolean) = {
      val cls = asc match {
        case true => "fa fa-sort-asc"
        case false => "fa fa-sort-desc"
      }
      <span class={cls}></span>
    }

    val funcs = headers.zipWithIndex.map { case ((colName, _), colIdx) =>
      TheBindParam(colName, sort match {
        case (idx, asc) if idx == colIdx => hintHtml(asc)
        case _ => NodeSeq.Empty
      })
    }
    bind("hint", super.paginate(xhtml), funcs.toSeq : _*)
  }
}

object Jstz extends Loggable {
  object localTimeZone extends SessionVar[Box[DateTimeZone]](Empty)

  def render = {
    val defaultTz = DateTimeZone.forTimeZone(S.timeZone)
    localTimeZone.get match {
      case Empty => {
        localTimeZone(Full(defaultTz))
        S.appendJs {
          val determineCmd = JE.Call("jstz.determine") ~> JE.JsFunc("name")
          SHtml.ajaxCall(determineCmd, { tzId =>
            val tz = tryo(logger.info(_:Throwable)) {
              DateTimeZone.forID(tzId)
            } openOr(defaultTz)
            localTimeZone(Full(tz))
            JsCmds.Noop
          }).cmd
        }
        PassThru
      }
      case _ => ClearNodes
    }
  }
}
