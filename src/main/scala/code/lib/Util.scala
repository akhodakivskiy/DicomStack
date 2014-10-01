package code.lib

import code.model._

import java.io.BufferedInputStream
import java.awt.image.BufferedImage
import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.DateTimeFormat

import scala.concurrent._
import scala.collection.immutable.Stream
import scala.language.implicitConversions
import scala.collection.immutable.{SortedMap, TreeMap}
import scala.xml.{Node, NodeSeq, Text, Unparsed}

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._

object Util extends Loggable {
  def localTimeZone = DateTimeZone.forTimeZone(S.timeZone)

  object DTF {
    def utcShort = DateTimeFormat.shortDateTime.withZone(DateTimeZone.UTC)
    def localShort = DateTimeFormat.shortDateTime.withZone(localTimeZone)

    def utcMedium = DateTimeFormat.mediumDateTime.withZone(DateTimeZone.UTC)
    def localMedium = DateTimeFormat.mediumDateTime.withZone(localTimeZone)

    def utcLong = DateTimeFormat.longDateTime.withZone(DateTimeZone.UTC)
    def localLong = DateTimeFormat.longDateTime.withZone(localTimeZone)
  }
  object DF {
    def utcShort = DateTimeFormat.shortDate.withZone(DateTimeZone.UTC)
    def localShort = DateTimeFormat.shortDate.withZone(localTimeZone)

    def utcMedium = DateTimeFormat.mediumDate.withZone(DateTimeZone.UTC)
    def localMedium = DateTimeFormat.mediumDate.withZone(localTimeZone)

    def utcLong = DateTimeFormat.longDate.withZone(DateTimeZone.UTC)
    def localLong = DateTimeFormat.longDate.withZone(localTimeZone)
  }

  val unnamedSeries = <em>Unnamed Series</em>
  val unnamedStudy = <em>Unnamed Study</em>
  val unnamedPatient = <em>Unnamed Patient</em>

  def maybeUnnamed(name: String, node: Node): Node = {
    Option(name).map(_.trim) match {
      case Some(name) if (!name.isEmpty) => Text(name)
      case _ => node
    }
  }

  def elideMiddle(str: String, maxLength: Int): NodeSeq = (str.length <= maxLength) match {
    case true => Text(str)
    case false => Text(str.take(maxLength / 2)) ++
                  Unparsed("&hellip;") ++
                  Text(str.takeRight(maxLength / 2))
  }

  def elideRight(str: String, maxLength: Int): NodeSeq = (str.length <= maxLength) match {
    case true => Text(str)
    case false => Text(str.take(maxLength)) ++ Unparsed("&hellip;")
  }

  def orderedGroupBy[A, K, B](objs: Seq[(A, B)]): SortedMap[A, Seq[B]] = {
    val as = objs.map(_._1)
    implicit val orderingA = Ordering.by[A, Int](as.indexOf(_))

    def a(m: TreeMap[A, Seq[B]], n: (A, B)): TreeMap[A, Seq[B]] = {
      m.get(n._1) match {
        case None => {
          m + (n._1 -> List(n._2))
        }
        case Some(bs) => {
          logger.info(bs)
          m + (n._1 -> (bs :+ n._2))
        }
      }
    }
    objs.foldLeft(TreeMap[A, Seq[B]]())(a)
  }

  def humanReadableByteCount(bytes: Long, si: Boolean = true): String = {
    val unit = if (si) 1000 else 1024
    bytes match {
      case x if x < unit => s"$bytes B"
      case _ => {
        val exp = (math.log(bytes) / math.log(unit)).toInt
        val pre = (if (si) "kMGTPE" else "KMGTPE").charAt(exp-1) + (if (si) "" else "i")
        val v = bytes / math.pow(unit, exp)
        f"$v%.1f ${pre}B"
      }
    }
  }

  def resizeImage(image: BufferedImage, maxDimension: Int): BufferedImage = {
    import java.awt.RenderingHints._
    import java.awt.AlphaComposite

    val dimension = if (image.getWidth > image.getHeight) image.getWidth else image.getHeight
    val factor = maxDimension.toDouble / dimension.toDouble

    if (factor >= 1) {
      return image
    } else {
      val (w, h) = ((image.getWidth * factor).toInt, (image.getHeight * factor).toInt)
      val resizedImage = new BufferedImage(w, h, image.getType)
      val g = resizedImage.createGraphics()
      g.setRenderingHint(KEY_INTERPOLATION, VALUE_INTERPOLATION_BILINEAR)
      g.setRenderingHint(KEY_RENDERING, VALUE_RENDER_QUALITY)
      g.setRenderingHint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON)

      g.drawImage(image, 0, 0, w, h, null)
      g.dispose()
      g.setComposite(AlphaComposite.Src)

      return resizedImage
    }
  }
}
