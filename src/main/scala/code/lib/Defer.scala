package code.lib

import code.model._

import net.liftweb.common._
import net.liftweb.http._

import java.util.concurrent.Executors
import java.nio.file.Paths

import scala.concurrent._

import java.awt.image.BufferedImage
import java.util.zip._
import java.util.concurrent.Executors
import java.io.{ByteArrayOutputStream, ByteArrayInputStream, FileInputStream, InputStream}
import org.dcm4che3.io.DicomStreamException

import org.joda.time.DateTime

object DeferQuery {
  import scala.slick.driver.PostgresDriver.simple._
  import Database.dynamicSession
  import Slick._

  val byParentIdAndName = for {
    (parentId, name) <- Parameters[(Option[FileId], String)]
    f <- Files if f.parentId === parentId && f.name === name
  } yield f

  def findByParentIdAndName(parentId: Option[FileId], name: String) = {
    byParentIdAndName(parentId, name).firstOption
  }
}

object Defer extends Logger {
  import ImportKind._
  val currentImportVersion = 1

  implicit val executionContext = ExecutionContext.fromExecutor {
    val threadPoolSize = Runtime.getRuntime.availableProcessors
    Executors.newFixedThreadPool(threadPoolSize)
  }

  def mkKey(file: FileT, parent: Either[UserT, FileT]): String = {
    val suffix = s"${ file.id }/${ file.name }"
    parent match {
      case Left(user) => s"uploads/${ user.email }/$suffix"
      case Right(parent) => s"${ Paths.get(parent.key).getParent }-$suffix"
    }
  }

  def mkThumbKey(file: FileT, size: Int): String = {
    s"${ file.key }.${size}.png }"
  }

  def uploadFuture(is: InputStream, parent: Either[UserT, FileT])(newFile: FileT): Future[FileT] = {
    val file = Slick.db.withDynSession {
      Files.insert(newFile)
    }
    val key = mkKey(file, parent)

    Future {
      Storage.inst.upload(key, is, file.size)
      is.close
      file.copy(key = key)
    } andThen {
      case scala.util.Success(file) => Slick.db.withDynSession {
        Files.update(file.copy(key = key, isRemoved = false))
      }
      case scala.util.Failure(t) => Slick.db.withDynSession {
        Files.delete_!(file.id)
      }
    }
  }

  def uploadFuture(user: UserT, fph: FileParamHolder): Future[FileT] = {
    uploadFuture(fph.fileStream, Left(user)) {
      trace(s"Uploading file ${fph.fileName}")
      FileT(user.id, fph.fileName, fph.length, "", None, true)
    }
  }

  def uploadFuture(entry: ZipEntry, parentFile: FileT, is: InputStream): Future[FileT] = {
    Slick.db.withDynSession {
      DeferQuery.findByParentIdAndName(Some(parentFile.id), entry.getName)
    } map { file =>
      trace(s"Zip Entry has alrady been uploaded: ${file.id} - ${file.name}")
      Future.successful(file)
    } getOrElse {
      uploadFuture(is, Right(parentFile)) {
        trace(s"Uploading Zip Entry ${entry.getName}")
        FileT(parentFile.userId, entry.getName, entry.getSize,
              "", Some(parentFile.id), true)
      }
    }
  }

  def downloadFuture(file: FileT): Future[java.io.File] = Future {
    Storage.inst.download(file.key)
  }

  def unzipFuture(file: FileT, javaFile: java.io.File): Future[List[FileT]] = {
    Future {
      import scala.collection.convert.Wrappers._

      val zipFile = new java.util.zip.ZipFile(javaFile)
      val entries = new JEnumerationWrapper(zipFile.entries)
        .filter(!_.getName.startsWith("__MACOSX"))
        .filter(e => e.getSize > -1 && !e.isDirectory)
        .toList
      trace(s"Unzipping ${entries.length} zip entries")
      Tuple2(zipFile, entries)
    } flatMap { case (zipFile, entries) =>
      Future.traverse(entries) { entry =>
        uploadFuture(entry, file, zipFile.getInputStream(entry))
      }
    }
  }

  def uploadBlobFuture[T <: BlobT](mkBlob: () => T)(is: InputStream, size: Long): Future[T] = {
    val blob = mkBlob()

    Future {
      Storage.inst.upload(blob.key, is, size)
      blob
    }
  }

  def uploadDicomThumb(file: FileT, image: ImageT, frame: BufferedImage,
                       dimension: Int): Future[ThumbnailT] = {
    Future {
      val resizedFrame = Util.resizeImage(frame, dimension)

      val baos = new ByteArrayOutputStream()
      javax.imageio.ImageIO.write(resizedFrame, "png", baos)
      new ByteArrayInputStream(baos.toByteArray)
    } flatMap { is =>
      val mkBlob = () => {
        val key = s"${file.key}.${dimension}.png"
        Slick.db.withDynSession(
          Thumbnails.insert(ThumbnailT(dimension, is.available, key, image.id))
        )
      }
      uploadBlobFuture(mkBlob)(is, is.available)
    }
  }

  def deleteBlob[T <: BlobT] (blob: T): Future[_] = {
    Future {
      Storage.inst.delete(blob.key)
      Slick.db.withDynSession {
        blob match {
          case f: FileT => Files.remove(f.id)
          case t: ThumbnailT => Thumbnails.delete_!(t.id)
        }
      }
    }
  }

  def uploadDicomImages(file: FileT, image: ImageT, javaFile: java.io.File): Future[Boolean] = {
    info("uploading thumbnails")

    Future {
      Dicom.frameData(new FileInputStream(javaFile), 0)
    } flatMap {
      case Full(frame) => {
        val dimensions = Thumbnails.dimensions(frame.getWidth max frame.getHeight)

        Future {
          Slick.db.withDynSession {
            Thumbnails.findByImageId(image.id)
          }
        } flatMap { thumbs =>
          Future.traverse(thumbs) { thumb =>
            deleteBlob(thumb)
          }
        } flatMap { _ =>
          Future.traverse(dimensions) { dimension =>
            uploadDicomThumb(file, image, frame, dimension)
          }
        } map { _ => true }
      }
      case Failure(msg, Full(e), _) => {
        error(msg, e)
        Future.successful(false)
      }
      case _ => Future.successful(false)
    }
  }

  def importFutureWithCallback[T](file: FileT)(cb: ImportResultT => T): Future[T] = {
    importFuture(file).map(cb)
  }

  def importFuture(file: FileT): Future[ImportResultT] = {
    def innerImportFuture(report: Boolean)(file: FileT): Future[ImportResultT] = {
      downloadFuture(file).flatMap { javaFile =>
        unzipFuture(file, javaFile) flatMap { zipFiles =>
          Future.traverse(zipFiles)(innerImportFuture(false))
        } map { results =>
          ImportResults(file.id, UnzipKind)
        } recoverWith {
          case e: ZipException => {
            Future {
              Dicom.readOrSaveEntities(file, new FileInputStream(javaFile))
            } flatMap {
              case Full((entities, data)) => {
                uploadDicomImages(file, entities.image, javaFile).map { imageRead =>
                  ImportResults(file.id, entities, imageRead)
                }
              }
              case Failure(msg, eBox, _) => {
                error(s"failed to read file as DICOM: $msg", eBox)
                Future.successful {
                  ImportResults(file.id, eBox)
                }
              }
              case Empty => Future.successful {
                ImportResults(file.id)
              }
            }
          }
        } recover { case e: Exception =>
          error("failed to process file", e)
          ImportResults(file.id, Some(e))
        } andThen { case _ =>
          javaFile.delete
        }
      } map { importResult =>
        importResult.copy(version = currentImportVersion)
      } andThen {
        case scala.util.Success(result) => {
          Slick.db.withDynSession {
            ImportResults.insert(result)
          }
          trace(s"Successfully processed file")
        }
        case scala.util.Failure(t) => {
          error(s"Failed to process file ${file.id}: ${file.name}", t)
        }
      }
    }

    innerImportFuture(true)(file)
  }

  def tagsFuture(file: FileT): Future[Seq[TagValue]] = {
    downloadFuture(file).flatMap { javaFile =>
      Future {
        Dicom.readTags(new FileInputStream(javaFile))
      } andThen { case _ =>
        javaFile.delete
      }
    }
  }
}
