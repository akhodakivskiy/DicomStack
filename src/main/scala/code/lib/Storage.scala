package code.lib

import code.model._

import java.io.InputStream

import scala.collection.convert.Wrappers._
import scala.concurrent._

import org.joda.time.DateTime

import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import net.liftweb.util.StringHelpers._

import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.transfer.TransferManager
import com.amazonaws.services.s3.model._
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.{AmazonClientException, AmazonServiceException}

trait Storage {
  def url(key: String): String

  def upload(key: String, is: InputStream, size: Long): Unit

  def delete(key: String): Unit

  def read[T](key: String)(f: InputStream => T): T

  def download(key: String): java.io.File
}

object S3Storage extends Storage with Logger {
  private val awsAccessKey = Props.get("aws.access.key", "")
  private val awsSecretKey = Props.get("aws.secret.key", "")
  private val awsS3Bucket = Props.get("storage.s3.bucket", "")

  private val awsCredentials = new BasicAWSCredentials(awsAccessKey, awsSecretKey)
  private val awsS3Client = new AmazonS3Client(awsCredentials)

  def url(key: String): String = {
    awsS3Client.generatePresignedUrl(awsS3Bucket, key, DateTime.now.plusDays(1).toDate).toString
  }

  def upload(key: String, is: InputStream, size: Long) = {
    trace(s"Uploading an InputStream to S3, Size: $size, Key: $key")
    val meta = new ObjectMetadata()
    meta.setContentLength(size)

    val request = new PutObjectRequest(awsS3Bucket, key, is, meta)
    awsS3Client.putObject(request)
  }

  def read[T](key: String)(f: InputStream => T) = {
    trace(s"Reading an object from S3, Key: $key")
    f(awsS3Client.getObject(awsS3Bucket, key).getObjectContent)
  }

  def delete(key: String) = {
    trace(s"Deleting an object from S3, Key: $key")
    awsS3Client.deleteObject(awsS3Bucket, key)
  }

  def download(key: String) = {
    trace(s"Downloading an object from S3, Key: $key")
    val javaFile = java.io.File.createTempFile(awsS3Bucket, nextFuncName)
    val request = new GetObjectRequest(awsS3Bucket, key)
    awsS3Client.getObject(request, javaFile)
    javaFile
  }
}

object LocalStorage extends Storage with Logger {
  import java.nio.file.{Files, Paths, StandardCopyOption}
  import java.nio.file.StandardCopyOption._

  private val rootPath = Paths.get {
    Props.get("storage.local.folder", "")
  }

  Files.createDirectories(rootPath)

  def url(key: String) = {
    rootPath.resolve(key).toString
  }

  def upload(key: String, is: InputStream, size: Long) = {
    trace(s"Reading an object to local storage, Size: $size, Key: $key")
    val path = rootPath.resolve(key)
    Files.createDirectories(path.getParent)
    Files.copy(is, path, REPLACE_EXISTING)
  }

  def read[T](key: String)(f: InputStream => T) = {
    trace(s"Reading an object from local storage, Key: $key")
    f(Files.newInputStream(rootPath.resolve(key)))
  }

  def delete(key: String) = {
    trace(s"Deleting an object from local storage, Key: $key")
    if (Files.exists(rootPath.resolve(key))) {
      Files.delete(rootPath.resolve(key))
    }
  }

  def download(key: String) = {
    trace(s"Downloading an object from local storage, Key: $key")
    val tmpPath = rootPath.resolve("tmp").resolve(nextFuncName)
    if (Files.deleteIfExists(tmpPath)) {
      warn(s"temp file name collision: $tmpPath. Old file has been deleted")
    }
    Files.createDirectories(tmpPath.getParent)
    Files.copy(rootPath.resolve(key), tmpPath, REPLACE_EXISTING).toFile
  }
}

object Storage extends Logger {
  lazy val inst = Props.get("storage") match {
    case Full("local") => LocalStorage
    case Full("s3") => S3Storage
    case _ => sys.error("Could not find storage backend")
  }
}
