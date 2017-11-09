package poker.utils

import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.file.{Path, StandardOpenOption}

import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

object FileUtils
{
  val BYTE_BUFFER_SIZE = 1024

  def readFile(filePath: Path): Future[List[String]] = try
  {
    val fileChannel = AsynchronousFileChannel.open(filePath, StandardOpenOption.READ)
    val byteBuffer = ByteBuffer.allocate(BYTE_BUFFER_SIZE)

    val promise = Promise[List[String]]

    fileChannel.read(byteBuffer, 0, byteBuffer, new CompletionHandler[Integer, ByteBuffer]
    {
      override def failed(throwable: Throwable, buffer: ByteBuffer): Unit = {
        promise.failure(throwable)
      }

      override def completed(result: Integer, buffer: ByteBuffer): Unit = {
        promise.success(new String(buffer.array()).trim.split("\n").toList)
      }
    })

    promise.future
  }
  catch {
    case NonFatal(throwable) => Future.failed(throwable)
  }
}
