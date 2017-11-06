package cards

import scala.util.{Failure, Success, Try}

trait Mapper[A]
{
  def key: String

  def value: A
}

object Mapper
{
  def mapping[A](list: List[Mapper[A]]): Map[String, A] =
    list.foldLeft(Map.empty[String, A]) {
      case (mappings, keyValue) => mappings + (keyValue.key -> keyValue.value)
    }

  def findValue[A](mappings: Map[String, A])(key: String, notFoundException: => Exception): Try[A] =
    mappings.get(key).fold[Try[A]](Failure(notFoundException))(Success(_))
}