package poker.exceptions

case class CardSuiteMappingException(string: String) extends Exception
{
  override def getMessage = s"""Unable to parse suite: \"$string\""""
}