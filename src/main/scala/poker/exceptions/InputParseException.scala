package poker.exceptions

case class InputParseException(string: String) extends Exception with ConsoleError
{
  override def getMessage = styleErrorMessage(s"Unable to parse: $string")
}
