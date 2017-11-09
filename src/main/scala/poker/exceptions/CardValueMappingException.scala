package poker.exceptions

case class CardValueMappingException(string: String) extends Exception
{
  override def getMessage = s"""Unable to parse card value: \"$string\""""
}
