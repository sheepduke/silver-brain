package silverbrain.shared.domain

import com.github.ksuid.Ksuid

object PrefixedKsuid:
  def generate(prefix: String): String =
    s"${prefix}_${Ksuid.newKsuid().toString}"

  def from(prefix: String, value: String): Either[String, String] =
    value match
      case s if s.startsWith(s"${prefix}_") =>
        try
          Ksuid.fromString(s.stripPrefix(s"${prefix}_"))
          Right(value)
        catch case _: Exception => Left(s"Invalid id: $value")
      case _ => Left(s"Invalid id: $value")
