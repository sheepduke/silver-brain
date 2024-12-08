package silverbrain.http.contract

extension (string: String)
  def splitByComma: Seq[String] =
    string.split(",").map(_.trim()).filter(_.nonEmpty)
