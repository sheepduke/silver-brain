package silverbrain.http.contract

import silverbrain.core.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import sttp.tapir.*
import sttp.tapir.json.jsoniter.jsonBody
import sttp.tapir.generic.auto.*

object HttpEndpoints:
  given JsonValueCodec[IdOnly] = JsonCodecMaker.make
  given JsonValueCodec[ConflictError] = JsonCodecMaker.make
  given JsonValueCodec[InvalidArgumentError] = JsonCodecMaker.make
  given JsonValueCodec[SerializableException] = JsonCodecMaker.make

  given JsonValueCodec[Item] = JsonCodecMaker.make
  given JsonValueCodec[Seq[Item]] = JsonCodecMaker.make
  given JsonValueCodec[CreateItemArgs] = JsonCodecMaker.make
  given JsonValueCodec[UpdateItemArgs] = JsonCodecMaker.make

  private val endpointBase =
    endpoint
      .in("api" / "v2")
      .in(header[String]("X-SB-Store").default("main"))
      .errorOut(statusCode.and(stringBody))

  val getItem =
    this.endpointBase.get
      .in("items")
      .in(path[String]("ids"))
      .in(query[String]("select").default("all"))
      .out(jsonBody[Item])

  val getItems =
    this.endpointBase.get
      .in("items")
      .in(query[Option[String]]("ids").default(None))
      .in(query[Option[String]]("search").default(None))
      .in(query[String]("select").default(""))
      .out(jsonBody[Seq[Item]])

  val createItem =
    this.endpointBase.post
      .in("items")
      .in(jsonBody[CreateItemArgs])
      .out(statusCode.and(jsonBody[IdOnly]))

  val updateItem =
    this.endpointBase.patch
      .in(jsonBody[UpdateItemArgs])
      .out(statusCode)

  val deleteItem =
    this.endpointBase.delete.in("items").in(path[String]("id")).out(statusCode)
