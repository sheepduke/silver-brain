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
  given JsonValueCodec[CreateItemReferenceArgs] = JsonCodecMaker.make
  given JsonValueCodec[UpdateItemReferenceArgs] = JsonCodecMaker.make

  private val endpointBase =
    endpoint
      .in("api" / "v2")
      .in(header[String]("X-SB-Store").default("main"))
      .errorOut(statusCode.and(stringBody))

  // ============================================================
  //  Item
  // ============================================================

  val createItem =
    endpointBase.post
      .in("items")
      .in(jsonBody[CreateItemArgs])
      .out(statusCode.and(jsonBody[IdOnly]))

  val getItem =
    endpointBase.get
      .in("items")
      .in(path[String]("id"))
      .in(query[String]("select").default("all"))
      .out(jsonBody[Item])

  val getItems =
    endpointBase.get
      .in("items")
      .in(query[Option[String]]("ids").default(None))
      .in(query[Option[String]]("search").default(None))
      .in(query[String]("select").default(""))
      .out(jsonBody[Seq[Item]])

  val updateItem =
    endpointBase.patch
      .in("items")
      .in(path[String]("id"))
      .in(jsonBody[UpdateItemArgs])
      .out(statusCode)

  val deleteItem =
    endpointBase.delete.in("items").in(path[String]("id")).out(statusCode)

  // ============================================================
  //  Link
  // ============================================================

  val createParent =
    endpointBase.post
      .in("items")
      .in(path[String]("id"))
      .in("parents")
      .in(path[String]("parent"))
      .out(statusCode)

  val createChild =
    endpointBase.post
      .in("items")
      .in(path[String]("id"))
      .in("children")
      .in(path[String]("child"))
      .out(statusCode)

  val deleteParent =
    endpointBase.delete
      .in("items")
      .in(path[String]("id"))
      .in("parents")
      .in(path[String]("parent"))
      .out(statusCode)

  val deleteChild =
    endpointBase.delete
      .in("items")
      .in(path[String]("id"))
      .in("children")
      .in(path[String]("child"))
      .out(statusCode)

  // ============================================================
  //  Reference
  // ============================================================

  val createReference =
    endpointBase.post
      .in("references")
      .in(jsonBody[CreateItemReferenceArgs])
      .out(statusCode.and(jsonBody[IdOnly]))

  val updateReference =
    endpointBase.patch
      .in("references")
      .in(path[String]("id"))
      .in(jsonBody[UpdateItemReferenceArgs])
      .out(statusCode)

  val deleteReference =
    endpointBase.delete.in("references").in(path[String]("id")).out(statusCode)
