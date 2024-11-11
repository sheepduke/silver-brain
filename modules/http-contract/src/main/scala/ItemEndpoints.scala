package silverbrain.http.contract

import silverbrain.core.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.jsoniter.*
import sttp.tapir.EndpointIO.annotations.statusCode

case class IdOnly(id: String)

trait ItemEndpoints extends StoreBasedEndpoint:
  given JsonValueCodec[IdOnly] = JsonCodecMaker.make
  given JsonValueCodec[Item] = JsonCodecMaker.make
  given JsonValueCodec[CreateItemArgs] = JsonCodecMaker.make
  given JsonValueCodec[UpdateItemArgs] = JsonCodecMaker.make

  private val endpointBase =
    this.storeBasedEndpoint.errorOut(statusCode.and(plainBody[String]))

  // TODO refine this part using oneOf variant.
  val getItemEndpoint =
    this.endpointBase.get
      .in("items")
      .in(path[String]("id"))
      .out(jsonBody[Item])

  val createItemEndpoint =
    this.endpointBase.post
      .in("items")
      .in(jsonBody[CreateItemArgs])
      .out(statusCode.and(jsonBody[IdOnly]))

  val updateItemEndpoint =
    this.endpointBase.patch
      .in(jsonBody[UpdateItemArgs])
      .out(statusCode)

  val deleteItemEndpoint =
    this.endpointBase.delete.in(path[String]("id")).out(statusCode)
