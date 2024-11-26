package silverbrain.server

import silverbrain.core.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

trait HttpEndpoints:
  given JsonValueCodec[IdOnly] = JsonCodecMaker.make
  given JsonValueCodec[ConflictError] = JsonCodecMaker.make
  given JsonValueCodec[InvalidArgumentError] = JsonCodecMaker.make
  given JsonValueCodec[SerializableException] = JsonCodecMaker.make

  given JsonValueCodec[Item] = JsonCodecMaker.make
  given JsonValueCodec[CreateItemArgs] = JsonCodecMaker.make
  given JsonValueCodec[UpdateItemArgs] = JsonCodecMaker.make

  private val endpointBase =
    endpoint
      .in(header[String]("X-SB-Store").default("main"))
      .errorOut(statusCode.and(plainBody[String]))
    // .errorOut(
    //   oneOf[Throwable](
    //     oneOfVariant(statusCode(StatusCode.NotFound).mapTo[IdNotFoundError]),
    //     oneOfVariant(
    //       statusCode(StatusCode.Conflict)
    //         .and(jsonBody[ConflictError])
    //         .mapTo[ConflictError]
    //     ),
    //     oneOfVariant(
    //       statusCode(StatusCode.BadRequest)
    //         .and(jsonBody[InvalidArgumentError])
    //         .mapTo[InvalidArgumentError]
    //     ),
    //     oneOfVariant(
    //       statusCode(StatusCode.InternalServerError)
    //         .and(jsonBody[SerializableException])
    //         .mapTo[SerializableException]
    //     )
    //   )
    // )

  val getItemEndpoint =
    this.endpointBase.get
      .in("items")
      .in(path[String]("id"))
      .in(query[String]("select"))
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
    this.endpointBase.delete.in("items").in(path[String]("id")).out(statusCode)
