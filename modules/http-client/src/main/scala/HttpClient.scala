package silverbrain.client.http

import silverbrain.core.*

import cats.effect.*
import com.github.plokhotnyuk.jsoniter_scala.core as json
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.http4s.Headers
import org.http4s.MediaType
import org.http4s.Method
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.Accept
import scala.collection.mutable
import org.http4s.Status

class HttpClient(
    scheme: String = "http",
    host: String = "localhost",
    port: Int = 8080
):
  given JsonValueCodec[IdOnly] = JsonCodecMaker.make
  given JsonValueCodec[Item] = JsonCodecMaker.make
  given JsonValueCodec[CreateItemArgs] = JsonCodecMaker.make
  given JsonValueCodec[IdNotFoundError] = JsonCodecMaker.make
  given JsonValueCodec[ConflictError] = JsonCodecMaker.make
  given JsonValueCodec[InvalidArgumentError] = JsonCodecMaker.make

  private val baseUrl =
    Uri.unsafeFromString(s"${scheme}://${host}:${port}/api/v2/")

  private val clientResource: Resource[IO, Client[IO]] =
    EmberClientBuilder.default[IO].build

  def getItem(
      itemId: String,
      loadOptions: ItemLoadOptions = ItemLoadOptions()
  ): AppIOResult[Item] =
    val select = this.itemLoadOptionsToSelect(loadOptions)

    this
      .get(Uri.unsafeFromString(s"items/$itemId?select=$select"))
      .mapValue(json.readFromString[Item](_))

  def createItem(item: CreateItemArgs): AppIOResult[String] =
    this.post(Uri.unsafeFromString("items"), item)

  def deleteItem(itemId: String): AppIOResult[Unit] =
    this.delete(Uri.unsafeFromString(s"items/$itemId"))

  private def itemLoadOptionsToSelect(loadOptions: ItemLoadOptions): String =
    val selectKeys = mutable.ArrayBuffer[String]()

    if loadOptions.contentType then selectKeys += "contentType"
    if loadOptions.content then selectKeys += "content"
    if loadOptions.parents then selectKeys += "parents"
    if loadOptions.children then selectKeys += "children"
    if loadOptions.properties then selectKeys += "properties"
    if loadOptions.createTime then selectKeys += "createTime"
    if loadOptions.updateTime then selectKeys += "updateTime"

    selectKeys.mkString(",")

  private def get(url: Uri): AppIOResult[String] =
    val request = Request[IO](
      method = Method.GET,
      uri = this.baseUrl.resolve(url),
      headers = Headers(Accept(MediaType.application.json))
    )

    this.send(request)

  private def post[A](url: Uri, content: A)(using
      JsonValueCodec[A]
  ): AppIOResult[String] =
    val requestBody = json.writeToString(content).getBytes()

    val request = Request[IO](
      method = Method.POST,
      uri = this.baseUrl.resolve(url),
      headers = Headers(Accept(MediaType.application.json)),
      body = fs2.Stream.emits(requestBody)
    )

    this.send(request).mapValue(json.readFromString[IdOnly](_).id)

  private def udpate[A](url: Uri, content: A)(using
      JsonValueCodec[A]
  ): AppIOResult[Unit] =
    val requestBody = json.writeToString(content).getBytes()

    val request = Request[IO](
      method = Method.PATCH,
      uri = this.baseUrl.resolve(url),
      headers = Headers(Accept(MediaType.application.json)),
      body = fs2.Stream.emits(requestBody)
    )

    this.send(request).mapValue(_ => Right(()))

  private def delete(url: Uri): AppIOResult[Unit] =
    val request = Request[IO](
      method = Method.DELETE,
      uri = this.baseUrl.resolve(url)
    )

    this.send(request).mapValue(_ => Right(()))

  private def send(request: Request[IO]): AppIOResult[String] =
    this.clientResource.use(client =>
      client
        .run(request)
        .use(response =>
          for body <- response.as[String]
          yield response.status match
            case Status.Ok | Status.Created | Status.NoContent => Right(body)
            case Status.NotFound =>
              Left(json.readFromString[IdNotFoundError](body))
            case Status.Conflict =>
              Left(json.readFromString[ConflictError](body))
            case Status.BadRequest =>
              Left(json.readFromString[InvalidArgumentError](body))
            case Status.InternalServerError =>
              Left(AppInternalError(RuntimeException(body)))
        )
    )

// project /
// project httpClient
object Main extends IOApp:
  def run(args: List[String]): IO[ExitCode] =
    val client = HttpClient()

    for
      item <- client.getItem(
        "i_2gMnxuTPOdmZINO3e2ExYfzHbxL",
        ItemLoadOptions().withAll
      )
      _ <- IO.println(item)
    yield ExitCode.Success
