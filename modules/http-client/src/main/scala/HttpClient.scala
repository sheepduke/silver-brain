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

class HttpClient(
    scheme: String = "http",
    host: String = "localhost",
    port: Int = 8080
):
  given JsonValueCodec[IdOnly] = JsonCodecMaker.make
  given JsonValueCodec[Item] = JsonCodecMaker.make
  given JsonValueCodec[CreateItemArgs] = JsonCodecMaker.make

  private val baseUrl =
    Uri.unsafeFromString(s"${scheme}://${host}:${port}/api/v2/")

  private val clientResource: Resource[IO, Client[IO]] =
    EmberClientBuilder.default[IO].build

  def getItem(itemId: String): IO[Item] =
    for itemJson <- this.get(Uri.unsafeFromString(s"items/$itemId"))
    yield json.readFromString[Item](itemJson)

  def createItem(item: CreateItemArgs): IO[String] =
    this.post(Uri.unsafeFromString("items"), item)

  def deleteItem(itemId: String): IO[Unit] =
    this.delete(Uri.unsafeFromString(s"items/$itemId"))

  private def get(url: Uri): IO[String] =
    val request = Request[IO](
      method = Method.GET,
      uri = this.baseUrl.resolve(url),
      headers = Headers(Accept(MediaType.application.json))
    )

    this.send(request)

  private def post[A](url: Uri, content: A)(using
      JsonValueCodec[A]
  ): IO[String] =
    val requestBody = json.writeToString(content).getBytes()

    val request = Request[IO](
      method = Method.POST,
      uri = this.baseUrl.resolve(url),
      headers = Headers(Accept(MediaType.application.json)),
      body = fs2.Stream.emits(requestBody)
    )

    this.send(request).map(json.readFromString[IdOnly](_).id)

  private def udpate[A](url: Uri, content: A)(using
      JsonValueCodec[A]
  ): IO[Unit] =
    val requestBody = json.writeToString(content).getBytes()

    val request = Request[IO](
      method = Method.PATCH,
      uri = this.baseUrl.resolve(url),
      headers = Headers(Accept(MediaType.application.json)),
      body = fs2.Stream.emits(requestBody)
    )

    this.send(request).map(_ => ())

  private def delete(url: Uri): IO[Unit] =
    val request = Request[IO](
      method = Method.DELETE,
      uri = this.baseUrl.resolve(url)
    )

    this.send(request).map(_ => ())

  private def send(request: Request[IO]): IO[String] =
    this.clientResource.use(client => client.expect[String](request))
