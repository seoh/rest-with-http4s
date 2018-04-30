
import cats.effect.IO
import fs2.{ Stream, StreamApp }
import org.http4s.HttpService
import org.http4s.dsl.Http4sDsl
import org.http4s.server.blaze.BlazeBuilder

import scala.concurrent.ExecutionContext.Implicits.global

import entities._

object HutServer extends StreamApp[IO] with Http4sDsl[IO] {

  def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, StreamApp.ExitCode] =
    BlazeBuilder[IO]
    .bindHttp(8080, "0.0.0.0")
    .mountService(service, "/")
    .serve

  import io.circe.{ Decoder, Encoder }
  import io.circe.generic.auto._
  import org.http4s.circe.{jsonOf, jsonEncoderOf}

  implicit val hutEntityDecoder = jsonOf[IO, Hut]
  implicit val hutEntityEncoder = jsonEncoderOf[IO, Hut]


  implicit val hutIdDecoder = Decoder.forProduct2("id", "name")(
    (id: String, name: String) => HutWithId(id, Hut(name))
  )
  implicit val hutIdEntityDecoder = jsonOf[IO, HutWithId]
  implicit val hutIdEncoder = Encoder.forProduct2("id", "name")((h: HutWithId) =>
    (h.id, h.hut.name)
  )
  implicit val hutIdEntityEncoder = jsonEncoderOf[IO, HutWithId]

  val hutRepo = HutRepository.empty.unsafeRunSync()
  val prefix = "huts"

  val service = HttpService[IO] {

    // create
    case req @  POST -> Root / prefix =>
      for {
        newHut <- req.as[Hut]
        result <- hutRepo.addHut(newHut)
        response <- Created(result)
      } yield response

    // read
    case GET -> Root / prefix / hutId =>
      hutRepo.getHut(hutId) flatMap {
        case Some(hut) => Ok(hut)
        case None => NotFound()
      }

    // update
    case req @ PUT -> Root / prefix =>
      for {
        hut <- req.as[HutWithId]
        _ <- hutRepo.updateHut(hut)
        res <- Ok()
      } yield res

    // delete
    case DELETE -> Root / prefix / hutId =>
      for{
        _ <- hutRepo.deleteHut(hutId)
        res <- NoContent()
      } yield res
  }
}

