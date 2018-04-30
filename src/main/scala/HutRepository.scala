
import java.util.UUID
import cats.effect.IO
import scala.collection.mutable.ListBuffer

import entities._

final class HutRepository(private val huts: ListBuffer[HutWithId]) {
  val makeId: IO[String] = IO { UUID.randomUUID().toString }

  def getHut(id: String): IO[Option[HutWithId]] =
    IO { huts.find(_.id == id) }

  def addHut(hut: Hut): IO[String] = 
    for {
      uuid <- makeId
      _ <- IO { huts += HutWithId(uuid, hut) }
    } yield uuid

  def updateHut(hutWithId: HutWithId): IO[Unit] =
    for {
      _ <- deleteHut(hutWithId.id)
      _ <- IO { huts += hutWithId }
    } yield ()

  def deleteHut(hutId: String): IO[Unit] =
    for {
      oh <- getHut(hutId)
      _ <- IO { oh.map(h => huts -= h) }
    } yield ()
}

object HutRepository {
  def empty: IO[HutRepository] =
    IO { new HutRepository(ListBuffer.empty) }
}

