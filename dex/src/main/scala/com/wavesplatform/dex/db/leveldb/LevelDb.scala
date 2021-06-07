package com.wavesplatform.dex.db.leveldb

import cats.Id
import com.google.common.primitives.Shorts
import org.iq80.leveldb.DB

import scala.concurrent.{ExecutionContext, Future}

/**
 * Notes about usage:
 * 1. Do all work with DB inside ony readOnly/readWrite
 * 2. All post processing could be moved outside readOnly/readWrite
 *
 * Examples:
 *   Illegal: levelDb.get(someKey).flatMap(_ => levelDb.readWrite(...))
 *   Legal: levelDb.get(someKey).flatMap(_.sortFlipVertF())
 */
trait LevelDb[F[_]] {
  def readOnly[A](f: ReadOnlyDb => A): F[A]
  def readWrite[A](f: ReadWriteDb => A): F[A]
  def get[A](key: Key[A]): F[A]
  def has(key: Key[_]): F[Boolean]
  def iterateOver(prefix: Short)(f: DBEntry => Unit): F[Unit] = iterateOver(Shorts.toByteArray(prefix))(f)
  def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit): F[Unit]
}

object LevelDb {

  def async(db: DB)(implicit ec: ExecutionContext): LevelDb[Future] = new LevelDb[Future] {

    /**
     * Do not chain with map/flatMap/etc. See above
     */
    override def readOnly[A](f: ReadOnlyDb => A): Future[A] = Future(db.readOnly(f))

    /**
     * Do not chain with map/flatMap/etc. See above
     */
    override def readWrite[A](f: ReadWriteDb => A): Future[A] = Future(db.readWrite(f))
    override def get[A](key: Key[A]): Future[A] = Future(db.get(key))
    override def has(key: Key[_]): Future[Boolean] = Future(db.has(key))
    override def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit): Future[Unit] = Future(db.iterateOver(prefix)(f))
  }

  def sync(db: DB): LevelDb[Id] = new LevelDb[Id] {
    override def readOnly[A](f: ReadOnlyDb => A): A = db.readOnly(f)
    override def readWrite[A](f: ReadWriteDb => A): A = db.readWrite(f)
    override def get[A](key: Key[A]): A = db.get(key)
    override def has(key: Key[_]): Boolean = db.has(key)
    override def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit): Unit = db.iterateOver(prefix)(f)
  }

}
