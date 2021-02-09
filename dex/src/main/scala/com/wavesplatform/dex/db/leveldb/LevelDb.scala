package com.wavesplatform.dex.db.leveldb

import cats.Id
import com.google.common.primitives.Shorts
import org.iq80.leveldb.DB

import scala.concurrent.{ExecutionContext, Future}

trait LevelDb[F[_]] {
  def readOnly[A](f: ReadOnlyDB => A): F[A]
  def readWrite[A](f: ReadWriteDB => A): F[A]
  def get[A](key: Key[A]): F[A]
  def has(key: Key[_]): F[Boolean]
  def iterateOver(prefix: Short)(f: DBEntry => Unit): F[Unit] = iterateOver(Shorts.toByteArray(prefix))(f)
  def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit): F[Unit]
}

object LevelDb {

  def async(db: DB)(implicit ec: ExecutionContext): LevelDb[Future] = new LevelDb[Future] {
    override def readOnly[A](f: ReadOnlyDB => A): Future[A] = Future(db.readOnly(f))
    override def readWrite[A](f: ReadWriteDB => A): Future[A] = Future(db.readWrite(f))
    override def get[A](key: Key[A]): Future[A] = Future(db.get(key))
    override def has(key: Key[_]): Future[Boolean] = Future(db.has(key))
    override def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit): Future[Unit] = Future(db.iterateOver(prefix)(f))
  }

  def sync(db: DB)(implicit ec: ExecutionContext): LevelDb[Id] = new LevelDb[Id] {
    override def readOnly[A](f: ReadOnlyDB => A): A = db.readOnly(f)
    override def readWrite[A](f: ReadWriteDB => A): A = db.readWrite(f)
    override def get[A](key: Key[A]): A = db.get(key)
    override def has(key: Key[_]): Boolean = db.has(key)
    override def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit): Unit = db.iterateOver(prefix)(f)
  }

}
