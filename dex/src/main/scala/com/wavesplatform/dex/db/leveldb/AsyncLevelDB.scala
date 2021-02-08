package com.wavesplatform.dex.db.leveldb

import com.google.common.primitives.Shorts
import org.iq80.leveldb.DB

import scala.concurrent.{ExecutionContext, Future}

trait AsyncLevelDB {
  def readOnly[A](f: ReadOnlyDB => A): Future[A]
  def readWrite[A](f: ReadWriteDB => A): Future[A]
  def get[A](key: Key[A]): Future[A]
  def has(key: Key[_]): Future[Boolean]
  def iterateOver(prefix: Short)(f: DBEntry => Unit): Future[Unit] = iterateOver(Shorts.toByteArray(prefix))(f)
  def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit): Future[Unit]
}

object AsyncLevelDB {

  def apply(db: DB)(implicit ec: ExecutionContext): AsyncLevelDB = new AsyncLevelDB {
    override def readOnly[A](f: ReadOnlyDB => A): Future[A] = Future(db.readOnly(f))
    override def readWrite[A](f: ReadWriteDB => A): Future[A] = Future(db.readWrite(f))
    override def get[A](key: Key[A]): Future[A] = Future(db.get(key))
    override def has(key: Key[_]): Future[Boolean] = Future(db.has(key))
    override def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit): Future[Unit] = Future(db.iterateOver(prefix)(f))
  }

}
