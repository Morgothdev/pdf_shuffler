package pl.morgoth.scala.utils

import Helpers._
import java.io.Closeable

object StreamHelpers {
  def withOpenStream[T <: Closeable, R](stream: T)(fun: T => R) =
    try {
      fun(stream)
    } finally {
      stream.close()
    }
}