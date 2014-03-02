package pl.morgoth.scala.utils

class YesLog extends Log {
  override def log(msg: String) {
    println(msg)
  }
}

class NoLog extends Log {
  override def log(msg: String) {}
}


object Log {
  def apply(verbose: Boolean) =
    if (verbose) new YesLog else new NoLog
}

abstract class Log {
  def log(msg: String)
}