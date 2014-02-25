abstract class Log {
  def log(msg: String)
}

class YesLog extends Log {
  override def log(msg: String) { println(msg) }
}
class NoLog extends Log {
  override def log(msg: String) {}
}

object Log {
  def apply(verbose: Boolean) =
    if (verbose) new YesLog else new NoLog
}