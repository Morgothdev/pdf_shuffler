package pl.morgoth.scala.utils

object Helpers {
  class ReturnVal[R](val value: R) {
    def butBefore(before: Unit => Unit): R = {
      before()
      value
    }
  }
  def riturn[R](value: R) = new ReturnVal(value)
}
