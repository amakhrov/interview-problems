/*
Run
> scala ./Permutations.scala

Problem:
given a string 1234567890 and a `target` number,
insert `+` or `-` before some characters in a string
so that the resulting expressions evaluates to `target`
 */
object Permutations extends App {
  val len = 10
  val symbols = "1234567890".split("").map(_.toInt)
  val possibleOps = List("", "-", "+")

  def find(target: Int): String = {
    val appliedOps = Array.ofDim[String](len)

    def combine =
      symbols
        .zip(appliedOps)
        .take(len)
        .map(pair => s"${pair._2}${pair._1}")
        .mkString("")

    def inner(
        idx: Int,
        total: Int,
        currentNum: Int,
        currentSign: Int
    ): Boolean = {
      def newTotal = total + currentNum * currentSign

      def applyOperator(op: String) =
        op match {
          case "" =>
            inner(
              idx + 1,
              total,
              currentNum * 10 + symbols(idx),
              currentSign
            )
          case "+" =>
            inner(idx + 1, newTotal, symbols(idx), +1)
          case "-" =>
            inner(idx + 1, newTotal, symbols(idx), -1)
          case _ => throw new Exception(s"Unexpected op '$op'")
        }

      if (idx == len)
        newTotal == target
      else {
        possibleOps.exists(op => {
          appliedOps(idx) = op
          applyOperator(op)
        })
      }
    }

    inner(0, 0, 0, +1) match {
      case true  => combine
      case false => ""
    }
  }

  println(s"res = ${find(1 - 23 + 45)}")
}
