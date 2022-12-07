@main def main(): Unit =
    val inp = scala.io.StdIn.readLine()
    [4,14].zipWithIndex
        .map((n, i) => s"Part ${i}: ${solve(inp)(i)}")
        .each(println)

def solve[A](seq: Seq[A]): Int => Option[Int] =
    (n: Int) => seq.sliding(n)
        .zipWithIndex
        .find(_._1.distinct.length == n)
        .map (_._2 + n)
