object day06 {
    def main(args: Array[String]) = {
        def solve (str: String, n: Int): Option[Int] =
            str.sliding(n)
               .zipWithIndex
               .find(_._1.distinct.length == n)
               .map (_._2 + n)
        val inp = scala.io.StdIn.readLine()
        println(s"Part 1: ${solve(inp, 4)}")
        println(s"Part 2: ${solve(inp, 14)}")
    }
}