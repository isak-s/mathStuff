package math

import Quotient.QuotientConversion
import Quotient.q

@main
def main = {

    val m = Matrix(Array(q"2/7", 1, 1), Array(1, 4, 1), Array(q"1/1", 1, 3))

    val m2 = Matrix.zeroMatrix(3, 3)
    for (i <- Range(0, 3)) {
        for (j <- Range(0, 3)) {
            m2.set((i, j), q"${3*i + j + 1}/7")
        }
    }
    println(s"${m}\n*\n${m2}\n=\n${m*m2} \n\n" )
    println(s"3\n*\n${m2}\n=\n${3 * m2}")
}