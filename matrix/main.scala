package math

import Quotient.QuotientConversion
import Quotient.q

@main
def main = {

    val m = Matrix(Array(q"2/7", 2, 1), Array(1, 0, 1), Array(q"1/1", 5, 3))

    val m2 = Matrix.zeroMatrix(3, 3)
    for (i <- Range(0, 3)) {
        for (j <- Range(0, 3)) {
            m2.set((i, j), q"${3*i + j + 1}/7")
        }
    }
    val m3 = Matrix(Array(1,2,3), Array(3, 2, 1), Array(2, 1, 3))

    // println(s"${m}\n*\n${m2}\n=\n${m*m2}\n\n" )
    // println(s"3\n*\n${m2}\n=\n${3 * m2}\n\n")
    // println(s"7\n*\n${m2}\n=\n${7 * m2}\n\n")
    // println(s"det\n${m}\n=\n${m.determinant}\n\n")
    // println(s"tanspose of \n${m}\n=\n${m.transposed}\n\n")
    println(s"adjugate of \n${m}\n=\n${m.adjungate}\n\n")
    println(s"inverse of \n${m}\n=\n${m.inverse}\n\n")
    println(s"inverse of \n${m3}\n=\n${m3.inverse}\n\n")
}