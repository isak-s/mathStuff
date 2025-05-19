import Quotient.QuotientConversion

@main
def main = {
    val m = Matrix(Array(Array(555, 1, 1), Array(1, 4, 1), Array(1, 1, 3)))

    val m2 = Matrix.zeroMatrix(3, 3)
    for (i <- Range(0, 3)) {
        for (j <- Range(0, 3)) {
            m2.set((i, j), 3*i + j + 1)
        }
    }
    val m3 = m2 * m
    val m4 = 3 * m2
    println("\n" + m2)
    println("\n" + m3)
    println("\n" + m4)
}