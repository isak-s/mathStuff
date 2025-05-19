@main
def main = {
    val m = Matrix(Array(Array(5, 1, 1), Array(1, 4, 1), Array(1, 1, 3)))
    println(m)

    val m2 = Matrix.zeroMatrix(3, 3)
    for (i <- Range(0, 3)) {
        for (j <- Range(0, 3)) {
            m2.set((i, j), 3*i + j + 1)
        }
    }
    val m3 = m2 * m
    val m4 = 3 * m2
    println(m2)
    println()
    println(m3)
    println("\n" + m4)
}