private class Matrix(val elements: Array[Array[Int]]) {

    val dim: (Int, Int) = (elements.length, elements.head.length)

    /**
      * Matrix multiplication isn't transative. Important which matrix is getting multiplied by which
      *
      * @param matrix
      */
    infix def *(matrix: Matrix): Matrix = {
        assert(this.dim._1 == matrix.dim._2)

        val newMatrix = Matrix.zeroMatrix(this.dim._1, matrix.dim._2)

        for (i <- Range(0, dim._1)) {
            for (j <- Range(0, dim._2)) {
                newMatrix.set((i, j), elements(i)(j) * matrix.elements(j)(i))
            }
        }
        newMatrix
    }

    infix def *(scalar: Int) = {
        new Matrix(elements.clone().map(row => row.map(e => e*scalar)))
    }

    // infix def *() {

    // }

    def set(location: (Int, Int), thing: Int) = {
        assert(location._1 <= dim._1 && location._2 <= dim._2)
        elements(location._1)(location._2) = thing
    }

    override def toString(): String = {
        elements.map(row => row.mkString("(", " ", ")")).mkString("\n")
    }

}

object Matrix {
    def apply(mArr: Array[Array[Int]]) = {
        new Matrix(mArr)
    }

    def zeroMatrix(dim: (Int, Int)) = {
        new Matrix(Array.fill(dim._1)(Array.fill(dim._2)(0)))
    }

    // Add a conversion from Int to an object that can multiply by a matrix
    given Conversion[Int, MatrixScalarOps] with {
        def apply(scalar: Int): MatrixScalarOps = new MatrixScalarOps(scalar)
    }
    implicit class MatrixScalarOps(scalar: Int) extends AnyVal {
        infix def *(matrix: Matrix): Matrix = matrix * scalar
    }
}