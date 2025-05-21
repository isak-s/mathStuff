package math

import scala.language.implicitConversions
import Quotient.QuotientConversion

private class Matrix(val elements: Array[Array[Quotient]]) {

    val dim: (Int, Int) = (elements.length, elements.head.length)

    /**
      * Matrix multiplication isn't transative. Important which matrix is getting multiplied by which
      *
      * @param matrix
      */
    infix def *(matrix: Matrix): Matrix = {
        require(this.dim._1 == matrix.dim._2)

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

    def set(location: (Int, Int), thing: Quotient) = {
        require(location._1 <= dim._1 && location._2 <= dim._2)
        elements(location._1)(location._2) = thing
    }

    lazy val determinant = {

    }

    lazy val adjunct = {

    }

    override def toString(): String = {
        val maxLength = elements.flatten.map(_.toString.length).max
        elements.map(row => row
            .map(n => n.toString + " " * (maxLength - n.toString.length))
            .mkString("(", " ", ")")).mkString("\n")
    }

}

object Matrix {

    def apply(args: Array[Int | Quotient]*) = {
        val data: Array[Array[Quotient]] = args.map(_.map {
            case i: Int => Quotient(i, 1)     // Convert Int to Quotient
            case q: Quotient => q            // Keep Quotient as is
            }).toArray
        new Matrix(data)
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