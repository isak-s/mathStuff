package math

import scala.language.implicitConversions
import Quotient.QuotientConversion
import Quotient.q
import math.Matrix.zeroMatrix
import scala.compiletime.ops.int

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

        for (i <- 0 until this.dim._1) {
            for (j <- 0 until matrix.dim._2) {
                var sum = Quotient(0, 1)
                for (k <- 0 until this.dim._2) {
                    sum += this.elements(i)(k) * matrix.elements(k)(j)
                }
                newMatrix.set((i, j), sum)
            }
        }
        newMatrix
    }

    infix def *(scalar: Int) = {
        new Matrix(elements.clone().map(row => row.map(e => e*scalar)))
    }

    infix def *(scalar: Quotient) = {
        new Matrix(elements.clone().map(row => row.map(e => e*scalar)))
    }

    def set(location: (Int, Int), thing: Quotient) = {
        require(location._1 <= dim._1 && location._2 <= dim._2)
        elements(location._1)(location._2) = thing
    }

    def get(location: (Int, Int)) = {
        require(location._1 <= dim._1 && location._2 <= dim._2)
        elements(location._1)(location._2)
    }

    def determinant = Matrix.determinant(deepcopy)

    def inverse = {
        val det = determinant
        require(!det.equals(0), "No inverse")
        adjungate * q"1/${det}"
    }

    def adjungate = {
        val m = zeroMatrix(dim)
        for (i <- 0 until dim._1) {
            for (j <- 0 until dim._2) {
                m.set((i, j), getCofactor((i, j)))
            }
        }
        m.transposed
    }

    def transposed = {
        val newM: Matrix = zeroMatrix(dim)
        for (i <- 0 until dim._1) {
            for (j <- 0 until dim._2) {
                newM.set((j, i), get(i, j))
            }
        }
        newM
    }
    /**
      * |---
      * |
      * |
      * @param pos
      */
    def getCofactor(pos: (Int, Int)) = {
        val m = zeroMatrix((dim._1-1, dim._2-1))
        for (i <- 0 until dim._1) {
            for (j <- 0 until dim._2) {
                if (i != pos._1 && j != pos._2) {
                    val ii = if (i < pos._1) i else i-1
                    val jj = if (j < pos._2) j else j-1
                    m.set((ii, jj), get(i, j))
                }
            }
        }
        val sign = if ((pos._1 + pos._2) % 2 == 0) 1 else -1
        Matrix.determinant(m) * sign
    }

    private def qrDecompose: (Matrix, Matrix) = {
        val n = dim._1
        require(dim._1 == dim._2, "QR decomposition requires square matrix")

        val Q = Matrix.zeroMatrix((n, n))
        val R = Matrix.zeroMatrix((n, n))

        val Acols = Array.tabulate(n)(j => (0 until n).map(i => get(i, j)).toArray)

        val Qcols = Array.ofDim[Array[Quotient]](n)

        for (j <- 0 until n) {
            var v = Acols(j).clone()

            for (i <- 0 until j) {
                val qi = Qcols(i)
                val rij = (qi zip Acols(j)).map { case (a, b) => a * b }.reduce(_ + _)
                R.set((i, j), rij)
                for (k <- 0 until n) v(k) = v(k) - qi(k) * rij
            }

            val normSq = v.map(x => x * x).reduce(_ + _)
            val norm = normSq.sqrt
            require(norm != 0.0, s"Zero norm on column $j — matrix may be singular or linearly dependent")

            val qj = v.map(x => x / Quotient.fromDouble(norm)) // convert to Quotient
            Qcols(j) = qj
            for (i <- 0 until n) {
                Q.set((i, j), qj(i))
            }
            R.set((j, j), Quotient.fromDouble(norm))
        }

        (Q, R)
    }



    private def qrEigenvalues(iterations: Int): Array[Quotient] = {
        require(dim._1 == dim._2, "Eigenvalue computation requires square matrix")
        val tolerance = 1e-6
        var A = deepcopy
        var converged = false

        var i = 0
        while (i < iterations && !hasConverged(A, tolerance)) {
            val (q, r) = A.qrDecompose
            A = r * q
            i += 1
        }

        (0 until dim._1).map(i => A.get((i, i))).toArray
    }

    private def hasConverged(A: Matrix, tolerance: Double): Boolean = {
        !(for (i <- 0 until A.dim._1; j <- 0 until A.dim._2 if i != j)
            yield A.get(i, j).toDouble.abs > tolerance).exists(identity)
    }

    def eigenvalues = qrEigenvalues(50)

        override def toString(): String = {
        val maxLength = elements.flatten.map(_.toString.length).max
        elements.map(row => row
            .map(n => n.toString + " " * (maxLength - n.toString.length))
            .mkString("(", " ", ")")).mkString("\n")
    }

    def deepcopy = new Matrix(elements.map(_.map(_.clone())))
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
        new Matrix(Array.fill(dim._1)(Array.fill(dim._2)(Quotient(0, 1))))
    }

    /**
      * Consumes the matrix
      *
      * @param m
      * @return
      */
    private def determinant(m: Matrix) = {
        require(m.dim._1 == m.dim._2)
        // m.deepcopy // REMOVE

        for (k <- Range(0, m.dim._1-1)) {
            for (i <- Range(k+1, m.dim._1)) {
                for (j <- Range(k+1, m.dim._1)) {
                    val res = (m.get(i, j) * m.get(k, k) - m.get(i, k) * m.get(k, j))
                    if (k == 0) {
                        m.set((i, j), res)
                    }
                    else {
                        m.set((i, j), res / m.get(k-1, k-1))
                    }
                }
            }
        }
        m.get(m.dim._1 -1, m.dim._2 - 1)
    }

    // Add a conversion from Int to an object that can multiply by a matrix
    given Conversion[Int, MatrixScalarOps] with {
        def apply(scalar: Int): MatrixScalarOps = new MatrixScalarOps(scalar)
    }
    implicit class MatrixScalarOps(scalar: Int) extends AnyVal {
        infix def *(matrix: Matrix): Matrix = matrix * scalar
    }
}