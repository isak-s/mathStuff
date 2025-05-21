package math

// numerator = num
// denominator = den
private class Quotient(val num: Int, val den: Int) {

    infix def *(o: Int) = new Quotient(num*o, den) // reduce ??
    infix def /(o: Int) = new Quotient(num, den*o) // reduce ??

    infix def *(o: Quotient) = new Quotient(num*o.num, den*o.den) // reduce ??
    infix def /(o: Quotient) = new Quotient(num*o.den, den*o.num) // reduce ??

    infix def +(o: Quotient) = {
        val gcd = Quotient.gcd(den, o.den)
        if (gcd == 1) {
            new Quotient(num*o.den + o.num*den, den*o.den)
        }
        else {
            new Quotient(num*o.den + o.num*den, den*o.den) // TODO: reduce
        }

    }

    override def toString(): String = {
        if (den == 1) {s"${num}"}
        else {s"${num}/${den}"}
    }
}

object Quotient {

    def apply(num: Int, den: Int) = {
        if (num == 0) {
            new Quotient(0, 1)
        }
        else {
            new Quotient(num, den)
        }
    }
    def fromRational() = {
        ???
    }

    def gcd(a: Int, b: Int): Int = {
        if (b == 0) { a.abs }
        else { gcd(b, a % b) }
    }

    def lcm(a: Int, b: Int) = {

    }

    private def simplifyFraction(q: Quotient) = {

    }

        // Add conversion from Int or double to quotient
    given QuotientConversion: Conversion[Int, Quotient] with {
        def apply(n: Int): Quotient = new Quotient(n, 1)
    }

    extension (inlineContext: StringContext)
        def q(args: Any*): Quotient = {
            val raw = inlineContext.s(args*)
            val Array(num, den) = raw.split("/").map(_.trim)
            Quotient(num.toInt, den.toInt)
        }
}