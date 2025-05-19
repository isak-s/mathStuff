// numerator = num
// denominator = den
class Quotient(val num: Int, val den: Int) {

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

    override def toString(): String = {s"${num}/${den}"}
}

object Quotient {
    def fromRational() = {
        ???
    }

    def gcd(a: Int, b: Int): Int = {
        if (b == 0) { a.abs }
        else { gcd(b, a % b) }
    }

    def lcm(a: Int, b: Int) = {

    }
        // Add conversion from Int or double to quotient
    given QuotientConversion: Conversion[Int, Quotient] with {
        def apply(n: Int): Quotient = new Quotient(n, 1)
    }
}