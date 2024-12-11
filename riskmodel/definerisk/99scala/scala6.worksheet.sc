

def isprime(n:Int): Boolean = 
    (n > 1) && (2 to Math.sqrt(n).toInt).exists( x => n % x == 0 ) == false

def gcd(n1: Int, n2: Int) : Int = 
    if (n2 == 0) n1 else gcd(n2,n1 % n2)

def isCoprime(n1: Int, n2: Int) : Boolean =
    oprime
    gcd(n1,n2) == 1

isprime(10)
isprime(7)
isprime(35)
isprime(64)
gcd(35,64)
gcd(10,5)
gcd(36,63)
isCoprime(35,64)
