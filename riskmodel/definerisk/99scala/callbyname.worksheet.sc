def whileLoop(condition: => Boolean)(body: => Unit): Unit =
  if condition then
    body
    whileLoop(condition)(body)

var i = 2

whileLoop (i > 0) {
  println(i)
  i -= 1
}  // prints 2 1

def calculate(input: => Int) = 
    println(input * 2)
    println(input * 2)

var j = 2
def add(): Int =
    j = j + 2
    j
calculate(add())
j = 1
calculate({
            j = j + 2
            j 
})