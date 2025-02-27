import numpy as np

class DualNumber:
    def __init__(self, real, dual=0.0):
        self.real = real
        self.dual = dual

    def __repr__(self):
        return f"DualNumber({self.real}, {self.dual})"

    def __add__(self, other):
        if isinstance(other, DualNumber):
            return DualNumber(self.real + other.real, self.dual + other.dual)
        else:
            return DualNumber(self.real + other, self.dual)

    __radd__ = __add__  # For commutative addition

    def __sub__(self, other):
        if isinstance(other, DualNumber):
            return DualNumber(self.real - other.real, self.dual - other.dual)
        else:
            return DualNumber(self.real - other, self.dual)

    def __rsub__(self, other):
        return DualNumber(other - self.real, -self.dual)

    def __mul__(self, other):
        if isinstance(other, DualNumber):
            return DualNumber(self.real * other.real, self.real * other.dual + self.dual * other.real)
        else:
            return DualNumber(self.real * other, self.dual * other)

    __rmul__ = __mul__ # For commutative multiplication

    def __truediv__(self, other):
        if isinstance(other, DualNumber):
            return DualNumber(self.real / other.real, (self.dual * other.real - self.real * other.dual) / (other.real * other.real))
        else:
            return DualNumber(self.real / other, self.dual / other)

    def __rtruediv__(self, other):
        return DualNumber(other / self.real, (-other * self.dual) / (self.real * self.real))

    def __pow__(self, other):
        if isinstance(other, DualNumber):
            return DualNumber(self.real**other.real, self.real**other.real * (self.dual * other.real / self.real + other.dual * np.log(self.real)))
        else:
            return DualNumber(self.real**other, other * self.real**(other - 1) * self.dual)

    def exp(self):
        return DualNumber(np.exp(self.real), np.exp(self.real) * self.dual)

    def sin(self):
        return DualNumber(np.sin(self.real), np.cos(self.real) * self.dual)

    def cos(self):
        return DualNumber(np.cos(self.real), -np.sin(self.real) * self.dual)

    def log(self):
        return DualNumber(np.log(self.real), self.dual / self.real)

# Example usage:
def f(x):
    return 3 * x**2 + 2 * x + 1

# Calculate derivative at x = 2
x = DualNumber(2.0, 1.0) # real part is value, dual part is derivative direction.
result = f(x)
print(f"f(2) = {result.real}")
print(f"f'(2) = {result.dual}")

def g(x,y):
  return x.sin() + y.exp()

x = DualNumber(np.pi/2, 1.0)
y = DualNumber(1.0, 0.0)

result = g(x,y)
print(f"g(pi/2,1) = {result.real}")
print(f"dg/dx(pi/2,1) = {result.dual}")

x = DualNumber(np.pi/2, 0.0)
y = DualNumber(1.0, 1.0)

result = g(x,y)
print(f"g(pi/2,1) = {result.real}")
print(f"dg/dy(pi/2,1) = {result.dual}")