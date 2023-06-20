import numpy as np
import math

class DualNumber:
    _supported_scalars = (int, float)
    def __init__(self, real, dual=1.0):
        """Initialize Dual number"""
        self.real = real
        self.dual = dual
    
    def __add__(self, other):
        """Dual add function"""
        if not isinstance(other, (*self._supported_scalars, DualNumber)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            return DualNumber(other + self.real, self.dual)
        else:
            return DualNumber(self.real + other.real, self.dual + other.dual)

    def __radd__(self, other):
        """Dual add scalar function"""
        return self.__add__(other)

    def __mul__(self, other):
        """Dual multiply function"""
        if not isinstance(other, (*self._supported_scalars, DualNumber)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            return DualNumber(other * self.real, other * self.dual)
        else:
            return DualNumber(self.real * other.real, self.real * other.dual + self.dual * other.real)

    def __rmul__(self, other):
        """Dual add multiply function"""
        return self.__mul__(other)
    
    def __sub__(self, other):
        """Dual add function"""
        if not isinstance(other, (*self._supported_scalars, DualNumber)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            return DualNumber(self.real - other, self.dual)
        else:
            return DualNumber(self.real - other.real, self.dual - other.dual)

    def __rsub__(self, other):
        return self.__sub__(other)

    def __truediv__(self, other):
        """Dual add function"""
        if not isinstance(other, (*self._supported_scalars, DualNumber)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            if other == 0:
                raise ZeroDivisionError
            return DualNumber(self.real / other, (self.dual * other) / other**2)
        else:
            if other.real == 0:
                raise ZeroDivisionError
            return DualNumber(self.real / other.real, (self.dual * other.real - self.real * other.dual) / other.real**2)

    def __rtruediv__(self, other):
        return DualNumber(other, 0).__truediv__(self)
    
    def __pow__(self, other):
        """Dual power function"""
        if not isinstance(other, (*self._supported_scalars, DualNumber)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            return DualNumber(np.power(self.real, other.real), np.power(self.real, other - 1) * other * self.dual)
        else:
            return DualNumber(np.power(self.real, other.real), np.power(self.real, other.real - 1) * other.real * self.dual
                                                                + np.log(self.real) * np.power(self.real, other.real) * other.dual)

    def __rpow__(self, other):
        """Dual power function"""
        return DualNumber(other, 0).__pow__(self)

    def __neg__(self):
        """Dual negate function"""
        return self * -1

def exp(x: DualNumber):
    """Dual exp function"""
    return DualNumber(np.exp(x.real), np.exp(x.real) * x.dual)

def log(b: float, x: DualNumber):
    """Dual log function"""
    return DualNumber(np.log(x.real)/np.log(b), (1/x.real)/(np.log(b)) * x.dual)

def cos(x: DualNumber):
    """Dual cos function"""
    return DualNumber(np.cos(x.real), -np.sin(x.real) * x.dual)

def sin(x: DualNumber):
    """Dual sin function"""
    return DualNumber(np.sin(x.real), np.cos(x.real) * x.dual)

def tan(x: DualNumber):
    return DualNumber(np.tan(x.real), (1/(np.cos(x.real))**2) * x.dual)

def sqrt(x: DualNumber):
    """Dual sin function"""
    return x**(1/2)

def logistic(x: DualNumber):
    return 1/(1+exp(-x))

def sinh(x: DualNumber):
    return DualNumber(np.sinh(x.real), np.cosh(x.real) * x.dual)

def cosh(x: DualNumber):
    return DualNumber(np.cosh(x.real), np.sinh(x.real) * x.dual)

def tanh(x: DualNumber):
    return DualNumber(np.tanh(x.real), (1/(np.cosh(x.real))**2) * x.dual)

def arccos(x: DualNumber):
    """Dual cos function"""
    return DualNumber(np.arccos(x.real), (-1/sqrt(1-x.real**2)) * x.dual)

def arcsin(x: DualNumber):
    """Dual sin function"""
    return DualNumber(np.arcsin(x.real), (1/sqrt(1-x.real**2)) * x.dual)

def arctan(x: DualNumber):
    return DualNumber(np.arctan(x.real), (1/(1+x.real**2)) * x.dual)


# x = DualNumber(3, 1)
# y = DualNumber(7, 0)
# c = lambda x, y: ((x ** 2)) ** (1 / 3)
# print(c(*tuple([x, y])).real)
# print(c(*tuple([x, y])).dual)