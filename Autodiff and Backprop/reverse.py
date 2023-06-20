import numpy as np

class Variable:
    _supported_scalars = (int, float)
    def __init__(self, value, children=[]):
        self.value = value
        self.children = children
        self.deriv = 0

    def __add__(self, other):
        if not isinstance(other, (*self._supported_scalars, Variable)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            other = Variable(other)
        return Variable(self.value + other.value, [(self, 1.0), (other, 1.0)])

    def __mul__(self, other):
        if not isinstance(other, (*self._supported_scalars, Variable)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            other = Variable(other)
        return Variable(self.value * other.value, [(self, other.value), (other, self.value)])
    
    def __sub__(self, other):
        if not isinstance(other, (*self._supported_scalars, Variable)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            other = Variable(other)
        return Variable(self.value - other.value, [(self, 1.0), (other, -1.0)])

    def __pow__(self, other):
        if not isinstance(other, (*self._supported_scalars, Variable)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            other = Variable(other)
        return Variable((self.value**other.value), [(self, other.value * (self.value**(other.value - 1))),
                                                    (other, np.log(self.value) * (self.value**other.value))])

    def __neg__(self):
        return self * -1

    def __radd__(self, other):
        return Variable(other).__add__(self)

    def __rsub__(self, other):
        return Variable(other).__sub__(self)

    def __rmul__(self, other):
        return Variable(other).__mul__(self)

    def __truediv__(self, other):
        if not isinstance(other, (*self._supported_scalars, Variable)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            other = Variable(other)
        return Variable(self.value / other.value, [(self, (other.value) / other.value**2), (other, (- self.value) / other.value**2)])

    def __rtruediv__(self, other):
         return Variable(other).__truediv__(self)

    def __rpow__(self, other):
        """Dual power function"""
        return Variable(other).__pow__(self)
    
    def _derivatives(self):
        self.deriv = 1.0
        def calc_derivs(vari, adjoint):
            for child, local_deriv in vari.children:
                next_adjoint = adjoint * local_deriv
                child.deriv += next_adjoint
                calc_derivs(child, next_adjoint)
        calc_derivs(self, 1.0)

def sin(x: Variable):
    return Variable(np.sin(x.value), [(x, np.cos(x.value))])

def cos(x: Variable):
    return Variable(np.cos(x.value), [(x, -np.sin(x.value))])

def tan(x: Variable):
    return Variable(np.tan(x.value), [(x, 1/(np.cos(x.value))**2)])
    
def log(b: float, x: Variable):
    return Variable(np.log(x.value)/np.log(b), [(x, 1/(x.value*np.log(b)))])

def exp(x: Variable):
    """Dual exp function"""
    return Variable(np.exp(x.value), [(x, np.exp(x.value))])

def sqrt(x: Variable):
    """Dual sin function"""
    return x**(1/2)

def logistic(x: Variable):
    return 1/(1+exp(-x))

def sinh(x: Variable):
    return Variable(np.sinh(x.value), [(x, np.cosh(x.value))])

def cosh(x: Variable):
    return Variable(np.cosh(x.value), [(x, np.sinh(x.value))])

def tanh(x: Variable):
    return Variable(np.tanh(x.value), [(x, 1/(np.cosh(x.value))**2)])

def arcsin(x: Variable):
    return Variable(np.arcsin(x.value), [(x, (1/sqrt(1-x.value**2)))])

def arccos(x: Variable):
    return Variable(np.arccos(x.value), [(x, (-1/sqrt(1-x.value**2)))])

def arctan(x: Variable):
    return Variable(np.arctan(x.value), [(x, (1/(1+x.value**2)))])

# x = Variable(0.65)
# y = Variable(3)
# c = 2**x
# c._derivatives()
# print(c.value)
# print(x.value)
# print(x.deriv)