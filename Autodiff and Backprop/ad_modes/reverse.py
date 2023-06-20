import numpy as np

class Variable:
    _supported_scalars = (int, float)
    def __init__(self, value, children=[]):
        """Initialize Variable object"""
        self.value = value
        self.children = children
        self.deriv = 0

    def __add__(self, other):
        """Variable class add function"""
        if not isinstance(other, (*self._supported_scalars, Variable)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            other = Variable(other)
        return Variable(self.value + other.value, [(self, 1.0), (other, 1.0)])

    def __mul__(self, other):
        """Variable class multiply function"""
        if not isinstance(other, (*self._supported_scalars, Variable)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            other = Variable(other)
        return Variable(self.value * other.value, [(self, other.value), (other, self.value)])
    
    def __sub__(self, other):
        """Variable class subtract function"""
        if not isinstance(other, (*self._supported_scalars, Variable)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            other = Variable(other)
        return Variable(self.value - other.value, [(self, 1.0), (other, -1.0)])

    def __pow__(self, other):
        """Variable class power function"""
        if not isinstance(other, (*self._supported_scalars, Variable)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            other = Variable(other)
        return Variable((self.value**other.value), [(self, other.value * (self.value**(other.value - 1))),
                                                    (other, np.log(self.value) * (self.value**other.value))])

    def __neg__(self):
        """Variable class negate function"""
        return self * -1

    def __radd__(self, other):
        """Variable class add scalar function"""
        return Variable(other).__add__(self)

    def __rsub__(self, other):
        """Variable class subtract scalar function"""
        return Variable(other).__sub__(self)

    def __rmul__(self, other):
        """Variable class multiply scalar function"""
        return Variable(other).__mul__(self)

    def __truediv__(self, other):
        """Variable class true divide function"""
        if not isinstance(other, (*self._supported_scalars, Variable)):
            raise TypeError(f"Unsupportedtype `{type(other)}`")
        if isinstance(other, self._supported_scalars):
            other = Variable(other)
        return Variable(self.value / other.value, [(self, (other.value) / other.value**2), (other, (- self.value) / other.value**2)])

    def __rtruediv__(self, other):
        """Variable class true divide scalar function"""
         return Variable(other).__truediv__(self)

    def __rpow__(self, other):
        """Variable class power scalar function"""
        return Variable(other).__pow__(self)
    
    def _derivatives(self):
        """
        Having kept track of the dependencies in the computational graph that represents the sequence of
        elementary arithmetic operations performed during a calculation through the implementation of the
        Variable class, leverage these dependecies and start from the output of the calculation, traverse
        the graph in reverse, applying the chain rule at each step to compute the gradient of the output
        with respect to the input variables.
        Parameters
        ----------
        None
        Returns
        -------
        None
            The method updates the gradients with respect to each input of
            the inputted function.
        """
        self.deriv = 1.0
        def calc_derivs(vari, adjoint):
            for child, local_deriv in vari.children:
                next_adjoint = adjoint * local_deriv
                child.deriv += next_adjoint
                calc_derivs(child, next_adjoint)
        calc_derivs(self, 1.0)

def sin(x: Variable):
    """Variable class sin function"""
    return Variable(np.sin(x.value), [(x, np.cos(x.value))])

def cos(x: Variable):
    """Variable class cos function"""
    return Variable(np.cos(x.value), [(x, -np.sin(x.value))])

def tan(x: Variable):
    """Variable class tan function"""
    return Variable(np.tan(x.value), [(x, 1/(np.cos(x.value))**2)])
    
def log(b: float, x: Variable):
    """Variable class log base b of x function"""
    return Variable(np.log(x.value)/np.log(b), [(x, 1/(x.value*np.log(b)))])

def exp(x: Variable):
    """Variable class exp function"""
    return Variable(np.exp(x.value), [(x, np.exp(x.value))])

def sqrt(x: Variable):
    """Variable class square root function"""
    return x**(1/2)

def logistic(x: Variable):
    """Variable class logistic function"""
    return 1/(1+exp(-x))

def sinh(x: Variable):
    """Variable class sinh function"""
    return Variable(np.sinh(x.value), [(x, np.cosh(x.value))])

def cosh(x: Variable):
    """Variable class cosh function"""
    return Variable(np.cosh(x.value), [(x, np.sinh(x.value))])

def tanh(x: Variable):
    """Variable class tanh function"""
    return Variable(np.tanh(x.value), [(x, 1/(np.cosh(x.value))**2)])

def arcsin(x: Variable):
    """Variable class arcsin function"""
    return Variable(np.arcsin(x.value), [(x, (1/sqrt(1-x.value**2)))])

def arccos(x: Variable):
    """Variable class arccos function"""
    return Variable(np.arccos(x.value), [(x, (-1/sqrt(1-x.value**2)))])

def arctan(x: Variable):
    """Variable class arctan function"""
    return Variable(np.arctan(x.value), [(x, (1/(1+x.value**2)))])

# x = Variable(0.65)
# y = Variable(3)
# c = 2**x
# c._derivatives()
# print(c.value)
# print(x.value)
# print(x.deriv)