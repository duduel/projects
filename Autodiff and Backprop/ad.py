from .forward import *
from .reverse import *

def autodiff(f, inputs, seed=None, autodiff_mode=0):
    variables = {}
    if autodiff_mode ==0:
        for i in inputs.keys():
            if seed == i:
                variables[i] = DualNumber(inputs[i], 1)
            else:
                variables[i] = DualNumber(inputs[i], 0)
        results = f(*tuple(variables[i] for i in inputs.keys()))
        return results.dual
    elif autodiff_mode ==1:
        for i in inputs.keys():
            variables[i] = Variable(inputs[i])
        results = f(*tuple(variables[i] for i in inputs.keys()))
        results._derivatives()
        return {x: variables[x].deriv for x in inputs.keys()}
    else:
        return

# print(autodiff(lambda x, y: x * y, {'x': 0.65, 'y': 3}, seed='x', autodiff_mode=1))