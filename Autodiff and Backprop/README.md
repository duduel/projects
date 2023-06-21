## How To Use this package:

The code has been deployed to PyPI and can thus be downloaded with `pip` and installed by running these commands in the command line:

```shell
pip install --upgrade pip
pip install numpy
pip install -i https://test.pypi.org/simple/ autodiff-team52==0.0.1
```
$\pi \approx 3.14159$

Using the package is straightforward. The user needs to import the module \texttt{ad} which contains the function $\texttt{autodiff}$
that takes in 4 inputs. The function is as follows: $\texttt{autodiff(f, input_dict, seed=None, autodiff_mode=0)}$.


The first input is a function on which the user wishes to run Automatic differentiation on. The second input is the point of the
function at which one needs to calculate the derivative (or gradient) with the coordinates specified in the form of a Python
dictionary. The third input is the seed vector; this applies to forward automatic differentiation where one needs to evaluate along a
given seed vector: for instance or depending on what the user wants. Then finally, the last input is a binary indicator for either
forward or reverse mode automatic differentiation: 0 for forward mode and 1 for reverse mode.

The last two inputs \texttt{seed} and \texttt{autodiff_mode} are optional (\texttt{seed} set to \texttt{None} by default, and
\texttt{autodiff_mode} set 0 by default). The user will only need to specify them when necessary: \texttt{seed} will always need to be
specified for forward mode while \texttt{autodiff_mode} will always be specified for reverse mode; that is, it will need to be set to 1
if the user is looking for reverse mode. For forward mode, the return value of the function will always be an int or float, while for
reverse mode, the return value of the function will always be a dictionary that represents the gradient of the function at the
specified point.

Python code:

``` shell
from autodiff_team52 import ad
forward_x = ad.autodiff(lambda x, y: x*y, {'x':3, 'y':7}, seed='x', autodiff_mode=0)
print(forward_x)

# Output: 7.0

forward_y = ad.autodiff(lambda x, y: x*y, {'x':3, 'y':7}, seed='y', autodiff_mode=0)
print(forward_y)

# Output: 3.0

reverse_grad = ad.autodiff(lambda x, y: x*y, {'x':3, 'y':7}, seed=None, autodiff_mode=1)
print(reverse_grad)

# Output: {'x': 7.0, 'y': 3.0}
One interesting use case: Jacobian for vector functions (useful for Newton's method)

import numpy as np
f(x_1, x_2, ..., x_n) = [f_1, f_2, ..., f_m] \quad (\text{Jacobian evaluated at} \; (v_1, v_2, ..., v_n))
jacobian_2d_list = []
for f_i in f:
    reverse_grad = ad.autodiff(lambda x_1, x_2, ..., x_n: f_i, {'x_1': v_1, 'x_2': v_2, ..., 'x_n': v_n}, seed=None, autodiff_mode=
