from math import pi
from importlib import reload
import numpy as np
import scipy
import matplotlib.pyplot as plt

plt.ion()

print("Imported matplotlib, numpy, scipy, reload, and pi")

# ENABLE AUTORELOAD
ip = get_ipython()
ip.run_line_magic('load_ext', 'autoreload')
ip.run_line_magic('autoreload', '2')
print("Imported autoreload. (Use magic '%autoreload 0' to disable,)")
