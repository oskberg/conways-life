# %%
import numpy as np
import matplotlib.pyplot as plt
import fractions
import pandas as pd
# %%
csv = pd.read_csv('../output/glider-c-test.csv')
# csv = pd.read_csv('../output/spaceship.csv')

# %%
csv
# %%
csv.plot(x='generation #', y=' avg x pos')
# %%
csv.plot(x='generation #', y=' avg y pos ')

# %%
from numpy import polyfit
# %%
gen = csv['generation #']
# %%
avg_x = csv[' avg y pos ']
# %%
start = 1
end = 150
opt = polyfit(gen[start:end], avg_x[start:end], 1)

x = np.linspace(gen[start], gen[end], 5)
plt.plot(gen[start:end], avg_x[start:end])
plt.plot(x, x * opt[0] + opt[1])
# %%
print('speed =',opt[0])
print('speed =',str(fractions.Fraction(opt[0]).limit_denominator()))
# %%

# %%
