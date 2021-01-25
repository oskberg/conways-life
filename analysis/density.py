# %%
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
# %%
csv = pd.read_csv('../output/LONG_RND.csv')
# %%
csv.plot(x='generation #', y=' # alive cells')
# %%
total_cells = csv.get(' # alive cells')[0] + csv.get(' # dead cells')[0]
activity = []
last_frac = 0.5
for alive in csv.get(' # alive cells'):
    frac = alive / total_cells
    activity.append(frac - last_frac)
    last_frac = frac
# %%
csv['activity'] = activity
# %%
csv
# %%
csv['activity']
# %%
plt.scatter(csv['generation #'][:], csv['activity'][:], s=1)
plt.ylim((-0.005,0.005))
# plt.yscale('log')
# %%
plt.scatter(csv['generation #'], csv[' # alive cells'], s=1)
# plt.ylim((1,10000))
plt.yscale('log')

# %%
start = 2000
sp = np.fft.fft(activity[start:])
# %%
freq = np.fft.fftfreq(csv['generation #'][start:].shape[-1])
# %%
plt.scatter(freq, sp.real, s=1)

# %%
