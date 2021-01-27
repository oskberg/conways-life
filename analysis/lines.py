# %%
import numpy as np
import matplotlib.pyplot as plt
import fractions
import pandas as pd
from pandas.core.frame import DataFrame
# %%
lines = pd.read_csv('../output/LINES-break-140-3.csv')

# %%
lines
# %%
# lines_out=lines[lines[' # alive cells'] != 0]
lines_out = lines
# %%
gen = 5000
step = 10000 
lines_out[(lines_out['generation #'] < (gen + step) ) & (lines_out['generation #'] >= gen)]
# %%
cols = ['Initial', 'Final', 'Generations','Stable', 'Dynamic']
out = pd.DataFrame(columns=cols)
step = 10000 
for gen in range(1,82):
    current = lines_out[(lines_out['generation #'] < (gen * step + step) ) & (lines_out['generation #'] >= gen * step)]
    stable = 0
    dynamic = 0
    gens = len(current)

    if current[' BORN'].iloc[-1] == 0 or gens < 1000: 
        stable = 1
        dynamic = 0
    else: 
        dynamic = 1
    

    
    row = {
        'Initial' : current[' # alive cells'].iloc[0],
        'Final':current[' # alive cells'].iloc[-1],
        'Generations': gens,
        'Stable':stable,
        'Dynamic':dynamic, 
        }
    out = out.append(row, ignore_index=True)

# %%
out
# %%
# %%
current[' BORN'].iloc[-1] == 0
# %%
current[current[' # alive cells'][-10:] == current[' # alive cells'].iloc[-1]]
# %%
out.plot(x='Initial', y='Generations')
# %%
gen = 10
step= 10000
gen_10 = lines_out[(lines_out['generation #'] < (gen + step) ) & (lines_out['generation #'] >= gen)]
# %%
gen_10.plot(x='generation #', y=' # alive cells')
# %%
gen_10_2 = lines_out[(lines_out['generation #'] < (gen * step + step) ) & (lines_out['generation #'] >= gen * step)]
# %%
gen_10_2.plot(x='generation #', y=' # alive cells')
# %%
start = 10
sp = np.fft.fft(gen_10_2[' # alive cells'][start:])
# %%
freq = np.fft.fftfreq(gen_10_2['generation #'][start:].shape[-1])
# %%
plt.plot(freq, sp.real)
# %%
plt.plot(gen_10_2['generation #'], gen_10_2[' # alive cells'])
plt.xlim((100200,100300))
# %%
useful_freq = []
freq_mag = []
for fr, spr in zip(freq, sp.real):
    if abs(spr) > 100:
        useful_freq.append(fr)
        freq_mag.append(spr)
# %%
plt.scatter(useful_freq, freq_mag)
# %%
x = np.linspace(100200, 100300, 1000)
plt.plot(gen_10_2['generation #'], gen_10_2[' # alive cells'])
for f in useful_freq:
    plt.plot(x, 20 + np.sin(x * f))
    

plt.xlim((100200,100300))
# %%
peaks = np.where(gen_10_2[' # alive cells'] > 35)
# %%
# %%
for i, p in enumerate(peaks[:-1]):
    print(peaks[i+1]-p)
# %%
diffs = []
for i, p in enumerate(peaks[0]):
    if i < 10:
        val = peaks[i+1]-p
        diffs.append(val)
# %%

# %%
for p in peaks[0]:
    print(p)
# %%
peaks[0][4] - peaks[0][3]
# %%
useful_freq = useful_freq[1:int((len(useful_freq)+1)/2)]

# %%
peridos = [round(1/f) for f in np.array(useful_freq)]
# %%
peridos
# %%
out[out['Generations'] > 1000]
# %%
out
# %%
