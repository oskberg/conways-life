# %%
import numpy as np
import matplotlib.pyplot as plt
import fractions
import pandas as pd
from pandas.core.frame import DataFrame
# %%
lines = pd.read_csv('../output/210128-lines-4.csv')

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
cols = ['Initial', 'Final', 'Generations', 'Adjusted Gen','Stable', 'Dynamic', 'Period']
out = pd.DataFrame(columns=cols)
step = 10000 
for gen in range(1,149):
    current = lines_out[(lines_out['generation #'] < (gen * step + step) ) & (lines_out['generation #'] >= gen * step)]
    stable = 0
    dynamic = 0
    max_period = 0
    gens = len(current)
    adjusted_gen = gens

    # if len(current[' BORN']) < 1:
    #     print(gen)
    #     continue
    if current[' BORN'].iloc[-1] == 0: 
        stable = 1
        dynamic = 0
    elif gens > 200:
        start = 100
        sp = np.fft.fft(current[' # alive cells'][-start:])
        freq = np.fft.fftfreq(current['generation #'][-start:].shape[-1])

        useful_freq = []
        freq_mag = []
        for fr, spr in zip(freq, sp.real):
            if abs(spr) > 10 :
                useful_freq.append(fr)
                freq_mag.append(spr)
        useful_freq = useful_freq[1:int((len(useful_freq))/2)]
        periods = [round(1/f) for f in np.array(useful_freq)]
        if len(periods) > 0 and (fr % start != 0) :
            max_period = max(periods)
        else:
            max_period = -1
        if max_period > 1:
            live_cells = list(current[' # alive cells'])
            ref_val = live_cells[-1]
            for i in range(1, int(len(live_cells)/max_period)):
                if live_cells[-(i * max_period + 1)] != ref_val:
                    print(gen, i, max_period, live_cells[-(i * max_period + 1)], ref_val)
                    adjusted_gen = gens - ( i * max_period)
                    break

         
    row = {
        'Initial' : current[' # alive cells'].iloc[0],
        'Final':current[' # alive cells'].iloc[-1],
        'Generations': gens,
        'Adjusted Gen':adjusted_gen,
        'Stable':stable,
        'Dynamic':dynamic, 
        'Period':max_period,
        }
    out = out.append(row, ignore_index=True)

# %%
out.iloc[9]
# %%
# %%
current[' BORN'].iloc[-1] == 0
# %%
current[current[' # alive cells'][-10:] == current[' # alive cells'].iloc[-1]]
# %%
out.plot(x='Initial', y='Generations', title='Stabilisation Time')
# %%
gen = 41
step= 10000
gen_10 = lines_out[(lines_out['generation #'] < (gen + step) ) & (lines_out['generation #'] >= gen)]
# %%
gen_10.plot(x='generation #', y=' # alive cells',)
# %%
gen_10_2 = lines_out[(lines_out['generation #'] < (gen * step + step) ) & (lines_out['generation #'] >= gen * step)]
# %%
gen_10_2.plot(x='generation #', y=' # alive cells',  title='Stabilisation Time')
# %%
start = 100
sp = np.fft.fft(gen_10_2[' # alive cells'][-start:])
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

peaks[0][-1] - peaks[0][-2]
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
