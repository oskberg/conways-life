# %%
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
# %%
# csv = pd.read_csv('../output/light-spaceship-c-test.csv')
csv = pd.read_csv('../output/spaceship.csv')

# %%
csv
# %%
csv.plot(x='generation #', y=' avg x pos')
# %%
csv.plot(x='generation #', y=' avg y pos ')

# %%
