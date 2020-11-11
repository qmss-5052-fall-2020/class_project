
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
from typing import Generator, Tuple
from collections import namedtuple
import matplotlib.pyplot as plt
import copy


# In[9]:


def sir(
    s: float, 
    i: float, 
    r: float, 
    beta: float, 
    gamma: float
) -> Tuple[float, float, float]:
    
    """The SIR model, one time step."""
    n = s + i + r
    
    s_n = (- beta * s * i) + s
    i_n = (beta * s * i - gamma * i) + i
    r_n = gamma * i + r
    if s_n < 0.0:
        s_n = 0.0
    if i_n < 0.0:
        i_n = 0.0
    if r_n < 0.0:
        r_n = 0.0

    scale = n / (s_n + i_n + r_n)
    return s_n * scale, i_n * scale, r_n * scale


def dynamic_sim_sir(
    s, i, r, gamma, i_day, 
    *args ## (beta vector, n_days)
) -> Generator[Tuple[int, float, float, float], None, None]:
    
    """Simulate SIR model forward in time yielding tuples.
    Parameter order has changed to allow multiple (beta, n_days)
    to reflect multiple changing social distancing policies.
    """
    
    s, i, r = (float(v) for v in (s, i, r))
    
    n = s + i + r
    
    Days = i_day
    
    while args:
        beta, n_days, *args = args
        for idx in range(n_days):
            yield Days, s, i, r
            s, i, r = sir(s, i, r, beta[idx], gamma)
            Days += 1
    
    yield Days, s, i, r



def dynamic_sim_sir_df(
    s, i, r, gamma,
    i_day: int, 
    *args ## (beta vector, n_days)
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    
    """Simulate the SEIR model forward in time."""
    
    result_complete = pd.DataFrame(
        data = dynamic_sim_sir(s, i, r, gamma, i_day, *args),
        columns=("day", "susceptible", "infected", "recovered")
    )
    
     
    return result_complete 


# ## Try

# In[13]:


# result = dynamic_sim_sir_df(1e6, 1e3, 0, 1/14, 0, [1e-6] * 60, 60)
# result[['infected']].plot()

