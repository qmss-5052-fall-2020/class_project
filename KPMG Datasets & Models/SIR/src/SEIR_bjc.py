
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
from typing import Generator, Tuple
from collections import namedtuple
import matplotlib.pyplot as plt
import copy


# ### SEIR BJC Model
# 
# mimic similar code structure as the streamlit app

# In[6]:


def seir_bjc(
    S: float,  # Susceptible
    E: float, # Exposed 
    IR: float, # Infected (not hosp)
    IH: float, # Infected (hosp)
    R: float, # Recovered (from IR)
    H: float, # Hospitalized (from IH)
    D: float, # Discahrged (from H)
    beta: float, # from S -> I
    sigma: float, # from E -> I
    hosp_prop: float, # percent of E that go to IH
    gamma_r: float, # IR -> R
    gamma_h: float, # IH -> H
    psi: float # H -> D
) -> Tuple[float, float, float, float, float, float]:
    
    """The SEIR model in BJC, one time step."""
    
    N = S + E + IR + IH + R + H + D
    
    dS = - beta * S * (IR + IH)
    dE = beta * S * (IR + IH) - sigma * E
    dIR = sigma * E * (1 - hosp_prop) - gamma_r * IR
    dIH = sigma * E * hosp_prop - gamma_h * IH
    dR = gamma_r * IR
    dH = gamma_h * IH - psi * H
    dD = psi * H
    
    S_t = S + dS
    E_t = E + dE
    IR_t = IR + dIR
    IH_t = IH + dIH
    R_t = R + dR
    H_t = H + dH
    D_t = D + dD
    
    
    if S_t < 0.0:
        S_t = 0.0
    if E_t < 0.0:
        E_t = 0.0
    if IR_t < 0.0:
        IR_t = 0.0
    if IH_t < 0.0:
        IH_t = 0.0
    if R_t < 0.0:
        R_t = 0.0
    if H_t < 0.0:
        H_t = 0.0
    if D_t < 0.0:
        D_t = 0.0

    scale = N / (S_t + E_t + IR_t + IH_t + R_t + H_t + D_t)
    
    # order is improtant
    return S_t * scale, E_t * scale, IR_t * scale, IH_t * scale, R_t * scale, H_t * scale, D_t * scale


# In[7]:


def dynamic_sim_seir_bjc(
    S0: float, E0: float, I0: float, R0: float, H0: float, D0: float,
    sigma: float, hosp_prop: float, gamma_r: float, gamma_h : float, psi: float, 
    i_day: int,
    *args ## (beta vector, n_days)
) -> Generator[Tuple[int, float, float, float, float, float, float, float], None, None]:
    """Simulate SEIR model forward in time yielding tuples.
    Parameter order has changed to allow multiple (beta, n_days)
    to reflect multiple changing social distancing policies.
    """
    
    S, E, IR, IH, R, H, D = (float(v) for v in (S0, E0, I0 * (1-hosp_prop), I0 * hosp_prop, R0, H0, D0))
    
    N = S + E + IR + IH + R + H + D
    
    Days = i_day
    
    while args:
        beta, n_days, *args = args
        for idx in range(n_days):
            yield Days, S, E, IR, IH, R, H, D
            S, E, IR, IH, R, H, D = seir_bjc(S, E, IR, IH, R, H, D, 
                                             beta[idx], sigma, hosp_prop, gamma_r, gamma_h, psi)
            Days += 1
    
    yield Days, S, E, IR, IH, R, H, D


# In[8]:


def dynamic_sim_seir_bjc_df(
    S0: float, E0: float, I0: float, R0: float, H0: float, D0: float,
    sigma: float, hosp_prop: float, gamma_r: float, gamma_h : float, psi: float, 
    i_day: int, 
    *args ## (beta vector, n_days)
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    
    """Simulate the SEIR model forward in time."""
    
    result_complete = pd.DataFrame(
        data = dynamic_sim_seir_bjc(S0, E0, I0, R0, H0, D0, sigma, hosp_prop, gamma_r, gamma_h, psi, i_day, *args),
        columns=("day", "susceptible", "exposed", "infected_r", "infected_h","recovered_r", 
                "hospitalized", "discharged")
    )
    
    result_simplify = result_complete
    result_simplify['infected'] = result_simplify['infected_r'] + result_simplify['infected_h']
    result_simplify['recovered'] = result_simplify['recovered_r'] + result_simplify['discharged']
    result_simplify = result_simplify[["day", "susceptible", "exposed", 'infected', 'hospitalized', 'recovered']]
    
    return result_simplify, result_complete 


# ## Try

# In[57]:


simple_result, complete_result = dynamic_sim_seir_bjc_df(
                        1e6, 
                        2e4, 
                        1e4, 
                        0, 
                        0, 
                        0,
                        1/3, # incubation period between exposed and infected
                        0.1, # hosp rate
                        1/7, # recovery rate for non-hospitalized
                        1/10, # 1 over time to get hospitalized
                        1/20, # discharge rate for hospitalized
                        0, # start day: 0
                        [1e-8]*100, # dynamic beta
                        60)

simple_result[['infected', 'hospitalized']].plot()


# incubtion period increase (1/3 to 1/20)

# In[58]:


simple_result, complete_result = dynamic_sim_seir_bjc_df(
                        1e6, 
                        2e4, 
                        1e4, 
                        0, 
                        0, 
                        0,
                        1/20, # incubation period between exposed and infected
                        0.1, # hosp rate
                        1/7, # recovery rate for non-hospitalized
                        1/10, # 1 over time to get hospitalized
                        1/20, # discharge rate for hospitalized
                        0, # start day: 0
                        [1e-8]*100, # dynamic beta
                        60)

simple_result[['infected', 'hospitalized']].plot()


# recovery rate for non-hospitalized increase (1/7 to 1/2)

# In[59]:


simple_result, complete_result = dynamic_sim_seir_bjc_df(
                        1e6, 
                        2e4, 
                        1e4, 
                        0, 
                        0, 
                        0,
                        1/20, # incubation period between exposed and infected
                        0.1, # hosp rate
                        1/2, # recovery rate for non-hospitalized
                        1/10, # 1 over time to get hospitalized
                        1/20, # discharge rate for hospitalized
                        0, # start day: 0
                        [1e-8]*100, # dynamic beta
                        60)

simple_result[['infected', 'hospitalized']].plot()

