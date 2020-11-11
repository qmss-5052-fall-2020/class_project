
# coding: utf-8

# In[9]:


import pandas as pd
import numpy as np
from typing import Generator, Tuple
from collections import namedtuple
import matplotlib.pyplot as plt
import copy


# ### SEIR CovidActNow Model
# 
# mimic similar code structure as the streamlit app

# In[10]:


def seir_actnow(
    S: float,  # Susceptible
    E: float, # Exposed 
    I_Mild: float, # Infected (mild, not hosp)
    I_Hosp: float, # Infected (hosp)
    I_ICU: float, # Infected (ICU)
    R: float, # Recovered (from 3 types of Infected)
    D: float, # Deceased (from I_C)
    beta_1: float, # from S -> Infected (mild, not hosp)
    beta_2: float, # from S -> Infected (hosp)
    beta_3: float, # from S -> Infected (ICU)
    alpha: float, # from E -> Infected (mild, not hosp)
    gamma_1: float, # Infected (mild, not hosp)-> R
    gamma_2: float, # Infected (hosp) -> R
    gamma_3: float, # Infected (ICU) -> R
    pop_1: float, # Infected (mild, not hosp) -> Infected (hosp)
    pop_2: float, # Infected (hosp) -> Infected (ICU)
    psi: float # Infected (ICU) -> D
) -> Tuple[float, float, float, float, float, float]:
    
    """The SEIR model in BJC, one time step."""
    
    N = S + E + I_Mild + I_Hosp + I_ICU + R + D
    
    dS = (- beta_1 * S * I_Mild - beta_2 * S * I_Hosp - beta_3 * S * I_ICU)
    dE = (beta_1 * S * I_Mild + beta_2 * S * I_Hosp + beta_3 * S * I_ICU) - alpha * E
    dI_Mild = alpha * E  - gamma_1 * I_Mild - pop_1 * I_Mild
    dI_Hosp = pop_1 * I_Mild  - gamma_2 * I_Hosp - pop_2 * I_Hosp
    dI_ICU  = pop_2 * I_Hosp  - gamma_3 * I_ICU - psi * I_ICU
    dR = gamma_1 * I_Mild + gamma_2 * I_Hosp + gamma_3 * I_ICU
    dD = psi * I_ICU
    
    S_t = S + dS
    E_t = E + dE
    I_Mild_t = I_Mild + dI_Mild
    I_Hosp_t = I_Hosp + dI_Hosp
    I_ICU_t = I_ICU + dI_ICU
    R_t = R + dR
    D_t = D + dD
    
    
    if S_t < 0.0:
        S_t = 0.0
    if E_t < 0.0:
        E_t = 0.0
    if I_Mild_t < 0.0:
        I_Mild_t = 0.0
    if I_Hosp_t < 0.0:
        I_Hosp_t = 0.0
    if I_ICU_t < 0.0:
        I_ICU_t = 0.0
    if R_t < 0.0:
        R_t = 0.0
    if D_t < 0.0:
        D_t = 0.0

    scale = N / (S_t + E_t + I_Mild_t + I_Hosp_t + I_ICU_t + R_t + D_t)
    
    # order is improtant
    return S_t * scale, E_t * scale, I_Mild_t * scale, I_Hosp_t * scale, I_ICU_t * scale, R_t * scale, D_t * scale


# In[11]:


def dynamic_sim_seir_actnow(
    S0: float, E0: float, I0: float, R0: float, D0: float,
    alpha: float, 
    beta_2: float, beta_3: float, 
    gamma_1: float, gamma_2 : float, gamma_3 : float, 
    pop_1: float, pop_2: float, 
    psi: float, 
    i_day: int,
    *args ## (beta vector, n_days)
) -> Generator[Tuple[int, float, float, float, float, float, float, float], None, None]:
    """Simulate SEIR model forward in time yielding tuples.
    Parameter order has changed to allow multiple (beta, n_days)
    to reflect multiple changing social distancing policies.
    """
    
    S, E, I_Mild, I_Hosp, I_ICU, R, D = (float(v) for v in (S0, E0, I0 * (1-pop_1), 
                                                            I0 * pop_1 *(1-pop_2),
                                                            I0 * pop_1 *pop_2,
                                                            R0, D0))
    
    N = S + E + I_Mild + I_Hosp + I_ICU + R + D
    
    Days = i_day
    
    while args:
        beta_1, n_days, *args = args
        for idx in range(n_days):
            yield Days, S, E, I_Mild, I_Hosp, I_ICU, R, D
            
            S, E, I_Mild, I_Hosp, I_ICU, R, D = seir_actnow(S, E, I_Mild, I_Hosp, I_ICU, R, D,
                                                            beta_1[idx], beta_2, beta_3, alpha, gamma_1, 
                                                            gamma_2, gamma_3, pop_1, pop_2, psi)
            Days += 1
    
    yield Days, S, E, I_Mild, I_Hosp, I_ICU, R, D


# In[14]:


def dynamic_sim_seir_actnow_df(
    S0: float, E0: float, I0: float, R0: float, D0: float,
    alpha: float, 
    beta_2: float, beta_3: float, 
    gamma_1: float, gamma_2 : float, gamma_3 : float, 
    pop_1: float, pop_2: float, 
    psi: float, 
    i_day: int, 
    *args ## (beta vector, n_days)
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    
    """Simulate the SEIR model forward in time."""
    
    result_complete = pd.DataFrame(
        data = dynamic_sim_seir_actnow(S0, E0, I0, R0, D0, alpha,beta_2, beta_3, gamma_1, gamma_2, gamma_3,
                                    pop_1, pop_2, psi, i_day,*args),
        columns=("day", "susceptible", "exposed", "infected_mild", "infected_hospitalized","infected_ICU"
                 ,"recovered", "deceased")
    )
    
    result_simplify = result_complete
    result_simplify['infected'] = result_simplify['infected_mild'] + result_simplify['infected_hospitalized'] + result_simplify['infected_ICU']
    result_simplify = result_simplify[["day", "susceptible", "exposed", 'infected', 'infected_hospitalized', 'recovered']]
    
    return result_simplify, result_complete 


# In[76]:


simple_result, complete_result = dynamic_sim_seir_actnow_df(
    1e5, #initial S
    1e4, #initial E
    1e2, #initial I
    0, #initial R
    0, #initial Deceased
    1/3, # alpha, incubation period between exposed and infected, based on covid actnow
    5e-5, #beta2, based on github
    1e-6, #beta3, based on github
    1/6, # gamma1,1/6 based on covid actnow
    1/6, # gamma2,1/6 based on covid actnow
    1/8, # gamma3,1/8 based on covid actnow
    0.0727, # prop mild to hospitalized, based on covid actnow
    0.1397, # prop hospitalized to ICU, based on covid actnow
    0.5, # death rate, based on covid actnow
    0, # start_day 
    [1e-5]*50, #beta1, based on covid actnow
    50)


# In[77]:


simple_result[['exposed', 'infected', 'infected_hospitalized']].plot()

