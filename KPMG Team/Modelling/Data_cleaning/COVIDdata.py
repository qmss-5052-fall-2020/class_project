# -*- coding: utf-8 -*-
"""
Created on Thu Nov 12 14:32:12 2020

@author: bolim
"""

class COVIDdata:
    
    import pandas as pd
    import numpy as np
    import re
    import os
    import datetime as dt
    
    def __init__(self, us_cases_df, us_deaths_df, pop_raw, oxford_raw):
        # initialize to get raw data files
        us_cases_df = self.pd.read_csv('url')
        us_deaths_df = self.pd.read_csv('url')
        
    
    def us_shape_clean(self, df):
        """
        Data cleaning for US covid, per STATE.
        If by county level, add 'FIPS' on pd.melt(id_vars), pd.pivot(index)
        """
        df = pd.melt(df, id_vars=['Province_State', 'indicator'], var_name='Date', value_name='Value')
        #df.head()
        df['Date'] = df['Date'].apply(lambda x: dt.datetime.strptime(str(x), '%m/%d/%y'))
        df = pd.pivot_table(df, index = ['Province_State', 'Date'], columns='indicator', values = 'Value', aggfunc=np.sum).reset_index()
        # Remove non-states
        not_state = ['American Samoa', 'Diamond Princess', 'Grand Princess', 'Guam', 'Northern Mariana Islands', 'Puerto Rico', 'Virgin Islands']
        df = df[~df['Province_State'].isin(not_state)]
        df = df.sort_values(by=['Date', 'Province_State'])

        return df
    
    def calculate_daily(self, df, state):
        df = df.sort_values(by=['Date'])
        confirmed = df[df['Province_State'] == state]['Confirmed'].values.tolist()
        deaths = df[df['Province_State'] == state]['Deaths'].values.tolist()
        tmp_df = df[df['Province_State'] == state]
        new_confirmed = [confirmed[0]]
        new_deaths = [deaths[0]]
        for i in range(len(tmp_df)-1):
            new_confirmed.append(confirmed[i+1]-confirmed[i])
            new_deaths.append(deaths[i+1]-deaths[i])
        tmp_df['new_confirmed'] = new_confirmed
        tmp_df['new_deaths'] = new_deaths

        return tmp_df # returns subsetted df with daily new confirmed and daily new deaths