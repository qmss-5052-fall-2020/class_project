import pandas as pd
import numpy as np

def gen_Metrics(data, var_x, var_true):
    '''
    provide SSE, SAE, SSPE, SAPE
    '''
    
    data = data.copy()
    data['SSE'] = data[var_x] - data[var_true]
    data['SAE'] = data[var_x] - data[var_true]
    # if true is zero, ignore it 
    data['SSPE'] = np.where(data[var_true] <= 0,
                            0,
                            (data[var_x] - data[var_true])/data[var_true])
    data['SAPE'] = np.where(data[var_true] <= 0,
                            0,
                            (data[var_x] - data[var_true])/data[var_true])

    def Sum_Square_Diff(x):
        return sum(x**2)

    def Sum_Absolute_Diff(x):
        return sum(abs(x))
    
    result = data.aggregate({
        'SSE':Sum_Square_Diff,
        'SAE':Sum_Absolute_Diff,
        'SSPE':Sum_Square_Diff,
        'SAPE':Sum_Absolute_Diff
    })
    
    result = pd.DataFrame(result).transpose()
    
    return(result)



