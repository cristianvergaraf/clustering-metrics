import numpy as np
import pandas as pd


def outliers_iqr(df):
    # Calculate lower and upper bounds for each column
    lower_bounds = df.apply(lambda x: x.quantile(0.25) - 1.5 * (x.quantile(0.75) - x.quantile(0.25)))
    upper_bounds = df.apply(lambda x: x.quantile(0.75) + 1.5 * (x.quantile(0.75) - x.quantile(0.25)))

    # Define the compare function
    def compare(x, lower_range, upper_range):
        return np.where((x < lower_range) | (x > upper_range), 'atipico', 'tipico')

    # Apply the compare function to each column
    result = df.apply(lambda x: compare(x, lower_bounds[x.name], upper_bounds[x.name]))

    return result
