import os
import numpy as np
from glob import glob
import xarray as xr

paths = sorted(glob('/work/MOD3DEV/kseltzer/GMA/metrics/*/O3_metrics_*_Sim?_05x05.nc'))
outkeys = ['SUMMER_MDA8', 'ANNUAL_MDA8']

for path in paths:
    bpath = os.path.basename(path)[:-3]
    print(bpath, end=':')
    f = xr.open_dataset(path)
    # Create place holders for outputs
    f['SUMMER_MDA8'] = f['MDA8_NH'].copy() * 0
    f['ANNUAL_MDA8'] = f['MDA8_NH'].copy() * 0
    
    # Overwrite SUMMER with only NH (where lat >= 0) or SH (where lat < 0)
    f['SUMMER_MDA8'][:] = xr.concat(
        [
            f['MDA8_NH'].where(f['latitude'] >= 0),
            f['MDA8_SH'].where(f['latitude'] < 0)
        ], dim='season'
    ).mean('season')
    # Overwrite ANNUAL with the average of both seasons
    f['ANNUAL_MDA8'][:] = xr.concat(
        [f['MDA8_NH'], f['MDA8_SH']],
        dim='season'
    ).mean('season')
    # Create a subset file
    outf = f[outkeys]
    nc = f.dims['longitude']
    nr = f.dims['latitude']
    # using 1-base to be consistent with previous files
    col = np.arange(1, nc + 1)
    row = np.arange(1, nr + 1)
    COL, ROW = np.meshgrid(col, row)
    outf['Col'] = xr.DataArray(COL, dims=('latitude', 'longitude'))
    outf['Row'] = xr.DataArray(ROW, dims=('latitude', 'longitude'))

    # Column is iterating first, then Row
    # This is just for consistency with previous files. I don't think it matters
    df = outf.transpose('longitude', 'latitude').to_dataframe()
    
    df['Metric'] = 'D8HOURMAX'
    df['Seasonal Metric'] = ''
    df['Statistic'] = ''
    for outkey in outkeys:
        print(outkey, end='.')
        csvpath = os.path.join('csv', bpath + f'_{outkey}.csv')
        # For each key, rename it as values and output it
        df.rename(
            columns={outkey: 'Values'}
        ).loc[
            :, ['Col', 'Row', 'Metric', 'Seasonal Metric', 'Statistic', 'Values']
        ].to_csv(
            csvpath, index=False
        )
    print()