from shapely.geometry import box
import geopandas as gpd
import os
import numpy as np
from glob import glob
import xarray as xr
import cdo


# Use Climate Data Operators to Calculate Area
cdoo = cdo.Cdo('/work/ROMO/anaconda_envs/cdo-1.9.8/bin/cdo')

# Choose a file at random to represent grid structure and cell area
paths = sorted(glob('/work/MOD3DEV/kseltzer/GMA/metrics/*/O3_metrics_*_Sim?_05x05.nc'))
gf = xr.open_dataset(paths[0])
areaf = cdoo.gridarea(input=paths[0], returnCdf=True)
AREA = areaf.variables['cell_area'][:]

# Create a vector of boxes and areas
boxes = []
areas = []
colvals = []
rowvals = []
i = 0
n = AREA.size
print('Building shapes')
# Iterating column on the inner dimension. This seems consistent with received shapefile
# I hope it doesn't really matter, but I don't know.
for ri, lat in enumerate(gf.latitude):
    for ci, lon in enumerate(gf.longitude):
        i += 1
        print(f'\r{i/n:.1%}', end='')
        mybox = box(lon - 0.25, lat - 0.25, lon + 0.25, lat + 0.25)
        boxes.append(mybox)
        areas.append(AREA[ri, ci])
        # using 1-base to be consistent with previous files
        colvals.append(ci + 1)
        # using 1-base to be consistent with previous files
        rowvals.append(ri + 1)

print('\nCreating vector object')
gdf = gpd.GeoDataFrame(
    dict(AREA=areas, ROW=rowvals, COL=colvals),
    geometry=np.array(boxes),
    crs=4326
)

print('Saving file')
gdf.to_file("shp/GMA_05x05.shp")