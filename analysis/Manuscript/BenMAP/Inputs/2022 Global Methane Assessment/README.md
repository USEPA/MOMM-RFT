Global Methane Assessment BenMap
================================

Overview
--------

This folder contains files that should work for BenMap that were created from
six simulations from the Global Methane Assessment Report.[1] To summarize,
these simulations represent variations on CO/NOx/VOC emissions and methane
concentrations for multiple meteorological years.


| Sim | CO/NOx/VOC            | Methane  |
|-----|-----------------------|----------|
| 1   | 2015                  | 1834 ppb |
| 2   | 2015                  | 1278 ppb |
| 3   | SSP3-7.0 2050         | 1834 ppb |
| 4   | SSP3-7.0 2050         | 1278 ppb |
| 5   | SSP3-7.0_lowNTCF 2050 | 1834 ppb |
| 6   | SSP3-7.0_lowNTCF 2050 | 1278 ppb |


The original ozone concentration output files were obtained by Karl Seltzer,
who transformed them into a common 0.5 degree by 0.5 degree grid and created
6-month averages of the max daily 8-hour average for the northern hemisphere
(Apr-Sep) and southern hemisphere (Oct-Mar).

For our purposes, the data from northern and southern hemispheres were merged
to create a "SUMMER" average where south of 0N, the values are from `MDA8_SH`
and otherwise from `MDA8_NH`. The average of both periods was saved as ANNUAL.
These results were output as csv files that conform to the BenMAP expected
format.

In addition, a shapefile was created in WGS84 (epsg:4326). The shapefile has
one polygon per cell that contains a ROW and COL attribute as well as AREA
calculated using the Climate Data Operators (cdo) gridarea operator.

The final results have been put in a zip file `GMA_for_BenMap.zip` The zipfile
has contents that mirror the directory structure of this folder. The contents
are listed below.

Annotated Directory Structure
-----------------------------

In the listing below, hashes indicate notes and the placehodlers <Model> and
<Sim> are replaced by specific values. <Model> is each of CESM2, GISS, GFDL,
HadGEM, or MIROC and <Sim> is each of Sim1, Sim2, Sim3, Sim4, Sim5, and Sim6.

```
.
|-- csv
|   |-- O3_metrics_<Model>_<Sim>_05x05_ANNUAL_MDA8.csv
|   |   # Average of MDA8_NH and MDA8_SH
|   `-- O3_metrics_<Model>_<Sim>_05x05_SUMMER_MDA8.csv
|        # MDA8_SH south of 0, and MDA8_NH otherwise
|-- shp
|   |   # Shapefile describing domain.
|   |-- GMA_05x05.cpg
|   |-- GMA_05x05.dbf
|   |-- GMA_05x05.prj
|   |-- GMA_05x05.shp
|   `-- GMA_05x05.shx
|-- tobenmap.py
|    # Script to make BenMap CSV files from Karl’s files.
|    # Any review would be welcome
|-- makeshape.py
|    # Script to Make Shape file
|    # Any review would be welcome
`-- QA.ipynb
      # Notebook to view and QA results
```

References
----------
    
Harmsen, M., Kriegler, E., van Vuuren, D. P., van der Wijst, K.-I., Luderer, G., Cui, R., Dessens, O., Drouet, L., Emmerling, J., Morris, J. F., Fosse, F., Fragkiadakis, D., Fragkiadakis, K., Fragkos, P., Fricko, O., Fujimori, S., Gernaat, D., Guivarch, C., Iyer, G., Karkatsoulis, P., Keppo, I., Keramidas, K., Köberle, A., Kolp, P., Krey, V., Krüger, C., Leblanc, F., Mittal, S., Paltsev, S., Rochedo, P., van Ruijven, B. J., Sands, R. D., Sano, F., Strefler, J., Arroyo, E. V., Wada, K., and Zakeri, B.: Integrated assessment model diagnostics: key indicators and model evolution, Environ. Res. Lett., 16, 054046, https://doi.org/10.1088/1748-9326/abf964, 2021.

