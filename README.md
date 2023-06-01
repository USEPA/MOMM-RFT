# MOMM-RFT

This repository contains the source code for the Methane-Ozone Mortality Model - Reduced Form Tool (MOMM-RFT), described in <i>McDuffie et al., 2023, in preparation</i>

<strong>Corresponding Author</strong>: Erin McDuffie (mcduffie.erin.e@epa.gov)

<strong>Plain Language Summary:</strong>
Using an approach consistent with the social cost of methane framework, we estimate that damages from the methane produced ozone are $1700/metric ton, which, if included, would double the current social cost of methane. These costs have uncertainties related to the health risks associated with exposure to ozone, assumptions about future NOx emissions, choice of discount rates, and other factors. We also develop a reduced form model that allows rapid estimation of many of these sensitivities and enables consideration of this mechanism in the social cost methodology. 

----------------
## About
--
The MOMM-RFT is designed to quantify the global annual respiratory-related mortality that is attributable to tropospheric ozone, chemically produced from a marginal pulse of methane emissions. This tool is based on a series of global ozone simulations from the <a href="https://www.ccacoalition.org/en/resources/global-methane-assessment-full-report/">UNEP/CCAC 2021 Global Methane Assessment</a> , socioeconomic and mortality data from <a href="https://zenodo.org/record/5898729/">Resources for the Future (RFF)</a> and the <a href="https://korbel.du.edu/pardee/international-futures-platform/">International Futures Project Platform (IFP)</a>, and mortality estimates from a global instance of EPA's  <a href="https://www.epa.gov/benmap/">BenMAP webtool</a>. 

As described in <i>McDuffie et al.,</i> the user first supplies the pulse size and year of methane emissions, as well as country-level NO<sub>x</sub> emissions (as a timeseries or 'present day' scalar). The code can then be run for a specific RFF-SP scenario or for the mean scenario across all RFF-SP projections.  Then run, the code a) calculates the timeseries of perturbed methane concentrations between the emissions pulse year and 2100, and b) calculates the timeseries of respiratory-related mortality counts from associated changes in tropospheric ozone, by taking the ratio between the calculated ozone/methane response, population, background respiratory-mortality counts, and perturbed methane concentrations in the user-specified run relative to those run in the central BenMAP simulation. The calculation is done at the country-level for 201 countries. 

This repository contains the following:<br>
  <ul>- /analysis/Manuscript/* - Data, scripts, and figure files to support <i>McDuffie et al., 2023, in preparation</i><br>
  - /code/* - code for the MOMM reduced form tool (including pre-processing & valuation scripts)<br>
  - /input/* - input data for the MOMM-RFT, including BenMAP mortality results, and RFF-SP scenarios<br>
  - /output/* - output from the MOMM-RFT and valuation script<br></ul>

## License
 --
The software code contained within this repository is made available under the <a href="https://opensource.org/license/mit/">MIT license</a>. The data and figures are made available under the <a href ="https://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 license</a>.
