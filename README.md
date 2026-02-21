<table>
  <tr>
    <td width="150">
      <img src="icon.ico.png" width="130">
    </td>
    <td>
      <h1>Multi-Attribute Catchment Hydrometeorological dataset</h1>
        <a href="https://doi.org/10.5281/zenodo.18686475">
          <img src="https://zenodo.org/badge/DOI/10.5281/zenodo.18686475.svg">
        </a>
    </td>
  </tr>
</table>

## Overview
**MACH** is large-sample hydrometeorological dataset spanning **1,014 watersheds** across the United States. It integrates daily meteorological forcings, streamflow observations, and a comprehensive suite of watershed attributes into a unified basin-scale resource. 
   
The dataset covers 1980-2023:
- 1980-2023 (44 years) for all 1,014 catchments
- 1948-2023 (75 years) for 395 basins

MACH is designed to support reproducible, basin-scale hydrologic analysis across diverse climatic and physiographic regimes.   

## Scope and Contents
MACH includes:
- Daily meteorological forcings
- Daily streamflow observations
- Static watershed attributes
   - Topography
   - Soils
   - Geology
   - Hydrology
   - Land cover
   - Anthropogenic influences
- Hydroclimatic indices and derived metrics 
  
### Designed to support
- Climate and runoff analyses
- Hydrologic model calibration and benchmarking
- Sensitivity and attribution studies
- Long-term watershed change assessment
- Large-sample hydrology research


## Basin framework
<p>MACH adopts basins identifiers consistent with the MOPEX and CAMELS frameworks to facilitate cross-dataset comparison and interoperability.</p>
<p>However, all meteorological forcings, streamflow records, watershed attributes, and derived indices have been <strong>newly processed, harmonized, and validated</strong> for this release. All variables are aggregated and standardized at the basin scale to ensure spatial consistency across catchments.</p>

## Primary data sources
MACH integrates information derived from:
- National meteorological forcing datasets including <a href="https://daymet.ornl.gov/">Daymet Version 4</a> and <a href="https://www.gleam.eu/">Gleam 4.2</a>
- Streamflow observations from the <a href="https://waterdata.usgs.gov/">USGS National Water Information System</a>
- Attributes from the <a href="https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data">NHDPlusV2</a>, <a href="https://www.nrcs.usda.gov/resources/data-and-reports/soil-survey-geographic-database-ssurgo">SSURGO</a>,  <a href="https://www.mrlc.gov/">MRLC</a>, <a href="https://nid.sec.usace.army.mil/nid/#/">NID</a>, and <a href="https://www.earthdata.nasa.gov/data/catalog/ornl-cloud-global-veg-greenness-gimms-3g-2187-1">GIMMS-3G+</a> 

## Data Access
:point_right: The MACH dataset is publicly archived on [Zenodo](https://zenodo.org/records/18686475). 
Each release is versioned and permanently archived. 
Please use the DOI corresponding to the version used in your analysis. 

Version 4.0 release date 2/19/2026
Future updates will include extended time series. 

## Feedback and issue reporting
If you encounter inconsistencies or have suggestions for improvement, please open an issue in this repository:
https://github.com/k-sink/MACH/issues
Constructive feedback, feature requests, and reproducibility reports are welcome. 

## About this repository
This repository contains:
- Documentation for the MACH dataset
- Data processing workflows and scripts
- Release notes and version history
- Issue tracking and community feedback
The full dataset is archived on Zenodo. 

## Contact information
Please contact Katharine Sink at katharine.sink@utdallas.edu with any questions or feedback regarding MACH. 
