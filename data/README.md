# Data

Input data files are **not distributed** with this repository (third-party
sources and license terms). The analysis scripts expect the following files
in this folder:

| File | Used by | Source |
|---|---|---|
| `HDI2022.txt` | analysis/geography-of-hdi (01, 03), sandbox | UNDP HDR, tab-separated extract with columns Code, Country, Level, Region, SubRegion, DevRegion, LE, EYS, MYS, GNIpC |
| `HDI2023.txt` | analysis/geography-of-hdi (02–05), analysis 03 | Same structure, 2023 release |
| `HDR23-24_Composite_indices_complete_time_series.csv` | analysis/30-years-of-hdi/01 | UNDP (https://hdr.undp.org/data-center) |
| `hdi_noNANum.csv` | analysis/30-years-of-hdi/02 | Derived from the file above (complete cases, numeric columns) |
| `Data_Fattore_PoseticToolsTutorial.txt` | tutorials/fattore_posetic_tools.R | Fattore's PoseticTools tutorial |
| `LNOB_MICS.txt` | (former Poset_101_Rallou.R example) | MICS-based LNOB indicators |
| `DATA POSET ENVIRONMENT TEXT 9 JULY V2.txt` | sandbox/proves_poset_som.R | Internal working file |
