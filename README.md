# Clustering and matching sequence data

Reproducible code for working paper on matching clustered sequence data and understanding secondary childcare via time use data.

<br>
<p align="center">
<img src="outputs/SA/seqd_2020_detail.png" width=80%>
</p>

<br>
<p align="center">
<img src="outputs/SA/seqi_2020_detailed.png" width=80%>
</p>


### Folder structure
    .
    ├── analysis/		# Analysis scripts
    ├── data/		# Cleaned data
    ├── inputs/		# Raw input data
    ├── outputs/		# Formal write-ups and figures
    ├── R/			# Functions for cleaning and analysis
    ├── renv/         	# Storage for renv
    ├── _targets/     	# Storage for targets
    ├── tests/		# Unit tests
    ├── _targets.R      	# targets pipeline
    ├── run.R           	# Run the targets pipeline from R
    ├── run.sh          	# Run the targets pipeline from shell
    └── README.md

<br>

### Reproducibility

To reproduce:  
1. Open `ATUS-matching.Rproj`. Install `renv` via `install.packages('renv')` and then run `renv::restore()`
2. Download, clean, and consolidate the data:  `run.R`. Note: this downloads a few data files including the 2003-2021 multi-year microdata from the [BLS](https://www.bls.gov/tus/datafiles-0321.htm) (~2GB) and saves them `inputs/ATUS-2003-2021/`   
3. Run the analyses  
    1. `analysis/timeseries.R`  
    2. `analysis/matching.R`  
    3. `analysis/sequence_analysis.R`  

<br>

See also: [ATUS repo](https://github.com/joemarlo/ATUS)
