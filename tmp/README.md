# Clustering and matching sequence data

Reproducible code for working paper on matching clustered sequence data and understanding secondary childcare via time use data.

<br>
<p align="center">
<img src="outputs/plots/seqi_grey_1920.png" width=80%>
</p>


### Folder structure
    .
    ├── data/			# Cleaned data
    ├── inputs/		# Raw input data
    ├── outputs/		# Formal write-ups and figures
    ├── R/			# Functions for cleaning and analysis
    ├── tests/			# Unit tests
    ├── meta_data.R		# Data cleaning 
    ├── meta_matching.R	# Matching analysis
    ├── meta_SA.R		# Sequence analysis
    ├── meta_timeseries.R	# Time series analysis
    └── README.md

<br>

### Reproducibility
To reproduce, run the scripts in the following order:  
1. Download 2003-2020 multi-year microdata from the [BLS](https://www.bls.gov/tus/#data) and save to `inputs/ATUS-2003-2020/`    
2. Clean and consolidate the data:  `meta_data.R`  
3. Run the analyses  
    1. `meta_timeseries.R`  
    2. `meta_matching.R`  
    3. `meta_SA.R`  

<br>

See also: [ATUS repo](https://github.com/joemarlo/ATUS)
