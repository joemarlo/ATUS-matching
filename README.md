# Clustering and matching sequence data

Reproducible code for working paper on matching clustered sequence data and understanding secondary childcare via time use data.

<br>
<p align="center">
<img src="outputs/plots/seqi_grey_1920.png" width=80%>
</p>


### Folder structure
    .
    ├── analyses          # Clustering, matching, demographic vars, etc.
    │   ├── backtest      # 2003-2018 backtest scripts and results
    │   ├── data          # Output data from one run
    │   ├── matching      # Scripts to perform matching
    │   ├── plots         # Plots from one run
    │   └── supporting    # Supporting analyses
    ├── data              # Cleaned data and cleaning scripts
    ├── inputs            # Raw input data
    ├── outputs           # Formal write-ups and final clusters
    ├── tests             # Unit tests
    └── README.md

<br>

### Reproducibility
To reproduce, run the scripts in the following order:  
1. Download 2003-2020 multi-year microdata from the [BLS](https://www.bls.gov/tus/#data) and save to `inputs/ATUS-2003-2020/`    
2. Clean and consolidate the data:  
    1. `data/cleaning_atus30.R`  
    2. `data/cleaning_demographics.R`  
    3. `data/essential_industries.R`  
3.  Run a single year or full back test:  
    - Single year:  
        1. `analyses/matching/matching_mahalanobis.R`  
        2. `analyses/clustering.R`  
    - Backtest:  
        1. `analyses/backtest/batch_backtest.R`  

<br>

See also: [ATUS repo](https://github.com/joemarlo/ATUS)
