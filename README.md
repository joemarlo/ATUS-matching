# Clustering and matching sequence data

### Folder structure
    .
    ├── analyses          # Clustering, matching, demographic vars, etc.
    │   └── plots         # Plots
    ├── data              # Cleaned data and cleaning scripts
    ├── inputs            # Raw input data
    ├── outputs           # Formal write-ups
    └── README.md


### Current matching results

<br>
<p align="center">
<img src="analyses/plots/perfect_matches_mahalanobis.png" width=80%>
</p>

<br>

### Reproducibility
To reproduce, run the scripts in the following order:  
1. Download 2003-2018 multi-year microdata from the [BLS](https://www.bls.gov/tus/#data) and save to `inputs/ATUS-2003-2018/`    
2. Clean and consolidate the data:  
    1. `data/cleaning_atus30.R`  
    2. `data/cleaning_demographics.R`  
    3. `data/essential_industries.R`  
3. `analyses/matching/matching_mahalanobis.R`  
4. `analyses/clustering.R`  

<br>

See also: [ATUS repo](https://github.com/joemarlo/ATUS)
