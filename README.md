# Unsupervised learning and matching 

Data available for download [here](https://www.dropbox.com/sh/hqx7dlh3cc6843i/AABXhrbnFJFgEnTN-6S-60Rda?dl=0).

See also: [ATUS repo](https://github.com/joemarlo/ATUS)

<!--
<br>
<br>
<p align="center">
<img src="Plots/mean_alone_time.png" width=79%>
</p>
-->

### Folder structure
    .
    ├── analyses          # Clustering, propensity matching, etc.
    │   └── plots         # Plots
    ├── data              # Cleaned data and cleaning scripts
    ├── inputs            # Raw input data
    └── README.md

### Outstanding questions / notes
- Dates for two groups: 
    - Financial crisis occurred over many months. Does it make more sense to compare 2007 to 2009?
    - Look at dates that participants were surveyed
- Do we need full matching? Or just matching for all of 2007 or all of 2009
- Should we also try IPTW matching?