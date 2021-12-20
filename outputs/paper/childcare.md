
# structure

## Part 1:
State and prevalence of childcare and household work -> what is the current state and historical trend. Use time series. Who are these people. Show marginal distributions

## Part 2:
What is the variability? Cluster analysis via SA. Use clustering to show how participant use their time qualitatively and how their days are shaped -> individual sequences (medoids), turbulences. Show distributions of demographics by clustering. Show "what a day looks lke" at an individual level. How scattered is the day? Is it a confetti day? What is the level of turblence? Show turbulence distribution. Is there continuity in one's day/ Are these people the sandwich generation (caring for young and the old). Need to describe 2019 clearly which will let us clarify why cluster 2 is distinct from cluster 3 (who are the people that moved from cluster 1 in 2019 to cluster 2 in 2020 (this is the matching analysis section)). Show marginal distributions and cluster proportions of weighted population


# Intro

Women have historically performed more childcare and household work than men. This split has slowly equalized over the years but women still do most of the work as of 2019. 2020 has been a unique year where there is a still a gap but secondary child care has increased substantially -- most evidently for the work-at-home crowd.

We support this with two major analyses: a time-use perspective of longitudinal data of childcare and household work --> typically women have shouldered the burden but the gender gap for household work has been decreasing (average minutes has decreased). The gender gap trend for childcare is more nuanced: overall time spent on childcare is ...[TODO: need data on primary childcare split by gender].



# Data



# Research question

- RQ: Has the difference in prevalence of childcare and household work between males and females changed in 2020 compared to the historical trend?
- RQ: How has the variability in (secondary) childcare changed in 2020 compared to previous years?  


# State and prevalence of childcare and household work

## Childcare and household work
Zoom into 2020. Show split within 2019 vs 2020?

[TODO: this split by gender]
![primary childcare](/analyses/supporting/plots/childcare_primary_timeseries.png)

![primary childcare by sex](/analyses/supporting/plots/childcare_primary_timeseries_by_sex.png)


Show marginal distributions of childcare and household work by sex and partner status
![primary childcare marginals](/analyses/supporting/plots/childcare_primary_marginals.png#)

![secondary childcare marginals](/analyses/supporting/plots/childcare_secondary_marginals.png)


# Secondary childcare

Explain secondary childcare_primary_timeseries --> why its important, how its nuanced from primary childcare, set up covid examples (NYT) and how it relates to WFH lifestyle

![secondary childcare](/analyses/supporting/plots/childcare_secondary_timeseries.png)

[TODO: add missing data line]
![secondary childcare by gender](/analyses/supporting/plots/childcare_secondary_timeseries_by_sex.png)

[TODO: make comprehensible]
![household work](/analyses/supporting/plots/housework_by_age_sex.png)

Segue to sequence analysis.


# Sequence analysis and clustering

SA can answer the variability question

![clustering default](/outputs/plots/seqi_grey_1920_resampled.png)

![clustering default](/outputs/plots/seqi_grey_1920_resampled_by_sex.png)
Repeat above plot for 2020 but split by gender
Repeat again but break out other activities


#


# Appendix


![Employment gap during 2020](/analyses/supporting/plots/labor_force_participation.png)


## Marc
Overall we are missing some characterization of the sequences. We have not looked at what examinig individual sequences/tranistions. Should do more work on SeqI and turbulence to lay to rest qualitatively what happened in cluster 2
- could do difference in turbulence between matched pairs
  - e.g. calculate the distribution of differences in turbulence between matched pairs
  - could group by cluster in time 1
  - then do some sort of independent samples t-test
- could look at number of spells
- could characterize the sequences using traminer tool
