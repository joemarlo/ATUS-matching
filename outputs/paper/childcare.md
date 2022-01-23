
# structure

## Part 1:
State and prevalence of childcare and household work -> what is the current state and historical trend. Use time series. Who are these people. Show marginal distributions

## Part 2:
What is the variability? Cluster analysis via SA. Use clustering to show how participant use their time qualitatively and how their days are shaped -> individual sequences (medoids), turbulences. Show distributions of demographics by clustering. Show "what a day looks like" at an individual level. How scattered is the day? Is it a confetti day? What is the level of turbulence? Show turbulence distribution. Is there continuity in one's day/ Are these people the sandwich generation (caring for young and the old). Need to describe 2019 clearly which will let us clarify why cluster 2 is distinct from cluster 3 (who are the people that moved from cluster 1 in 2019 to cluster 2 in 2020 (this is the matching analysis section)). Show marginal distributions and cluster proportions of weighted population


# Intro

Women have historically performed more childcare and household work than men. This split has slowly equalized over the years but women still do most of the work as of 2019. 2020 has been a unique year where there is a still a gap but secondary child care has increased substantially -- most evidently for the work-at-home crowd.

We support this with two major analyses: a time-use perspective of longitudinal data of childcare and household work --> typically women have shouldered the burden but the gender gap for household work has been decreasing (average minutes has decreased). The gender gap trend for childcare is more nuanced: overall time spent on childcare is ...[TODO: need data on primary childcare split by gender].



# Data



# Research question

- RQ: Has the difference in prevalence of childcare and household work between males and females changed in 2020 compared to the historical trend?
- RQ: How has the variability in (secondary) childcare changed in 2020 compared to previous years?  


# State and prevalence of childcare and household work

- Introduce definitions of primary childcare, household work, and secondary childcare. Why secondary childcare matters in the context of 2020

## Household work

Plot takeaways:
- There exists a meaningful difference in the mean time spent per day on household activities between females and males.
- This difference has been changing over the last 15 years and is moderated by average
- Females are generally doing less household work measured by absolute minutes however the daily participation rate remains steady
- Younger males have increased their participation rate

<!--![household work](/analyses/supporting/plots/housework_by_age_sex.png)-->
![household work](/analyses/supporting/plots/housework_by_age_sex_split.png#)

## Primary childcare
Zoom into 2020. Show split within 2019 vs 2020?

Plot takeaways:
- Mean daily time spent on childcare (as a primary activity) has not meaningful increased over the last 16 years
- Within the covid era (2020 3Q and 4Q), primary childcare has not seen an increase
- There is a split between females and males. Females, on average, spend (approx.) 2 hours daily on primary childcare while males spend (approx.) half.
- Only includes households with children under the age of 13

[TODO: this split by gender]
![primary childcare](/analyses/supporting/plots/childcare_primary_timeseries.png)


![primary childcare by sex](/analyses/supporting/plots/childcare_primary_timeseries_by_sex.png)


Plot takeaways:
- Time spent on primary childcare varies principally with variables that indicate support mechanisms: elder in household, presence of a partner, partner working status.
- There are large differences in time by labor force status (employed at work vs not in labor force)

![primary childcare marginals](/analyses/supporting/plots/childcare_primary_marginals.png#)


## Secondary childcare
Explain how primary childcare doesn't capture the whole picture. Why secondary childcare provides another lens.

Set up covid examples (NYT) and how it relates to work-from-home lifestyle.

Plot takeaways:
- Mean daily time spent on secondary childcare is greater than primary childcare (~5 hours daily vs ~1.3 hours) for the same households.
- Mean daily time spent on secondary childcare was at an all-time high for 2020 3Q and stayed elevated in 2020 4Q
- The gap between females and males within primary vs secondary childcare is greater on absolute basis (~1 hour vs ~2 hours) but less relatively (~50% less vs ~33% less)

![secondary childcare](/analyses/supporting/plots/childcare_secondary_timeseries.png)

![secondary childcare by gender](/analyses/supporting/plots/childcare_secondary_timeseries_by_sex.png)

Plot takeaways:
- Secondary childcare is not as strongly moderated as primary childcare except for elder in household which shows a greater difference

![secondary childcare marginals](/analyses/supporting/plots/childcare_secondary_marginals.png)



Segue to sequence analysis.


# Sequence analysis and clustering

SA can answer the variability question

Plot takeaways:
- Sequence analysis clustering results in three distinct clusters. These clusters are defined by workers not participating in secondary childcare (Cluster one), a mixture of activities with a strong contingent of workers providing secondary childcare (cluster 2), and individuals participating mostly in leisure activities (cluster 3)
- Compared to 2019, there has been an increase in the prevalence of workers providing secondary childcare (red, cluster 2 2020 vs cluster 2 2019)
- Discuss sex differences. I don't think there is a strong difference but waiting on final plot

![clustering default colored](/outputs/final_clusters/default/2019_2020/plots/clustering/seqD_time2.png)

![clustering default grey](/outputs/plots/seqi_grey_1920_resampled.png)

![clustering default](/outputs/plots/seqi_grey_1920_resampled_by_sex.png)
Repeat above plot for 2020 but split by gender
Repeat again but break out other activities




# Appendix

![elbow plots](/outputs/final_clusters/default/2019_2020/plots/clustering/cluster_validity.png)

![Employment gap during 2020](/analyses/supporting/plots/labor_force_participation.png)


## Marc
Overall we are missing some characterization of the sequences. We have not looked at what examinig individual sequences/tranistions. Should do more work on SeqI and turbulence to lay to rest qualitatively what happened in cluster 2
- could do difference in turbulence between matched pairs
  - e.g. calculate the distribution of differences in turbulence between matched pairs
  - could group by cluster in time 1
  - then do some sort of independent samples t-test
- could look at number of spells
- could characterize the sequences using traminer tool


# TODO
- look at cluster 2 women vs cluster 2 male and compare their daily minutes spent on SSC (anova or ttest)
- revamp cluster plot to show white space between male/females
- need proportion of males in cluster 2 2019 vs 2020
