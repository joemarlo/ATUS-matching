This batch was run at 2021-09-10 14:35:02 with the following specifications:  

Years: 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019  
Time 1 to time 2 lag: 1  
k values: float across t1 and t2  
k search range: 3, 4, 5, 6, 7, 8, 9, 10  
Distance method: TRATE  
Clustering algorithm: hclust, ward.D2  
Matching method:  
	-Mahalanobis distance on age, sex, race, fam_income, has_partner, education, child_in_HH, n_child, age_youngest, region, partner_working, elder_in_HH, metropolitan, labor_force_status  
	-Stratified on sex, race, +/- 2 age, labor_force_status  
	-Matching performed from time1 to time2 (many-to-1)  
Activities are segmented to include/not include secondary child care
Only months May+ (inclusive) are included