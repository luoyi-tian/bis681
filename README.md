# BIS 681 Capstone Project


## Background

Connecticut (population approximately 3.565 million) was among the US states most severely impacted by the first wave of COVID-19 epidemic. On March 8th, 2020, the first Connecticut COVID-19 case was reported, followed by a rapid increase in case counts. In the first three weeks of the epidemic, the state reported over 2500 confirmed COVID-19 cases. On March 17, Governor Ned Lamont ordered all in-person classes at K-12 schools canceled, and later extended the closure for the remainder of the 2019–2020 academic year. The Governor issued a statewide “Stay Safe, Stay Home” order to take effect on March 23. The order called on all nonessential businesses to cease in-person operations. 

The number of hospitalized COVID-19 patients in Connecticut peaked on April 21, 2020 and began a slow decline. In early May, Governor Lamont issued plans and guidance for reopening, a process set to begin with “Phase 1” on May 20 when some businesses, mostly those operating outdoors, were allowed to reopen at 50% capacity.  Phase 2 of reopening began on June 17th when indoor dining, libraries and religious services were allowed to reopen at reduced capacity. 

For most of summer 2020, case counts in Connecticut remained low, even while large outbreaks were happening in many parts of the US.  In-person, remote, and hybrid education at all levels resumed in Connecticut in August and early September 2020. Case counts and hospitalizations during fall 2020 increased slowly, leading the Governor to implement Phase 3 reopening, permitting indoor businesses to operate at higher capacity, on October 8. By early November, public health officials recognized a broad statewide epidemic resurgence. In response to rising case counts and fears of a substantial second wave, on November 6 Governor Lamont reverted to "Phase 2.1", reducing permitted occupancy of indoor businesses and events. In-person education at most public schools and all universities ended in mid-November before the Thanksgiving recess. Asymptomatic testing programs implemented by many universities were subsequently scaled down, resulting in a reduction in testing rates in Connecticut – making it difficult to interpret changes in the test positive proportion. Case counts in the second wave of the epidemic peaked in mid-December, plateaued for about a month and began a slow decline starting the second half of January 2021. 

Vaccine deployment in Connecticut began on December 14th, 2020 with residents of congregate settings and some essential workers being vaccinated first.  In the first year of the COVID-19 vaccine rollout, the vaccine coverage in CT was among the highest in the United States. The vaccination rollout proceeded by age group. Daily doses followed cyclical day-of-week patterns with lower counts on weekends. As each age group became eligible for vaccination, vaccine uptake followed a characteristic pattern in which it rose steeply at first and then plateaued after several weeks of eligibility. Coverage increased most rapidly in the spring of 2021, when daily doses were at a peak. The highest levels of cumulative coverage were achieved in older age groups.  The vaccinated population was not entirely representative of the state’s overall population. Early in the vaccine rollout, there were large disparities between coverage in urban and non-urban parts of the state. As the rollout continued, the geographic differences in coverage have decreased.


## Data

A data file `covid19_ct.csv` in this repository contains the data you'll need for this project. 


### COVID-19 cases

`The `cases` column shows the daily number of reported COVID-19 cases in Connecticut. A "case" is a positive test for a person whose identity is known, and who has not recently been counted as a case. 

It is important to understand that that COVID-19 diagnostic tests were scarce during the Spring and Summer of 2020. During the initial surge in cases, in March-June 2020, there were few available tests and the case counts for this period are likely artificially low. 

It is also important to understand that testing in Connecticut dramatically increased during the Fall 2020 semester, during which time the state required most colleges and universities to test students at least once per week. The number of tests increased dramatically, and the number of cases detected rose steadily. Most of those testing positive were relatively young and healthy. 

![](img/cases.png)


### COVID-19 Deaths

The `deaths` column shows counts of deaths for which COVID-19 is listed on the death certificate. While some early deaths may have been difficult to confirm due to scarcity of diagnostic tests, death counts are likely to be much more reliable than case counts throughout the pandemic. 

![](img/deaths.png)

### Vaccination

The `vaccinated` column shows the percent of the CT population that has received at least one vaccination dose. The state did not properly track early vaccinations (which took place starting in December of 2020), so the estimates begin at around 20%. Furthermore the vaccination percentage is not monotonic and climbs to more than 100%, presumably because the number vaccinated exceeds the population of the state estimated in the last U.S. Census.  

![](img/vaccinated.png)


## Instructions

Your overall task is to explain what happened in Connecticut during the first 2 years of the COVID-19 pandemic. 
- Did the actions of the Governor, including the "stay-at-home" order, school closures, and reopening, affect cases and deaths? 
- How were interventions (vaccinations) and outcomes (cases and deaths) measured, and what is the meaning of data artifacts such as day-of-week patterns and proportions that exceed 100%? 
- What was the effect of the vaccination rollout on cases and deaths? 

Specific tasks: 
- Compute the "case-fatality ratio" by week. The case-fatality ratio is the number of deaths divided by the number of cases. Plot the time series and explain what you see. 
- Construct and fit a statistical model to estimate the effect of cumulative vaccine coverage on cases, deaths, and the case-fatality rate. Describe the quantity you are estimating, its units, and quantify your uncertainty in the estimate. Explain why this estimate might or might not measure the effect you are interested in. 
- Use the dates of the Governor's actions (listed in the Background section) to study the their effects on cases and deaths. Formulate and fit a statistical model to do this, and explain what you're measuring. Estimate uncertainty. 

## References

Some references you may find helpful: 

- O Morozova, Z Li, F W Crawford. One year of modeling COVID-19 transmission to support policymakers in Connecticut (2021) Scientific Reports, 11 20271
- F W Crawford, S Jones, M Cartter, S G Dean, J L Warren, Zehang Li, J Barbieri, J Campbell, P Kenney, T Valleau, O Morozova. Impact of close interpersonal contact on COVID-19 incidence: evidence from one year of mobile device data (2022) Science Advances, 8 
- O Schultes, V Clarke, A D Paltiel, M Cartter, L Sosa, F W Crawford. COVID-19 Testing and Case Rates and Social Contact Among Residential College Students in Connecticut During the 2020-2021 Academic Year (2021) JAMA Network Open, 12 e2140602 




