
Procrastination Analysis Report

Nuoya Rezsonya & Steven Millett

Contact Information:

Nuoya Rezsonya

email: nrezsonya@smu.edu


Steven Millett

email: smillett@smu.edu

git repository: https://github.com/stmillett

Introduction

In this repository, we have uploaded and adapted R code in order to deliver the analysis process, analysis result and conclusion of the procrastination data set provided by the client.

Purpose:

The purpose of this study was to do an in-depth analysis of two data from two different sources, questionnaire data from different surveys regarding procrastination and Human Development Index data scraped from the HDI Wikipedia page. The procrastination data has characteristics of the respondents to the survey and how they answered four surveys regarding procrastination, the adult inventory of procrastination scale, general procrastination scale, decisional procrastination scale, and the satisfaction with life scale. 

Conclusion:

We found that higher average feelings of procrastination are felt by individuals that live in countries with high to very high levels of human development. It is the finding of the authors that further study should be done to determine whether these surveys were random samples of the population and can be used to make an inference to the entire population. 

Additional Information:

There are two main files in this repository:

Analysis Folder: this contains the analysis 

1. Data: The original data set, Procrastination.csv 

2. Case_Study_2.md - the markdown file that contains the details of this analysis(report and plots)

3. Case_Study_2.html - the html output by knitting rmarkdown file

4. Case_Study_2.Rmd - the rmarkdown file created by our group that contains the details of this analysis(report and plots)

5. Case_Study_2_files/figure-html - contains all the plots generated from this analysis

Output Folder

1. AIP15.csv - Top 15 nations in average procrastination scores using the measure of AIP score

2. GP15.csv - Top 15 nations in average procrastination scores using the measure of the GP score

3. HDI.csv - Finalized HDI table(country, HDI score, HDI category)

4. cleaned_data.csv - tidied version of the original data

5. jobdata.csv - Finalized job data

6. Country.csv - Finalized output of Country by participants

Variables:

• Age: The participant’s age in years.

• Gender: The gender the participant identifies as (Male or Female)

• Kids: Binary, whether they have kids or not.

• Edu: Education level

• Work Status: What kind of job are they working?

• Annual Income: All converted to dollars.

• Current Occupation: A write-in for occupation.

• How long have you held this position?: Years: Number of years in this job.

• How long have you held this position?: Months: Number of months in this job.

• Community: Size of community

• Country of Residence: The country where the person holds citizenship.

• Marital Status: Single, Married, Divorced, Separated, etc.

• Number of sons/Number of daughters: integer number of children.

• All variables starting DP(5 columns/questions) – the Decisional Procrastination Scale (Mann, 1982)

• All variables starting AIP(15 columns/quesions) – Adult Inventory of Procrastination (McCown & Johnson, 1989)

• All variables starting GP(20 columns/questions) – the General Procrastination scale (Lay, 1986)

• All variables starting SWLS(5 columns/questions) – the Satisfaction with Life Scale (Diener et al., 1985)

• Do you consider yourself a procrastinator?: a binary response

• Do others consider you a procrastinator?: a binary response


Session Information

R version 3.4.2 (2017-09-28)

Platform: x86_64-w64-mingw32/x64 (64-bit)

Running under: Windows 7 x64 (build 7601) Service Pack 1

Libraries used:

ggplot2 2.2.1 

knitr 1.17

rvest 0.3.2

XML 3.98-1.9

RCurl 1.95-4.8

plyr 1.8.4

stringr 1.2.0

xtable 1.8-2

kableExtra 0.6.1

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
[1] compiler_3.4.2 tools_3.4.2   

