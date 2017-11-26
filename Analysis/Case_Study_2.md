---
title: "MSDS 6306 Case Study 2"
author: "Nuoya Rezsonya & Steven Millett"
date: "November 23, 2017"
output: 
  html_document:
      keep_md: true
---



### Introduction

This report summarizes the statistical analysis, analysis results and analysis plots associated with the procrastination data set.

The purpose of this report is to illustrate data processing and modelling method used in this analysis as well as inferences and conclusions one can draw from it.

Project target is providing the analysis result to clients who want to get a preliminary understanding of how procrasination and life satisfaction scale are associated with the following factors:

* Age
	
* Gender
	
* Children(having children or not and how many children)
	
* Education Level
	
* Work Status(full time or part time)
	
* Annual Income
	
* Occupation
	
* Years of Experience
	
* Community Size
	
* Country of Residence(HDIs of Nations)
	
* Marital Status

Project report including:

* Statistical analysis:

	+ keep all pbservations where the participant is above 18
	
	+ provide descriptive statistics on age, income, HDI and mean scores of decisional procrastination, procrastination behavior,generalized procrastination and life satisfactor

	+ provide histograms of Age and mean score of generalized procrastination

	+ provide tables of the number count of participants in the survey by gender, work status and occupation
	
	+ provide table of the number count of participants in the survey per contry in descending order
	
	+ provide how many people said they felt they were procrastinators and also said others thought they were procrastinators
	
	+ provide a plot which displays top 15 nations in average generalized procrastination score
	
	+ provide a plot which displays top 15 nations in average procrastination behavior score
	
	+ find out the relationship between age and income
	
	+ find out the relationship between life satisfaction score and HDI score
	
	+ find out the relationship between life satisfaction score and HDI category
	
* Conclusion and references from the statistical analysis 

* Output files(in csv format) can be found in the repository.

### Analysis processes

#### Import of procrastination data

##### Importing the procrastination data that we got from the client and get the dimension of the data.This data set has 4262 rows and 61 columns.


```r
procrastination_data <- read.csv("Data/Procrastination.csv",stringsAsFactors = FALSE)

kable(dim(procrastination_data), header = "Dimension of procrastination dataset")
```

```
## Warning in kable_markdown(x = structure(c("4264", "61"), .Dim = c(2L, 1L:
## The table should have a header (column names)
```



|     |
|----:|
| 4264|
|   61|


#### Munging the imported data 

##### 1. Renaming the values of the columns to limit the size of all variable names to 12 characters or less. We have a lot of questions from different questionnaires, due to the fact that we are more interested in the average score from these questionnaires we are simply going to create sequential names based on the source questionnaire. 


```r
#a function that removes all of the periods from variable names and makes the name into Camel Case form.
camel <- function(x){ #function for camel case
    capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
    sapply(strsplit(x, "\\."), function(x) paste(capit(x), collapse=""))
}

names(procrastination_data)<-camel(names(procrastination_data))

#a manual update of variable names that are too long or not descriptive. 
procrastination_data<- rename(x=procrastination_data,replace=c("HowLongHaveYouHeldThisPositionYears"="ExpYears", "Edu"="Education",
"CountryOfResidence"="Country", 
"ÏAge"="Age",                              
"HowLongHaveYouHeldThisPositionMonths"="ExpMonths",
"DoYouConsiderYourselfAProcrastinator"="SelfQuestion",
"NumberOfDaughters" = "Daughters", 
"NumberOfSons" = "Sons",
"CurrentOccupation"="Job",
"CommunitySize"="Community",
"MaritalStatus"="Marital",
"DoOthersConsiderYouAProcrastinator"="OthQuestion",
"AnnualIncome"="Income"))

#This will rename the columns of the different questionnaires 

colnames(procrastination_data)[grep(names(procrastination_data),pattern = "GP")] <- sprintf("GPQues%d",1:length(grep(names(procrastination_data),pattern = "GP")))
colnames(procrastination_data)[grep(names(procrastination_data),pattern = "AIP")] <- sprintf("AIPQues%d",1:length(grep(names(procrastination_data),pattern = "AIP")))
colnames(procrastination_data)[grep(names(procrastination_data),pattern = "SWLS")] <- sprintf("SWLSQues%d",1:length(grep(names(procrastination_data),pattern = "SWLS")))
colnames(procrastination_data)[grep(names(procrastination_data),pattern = "DP")] <- sprintf("DPQues%d",1:length(grep(names(procrastination_data),pattern = "DP")))
```

##### 2. Cleaning up the data. We are eliminating values that don't make sense as well as errors that occured when the data was exported.

* There are unrealistic and null values in the years of experience data, here those values will be assigned to zero. We also round up values to only one digit.


```r
#Years of experience
#For years of experience any unrealistic value or null value is assigned 
procrastination_data$ExpYears <-as.numeric(procrastination_data$ExpYears)

procrastination_data$ExpYears[procrastination_data$ExpYears==999 | is.na(procrastination_data$ExpYears)] <- 0

procrastination_data$ExpYears <- round(procrastination_data$ExpYears,digits=1)
```

* There are zeros in the occupation data. We are replacing zeros with NA. And also there are blanks in the Income data. We are replacing them with zero.


```r
#We are replacing mis identified
procrastination_data$Job[procrastination_data$Job=="0"] <- "NA"

#Any blank income is assigned a value of 0
procrastination_data$Income[is.na(procrastination_data$Income)] <- 0
```

* Relabelling the number of sons data. The data is labelled as Male and Female. Here we relabel the genders back to intergers with Male=1 and Female =2.


```r
procrastination_data$Sons[procrastination_data$Sons=="Male"] <- "1"
procrastination_data$Sons[procrastination_data$Sons=="Female"] <- "2"
```

* Update the kids data to only Yes and No.


```r
#The Kids data is updated to only Yes or no
procrastination_data$Kids <- ifelse(grepl(procrastination_data$Kids,pattern = "Yes"),"Yes","No") 
```

* Update the age data. We are truncating all values of age after the decimal.


```r
#We are truncating all values of age after the decimal
procrastination_data$Age <- trunc(procrastination_data$Age,digits=0)
```

* There are zero values in the contry of residence. We are replacing them with NA to treat this as missing.


```r
#This is to replace all 0 values of Country with an empty string
procrastination_data$Country[procrastination_data$Country=="0"] <- "NA"
```

* There are blanks answers under the question: 
	* Do you consider yourself a procrastinator?
	* Do others consider you a procrastinator?
	* Here we assign them a NA value.
	

```r
#Any blank answers in the procrastination questionnaires are assigned a Yes value
procrastination_data$OthQuestion[procrastination_data$OthQuestion==""] <- "NA"
procrastination_data$SelfQuestion[procrastination_data$SelfQuestion==""] <- "NA"
procrastination_data$SelfQuestion[procrastination_data$SelfQuestion == '0'] <- "NA"
procrastination_data$SelfQuestion[procrastination_data$SelfQuestion == '4'] <- "NA"
```

* Creating columns for the mean of DP,AIP,GP and SWLS to represent the individual's average decisional procrastination, procrastination behavior,generalized procrastination and life satisfaction. We round the mean up to only one digit.


```r
#Here we are greping all of the variables with certain criteria in their names and creating a new variable of the mean of variables
procrastination_data$GPMean <- rowMeans(procrastination_data[,grep(names(procrastination_data),pattern = "GP")])
procrastination_data$AIPMean <- rowMeans(procrastination_data[,grep(names(procrastination_data),pattern = "AIP")])
procrastination_data$SWLSMean <- rowMeans(procrastination_data[,grep(names(procrastination_data),pattern = "SWLS")])
procrastination_data$DPMean <- rowMeans(procrastination_data[,grep(names(procrastination_data),pattern = "DP")])

#We are rounding the characters to only 1 digit after the decimal
procrastination_data$GPMean <- round(procrastination_data$GPMean,digits=1)
procrastination_data$AIPMean <- round(procrastination_data$AIPMean,digits=1)
```

* The job titles also need to be organized. 

	* In this process, any job title with 'please specify' will be assigned to a NA to be treated as missing.
	
	* All students are titled as student.
	
	* Professional job titles are being assigned to a more general term, i.e. yoga teacher and ESL teacher were simplified to teacher.
	
	* Everyone with a job status of Unemployed with a blank occupation is assigned the value Unemployed for their occupation 
	
	* All Job titles under 5 characters were made to an empty string.
	
	* All jobs with a slash(/) had all text after the slash removed.
	
	* All jobs with parantheses had the parantheses removed.
	
	* All jobs with leading and trailing white space were trimmed.
	

```r
camelpreserve <- function(x){ #function for camel case
    capit <- function(temp_x) {
      temp_x<-tolower(temp_x)
      paste0(toupper(substring(temp_x, 1, 1)), substring(temp_x, 2, nchar(temp_x)))
    }
    capit2 <- function(temp_x) {
      paste0(toupper(substring(temp_x, 1, 1)), substring(temp_x, 2, nchar(temp_x)))
    }
    x2<-sapply(strsplit(x, "[ ]+"), function(x) paste(capit(x), collapse=" "))
    sapply(strsplit(x2, "\\-"), function(x2) paste(capit2(x2), collapse="-"))
}

#Any job title where the person filled in please specify is made into an empty string.

procrastination_data$Job[grep(procrastination_data$Job,pattern = "please specify")] <- "NA"

#All students are titled as student. As well if someone put their work status as Student then their occupation was updated to student
procrastination_data$Job[grep(procrastination_data$WorkStatus,pattern = "[sS]tudent")] <- "Student"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[sS]tudent")] <- "Student"

#These are statements to make professional job titles more general, i.e. yoga teacher and ESL teacher were simplified to teacher
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[tT]eacher")] <- "Teacher"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[wW]riter")] <- "Writer"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "RN|[nN]urse|LPN|PCA")] <- "Nurse"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[cC][ ]*[eE][ ]*[oO]|[Cc]hief")] <- "Executive"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "IT|[Nn]etwork")] <- "Information Technology"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[dD]octor|[mM][dD]")] <- "Doctor"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[sS]ales")] <- "Sales"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Aa]cademic")] <- "Academic"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Pp]roducer")] <- "Producer"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Ss]upervis")] <- "Supervisor"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Dd]esigner")] <- "Designer"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Ff]inanc|[Bb]ank")] <- "Finance"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Ss]oftware")] <- "Software Developer"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "houswife|\\Shome|^home")] <- "Homemaker"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Ee]ngineer")] <- "Engineer"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Rr]eal [Ee]state")] <- "Real Estate"

procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Aa]dmin")] <- "Administration"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Aa]nalyst")] <- "Analyst"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Aa]rt")] <- "Art"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Aa]ssist")] <- "Assistant"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Aa]ttor")] <- "Attorney"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Bb]usiness")] <- "Business"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Cc]linical")] <- "Clinical"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Cc]ommunications")] <- "Communication"
procrastination_data$Job[grep(procrastination_data$Job,pattern = "[Cc]omputer")] <- "Computer"
procrastination_data$Job[procrastination_data$Job==""] <- "NA"
#Everyone with a job status of Unemployed with a blank occupation is assigned the value Unemployed for their occupation 
procrastination_data$Job[procrastination_data$WorkStatus=="unemployed"& procrastination_data$Job==""] <- "Unemployed"

#All Job titles under 5 characters were made to an empty string
procrastination_data$Job[nchar(procrastination_data$Job)<5] <- ""
procrastination_data$Job[procrastination_data$Job==""] <- "NA"

#All jobs with a slash(/) had all text after the slash removed
procrastination_data$Job<-sub("\\s*/.*", "", procrastination_data$Job)

#All jobs with parantheses had the parantheses removed
procrastination_data$Job<-sub("\\s*\\(.*", "", procrastination_data$Job)

#All jobs with leading and trailing white space were trimmed
procrastination_data$Job<-gsub("^\\s+|\\s+$", "", procrastination_data$Job)

procrastination_data$Job<-camelpreserve(procrastination_data$Job)

procrastination_data$Job[procrastination_data$Job==""] <- "NA"
```

* There are blanks in Gender data. We are replacing blanks with NA.


```r
procrastination_data$Gender[procrastination_data$Gender ==""] <- "NA"
```

* There are blanks in Working Status. We are replacing blanks with NA.

```r
procrastination_data$WorkStatus[procrastination_data$WorkStatus ==""] <- "NA"
```

#### Web Scraping wikipedia

##### 1. We are pulling data(HDI for nations recognized by the United Nation only) from the Human Development Index page on Wikipedia. We will combine this data from different tables and assign it a category value based on the HDI score.


```r
#This is a function to combine multiple tables of scraped data that share a similar category
bindData <- function(firstframe,dataset,category){
	
#Here we are assigning the columns we need to some temporary variables and renaming the columns
working_temp <- dataset[[firstframe]][2:nrow(dataset[[firstframe]]),c(3,4)]
names(working_temp)<-c("Country","HDI")
working_temp1 <- dataset[[firstframe+1]][2:nrow(dataset[[firstframe+1]]),c(3,4)]
names(working_temp1)<-c("Country","HDI")

#We are binding the rows of our two temp variables and adding the Category value.
working_temp<-rbind(working_temp,working_temp1)
working_temp<-cbind(working_temp,"Category"=category)
}

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index"

HDI_table <- url %>%
  read_html() %>%  
  html_nodes("table")%>%
html_table(fill=TRUE)

HDI <- data.frame("Country","HDI","Category")

HDI<-rbind(bindData(4,HDI_table,"Very high human development"),
           bindData(7,HDI_table,"High human development"),
           bindData(10,HDI_table,"Medium human development"),
           bindData(13,HDI_table,"Low human development"))
```

##### 2. Merging our procrastination data to the HDI data pulled from Wikipedia. 

```r
#We are doing a left merge of the procrastination data on the HDI data pulled from wikipedia. This means that if there is a missing country value from the procrastination data we will still bring that data over with missing HDI information.
merged_data<-merge(x=procrastination_data,y=HDI,by="Country",all.x=TRUE)
```

##### 3. Based on the request from our client. We only study subjects over the age of 18 so we are selecting a subset of only ages that we can confirm are over the age of 18.And let columns have proper data type.

```r
cleaned_data <- merged_data[merged_data$Age>18 & !is.na(merged_data$Age),]
# make columns into proper data types
cleaned_data$Sons <- as.numeric(cleaned_data$Sons)
cleaned_data$HDI <- as.numeric(cleaned_data$HDI )
```

##### 4. Presented below are the descriptive statistics on Age, Income, HDI, and for mean columns of GP,AIP,SWLS,DP. There are two histograms for Age and Mean GP data. The histogram for Age is right skewed while the histogram for Mean GP is more symmetrical and bell shaped.


```r
agesummary <- summary(cleaned_data$Age)
incomesummary <-summary(cleaned_data$Income)
HDIsummary <- summary(cleaned_data$HDI)

agesummary
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   19.00   28.00   37.00   38.14   45.00   80.00
```

```r
incomesummary
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0   10000   35000   53723   67500  250000
```

```r
HDIsummary
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.4790  0.9200  0.9200  0.9056  0.9200  0.9490     192
```

```r
meanGPsummary <- summary(cleaned_data$GPMean)
meanAIPSsummary <-summary(cleaned_data$AIPMean)
meanSWLsummary <-summary(cleaned_data$SWLSMean)
meanDPsummary <- summary(cleaned_data$DPMean)

meanGPsummary 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.800   3.200   3.235   3.800   5.000
```

```r
meanAIPSsummary
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.400   2.900   2.964   3.500   5.000
```

```r
meanSWLsummary 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.400   3.000   3.047   3.800   5.000
```

```r
meanDPsummary 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.400   3.000   3.052   3.800   5.000
```

```r
#histogram of age
qplot(cleaned_data$Age, 
			geom="histogram",
      binwidth = 2,  
      main = "Histogram for Age", 
      xlab = "Age",  
      fill=I("light blue"), 
      col=I("red"))+
	theme(plot.title=element_text(hjust = .5), axis.ticks.y=element_blank(),axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(angle=60,hjust=1))
```

![](Case_Study_2_files/figure-html/countByGender-1.png)<!-- -->

```r
#histogram of mean GP
qplot(cleaned_data$GPMean, 
			geom="histogram",
      binwidth = 0.1,  
      main = "Histogram for Mean GP", 
      xlab = "Mean GP Score",  
      fill=I("light blue"), 
      col=I("red"))+
	theme(plot.title=element_text(hjust = .5), axis.ticks.y=element_blank(),axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(angle=60,hjust=1))
```

![](Case_Study_2_files/figure-html/countByGender-2.png)<!-- -->

##### 5. Presented below is a table of the number count of the participants in the survey by genders.If there are blanks in gender data, they will be assigned NA. Therefore the table will have females, males and NA.

```r
frequencyOfRespondantsByGender <- as.data.frame(table(cleaned_data$Gender))
colnames(frequencyOfRespondantsByGender) <- c("Gender","Number of Participants")
kable(frequencyOfRespondantsByGender[order(-frequencyOfRespondantsByGender$`Number of Participants`),],row.names = FALSE)
```



Gender    Number of Participants
-------  -----------------------
Female                      2309
Male                        1721
NA                             6


##### 6. Presented below is a table of the number count of the participants in the survey by Work Status.If there are 'blanks'please specify" in work status data, they will be assigned NA. Therefore the table will have full-time, part-time,student,unemployed, retired and NA.

```r
frequencyOfRespondantsByWork <- as.data.frame(table(cleaned_data$WorkStatus))
colnames(frequencyOfRespondantsByWork) <- c("WorkStatus","Number of Participants")
kable(frequencyOfRespondantsByWork[order(-frequencyOfRespondantsByWork$`Number of Participants`),],row.names = FALSE)
```



WorkStatus    Number of Participants
-----------  -----------------------
full-time                       2260
student                          837
part-time                        465
unemployed                       258
retired                          174
NA                                42

##### 7. Presented below is a table of the number count of the participants in the survey by Occupation.


```r
frequencyOfRespondantsByJob<- as.data.frame(table(cleaned_data$Job))
colnames(frequencyOfRespondantsByJob) <- c("Job","Number of Participants")
kable(frequencyOfRespondantsByJob[order(-frequencyOfRespondantsByJob$`Number of Participants`),],row.names = FALSE)
```



Job                                         Number of Participants
-----------------------------------------  -----------------------
Na                                                            1966
Student                                                        845
Teacher                                                         88
Attorney                                                        53
College Professor                                               42
Writer                                                          41
Engineer                                                        37
Analyst                                                         35
Assistant                                                       31
Manager                                                         31
Finance                                                         28
Information Technology                                          28
Retired                                                         28
Homemaker                                                       27
Designer                                                        25
Administration                                                  24
Software Developer                                              23
Editor                                                          21
Nurse                                                           21
Marketing                                                       20
Doctor                                                          19
Sales                                                           18
Supervisor                                                      13
Unemployed                                                      13
Business                                                        12
Scientist                                                       12
Consultant                                                      11
Director                                                        11
Executive                                                       11
Computer                                                        10
Project Manager                                                 10
Librarian                                                        9
Customer Service                                                 8
Journalist                                                       8
Research Scientist                                               8
President                                                        7
Psychologis                                                      7
Social Worker                                                    7
Academic                                                         6
Server                                                           6
Translator                                                       6
Pastor ; Life Coach Clergy                                       5
Real Estate                                                      5
Architect                                                        4
Communication                                                    4
Insurance Agent                                                  4
Operations Manager                                               4
Producer                                                         4
Programmer                                                       4
Research Associate                                               4
Secretary                                                        4
Deputy Director                                                  3
Human Resource Manager                                           3
Law Enforcement                                                  3
Musician                                                         3
Pharmacist                                                       3
Psychotherapist                                                  3
Receptionist                                                     3
Researcher                                                       3
Self Employed                                                    3
Accountant                                                       2
Accounting                                                       2
Accounting Manager                                               2
Accounts Payable                                                 2
Associate                                                        2
Associate Director                                               2
Bookkeeper                                                       2
Civil Servant                                                    2
Clerk                                                            2
Clinical                                                         2
Consumer Case Coordinator                                        2
Counselor                                                        2
Creative Director                                                2
Dentist                                                          2
Diplomat                                                         2
Epidemiologist                                                   2
Executive Director                                               2
Geologist                                                        2
Information Technology Consultant                                2
Insurance                                                        2
Lecturer                                                         2
Letter Carrier                                                   2
Library Technician                                               2
Medical                                                          2
Office Manager                                                   2
Owner                                                            2
Paralegal                                                        2
Paraprofessional                                                 2
Postdoc                                                          2
Press Officer                                                    2
Retail                                                           2
Self-Employed Photographer                                       2
Stocker                                                          2
Training Coordinator                                             2
Tutor                                                            2
University Faculty                                               2
Vice-President                                                   2
Vice President                                                   2
Account Manager                                                  1
Account Planner                                                  1
Account Service Rep                                              1
Activities Leader                                                1
Actress                                                          1
Acupuncturist                                                    1
Adjunct Faculty                                                  1
Adult Care                                                       1
Advocate                                                         1
Agronomist                                                       1
Airline                                                          1
Airport Ground Handler                                           1
Anthropologist                                                   1
Antique Dealer                                                   1
Associate At Law Firm                                            1
Asst. Prof.                                                      1
Astrohysicist                                                    1
Aviation Specialist                                              1
Bar & Restaurant Owner                                           1
Biologist                                                        1
Box Office Representative                                        1
Braillist                                                        1
Business Consulta                                                1
Buyer                                                            1
Cad Operator                                                     1
Cad Technician                                                   1
Camera Coordinator                                               1
Campus Planner                                                   1
Capstone Golf Course                                             1
Career Placement Associate                                       1
Case Manager                                                     1
Casting Director                                                 1
Chairman Of The Board                                            1
Chauffeur                                                        1
Chiropractor                                                     1
Clutter Clearer, Video Editor, Caterer                           1
Co-Proprietor                                                    1
Collection Management Specialist                                 1
College Faculty                                                  1
Company Director                                                 1
Consultant And Entrepreneur                                      1
Consulting Manager                                               1
Controller                                                       1
Contsuruction Management                                         1
Coordinator Of International Programs                            1
Coordinatore Operativo                                           1
Corporate Instructor                                             1
Corporate Trainer                                                1
Corporation President                                            1
Corrections                                                      1
Country Style Employee                                           1
Creative Consultant                                              1
Dealer                                                           1
Dental & Disability Coordinator                                  1
Dept. Director                                                   1
Deputy Chieif Information Officer                                1
Deputy Practice Manager                                          1
Developer                                                        1
Dietitian                                                        1
Director Of A Language Program                                   1
Director Of Contract Management                                  1
Director Of Non-Profit Organization                              1
Director Operations                                              1
Director,social Dvelopment                                       1
Disability Allowance                                             1
Dish Washer                                                      1
Divisional Manager Of A Large Cosmetics                          1
Driver                                                           1
Ecology Technician                                               1
Economist                                                        1
Economy                                                          1
Education                                                        1
Education Specialist                                             1
Ehs Manager                                                      1
Election Services                                                1
Electrical Technician                                            1
Electronic Technician                                            1
Employed By A Church                                             1
Energy Therapist                                                 1
Enologist                                                        1
Entertainer                                                      1
Entrepreneur                                                     1
Entrepreneur & Consultant                                        1
Environmental Education Non Profit Direc                         1
Environmental Senior Specialist                                  1
Executive Officer                                                1
Executive Vice President                                         1
Facilitator                                                      1
Facilities Management                                            1
Farm Manager                                                     1
Federal Excise Tax Auditor                                       1
Field Coordinator                                                1
Film Editor                                                      1
Film Industry                                                    1
Film Maker                                                       1
First Vp & Associate General Counsel                             1
Fitness Instructor                                               1
Flight Surgeon                                                   1
Foreign Affairs Specialist                                       1
Free Lance Bookkeeper                                            1
Free Lance Editor And Tutor--In Theory                           1
Free Professionist                                               1
Freelance                                                        1
Freelance Project Manager                                        1
Gender                                                           1
Geophysicist                                                     1
Gove Service                                                     1
Head - Operations & Qa                                           1
Health Care                                                      1
Healthcare Consultant                                            1
Hostess                                                          1
Hotel Desk Clerk                                                 1
Housekeeping                                                     1
Hr Generalist                                                    1
Human Resource Manger                                            1
Hvac Tech                                                        1
Ict Director                                                     1
In-House Legal Counsel                                           1
Information Assisstant                                           1
Information Developer                                            1
Information Management                                           1
Innkeeper                                                        1
Instructor                                                       1
Insurance Coordinator                                            1
Internet & Media Consultant                                      1
Internship                                                       1
Interpreter                                                      1
Investigative Specialist                                         1
Investment Counsel                                               1
Istraining Coordinator                                           1
Janitor                                                          1
Juvenile Corrections Officer                                     1
Lab Director                                                     1
Labor Relations Specialist                                       1
Laboratory Technician                                            1
Land Use Planner                                                 1
Language Service Provider                                        1
Language Trainer                                                 1
Law Clerk                                                        1
Legal Secretary                                                  1
Library Paraprofessional                                         1
Licensed Professional Counselor                                  1
Maintenance Tech.                                                1
Management Consultant                                            1
Management Consultant & Entrepreneur                             1
Manager - Analytical And Environmental S                         1
Manager,interacitve Media                                        1
Manufacturing                                                    1
Master Control Operator                                          1
Media Consultant                                                 1
Media Relations                                                  1
Media Relations Manager                                          1
Medical Laboratory                                               1
Medical Practitioner                                             1
Medical Sonographer                                              1
Mentor                                                           1
Military                                                         1
Multimedia Developer                                             1
Museum Docent                                                    1
New Realtor                                                      1
Newspaper Carrier                                                1
Non-Profit Consultant                                            1
Nursing Home                                                     1
Office                                                           1
Office Services Manager                                          1
Organic Grocery Store Cashier                                    1
Outdoor Recreation Coordinator                                   1
Owner - Private Practice Physical Therap                         1
P-T College Faculty & P-T Self-Employed                          1
Pathology                                                        1
Pharmacy Tech.                                                   1
Photo Profucer                                                   1
Physician                                                        1
Physicist                                                        1
Physiotherapst                                                   1
Pjublic Relations Director                                       1
Please Specify Title Manager For Regulat                         1
President Nongovernmental Organization                           1
Private Equity Principal                                         1
Pro Poker Player                                                 1
Procrastinator                                                   1
Product Field Test Manager                                       1
Professional Organizer                                           1
Program Coordinator                                              1
Program Director                                                 1
Program Director At A Non-Profit Organiz                         1
Program Manager                                                  1
Program Manager And Acting Director                              1
Program Officer                                                  1
Program Specialist                                               1
Proofreader                                                      1
Proposal Director                                                1
Psychiatrist In Private Practice                                 1
Public Health                                                    1
Public Relations                                                 1
Publishing                                                       1
Quality Manager                                                  1
Quotations Specialist                                            1
Realtor                                                          1
Recreational Staff                                               1
Registered Respiratory Therapist                                 1
Regulatory Affairs                                               1
Research Coordinator                                             1
Research Manager                                                 1
Research Specialist                                              1
Research Technician                                              1
Researcher - Physician                                           1
Resident Physician                                               1
Respiratory Therapist                                            1
Restaurant Operations Manager                                    1
Rocket Scientist                                                 1
School Counselor                                                 1
Science Writing Intern                                           1
Self-Employed                                                    1
Self-Employed Family Therapist                                   1
Self-Employed Translator                                         1
Self Employed Public Relations                                   1
Self Employeed                                                   1
Selfemplyed Renovator                                            1
Senior Consultant                                                1
Senior Grant Officer                                             1
Senior Human Resources Consultant                                1
Senior Policy Advisor                                            1
Senior Project Manager                                           1
Service Co-Ordinator                                             1
Service Registrar                                                1
Set Lighting Technician                                          1
Shipping                                                         1
Social Media Consultant                                          1
Speaker                                                          1
Speaker Author Consultant                                        1
Special Projects Editor                                          1
Specialist                                                       1
Sr. Drug Safety Associate                                        1
Statistician                                                     1
Steamship Agent                                                  1
Surgeon                                                          1
Surgical Resident                                                1
System Manager                                                   1
Tax Consultant                                                   1
Tax Examiner                                                     1
Technical Coordinator                                            1
Technical Director                                               1
Technical Officer                                                1
Technical Support Rep                                            1
Technical Trainer                                                1
Technology                                                       1
Technology Curriculum Developer Science                          1
Television Director                                              1
Temporary Office                                                 1
Theater General Manager                                          1
Town Clerk                                                       1
Town Planner                                                     1
Trader                                                           1
Traffic Reporter-Radio                                           1
Trainee                                                          1
Treatment Support Co-Ordinator                                   1
Tv Broadcast Technician                                          1
University Staff                                                 1
Urban Planner                                                    1
Veterinarian                                                     1
Vetrans Representative                                           1
Volunteer Director                                               1
Vp Scientific Affairs                                            1
Warehouse                                                        1

##### 8. Presented below is a table of the number count of the participants in the survey per country. Blanks in the country data will be assigned to NA. 


```r
cleaned_data$Country[cleaned_data$Country==""] <- "NA"
frequencyOfRespondantsByCountry <- as.data.frame(table(cleaned_data$Country))
colnames(frequencyOfRespondantsByCountry) <- c("Country","Number of Participants")
kable(frequencyOfRespondantsByCountry[order(-frequencyOfRespondantsByCountry$`Number of Participants`),],row.names = FALSE)
```



Country               Number of Participants
-------------------  -----------------------
United States                           2785
Canada                                   243
United Kingdom                           177
NA                                       160
Australia                                 99
India                                     78
Italy                                     62
Germany                                   36
Brazil                                    20
Ireland                                   19
Isreal                                    19
Netherlands                               18
Sweden                                    15
Norway                                    14
France                                    13
Japan                                     13
Spain                                     13
China                                     12
Finland                                   12
Mexico                                    12
New Zealand                               12
South Africa                              12
Switzerland                               12
Philippines                               11
Greece                                    10
Belgium                                    9
Denmark                                    9
Turkey                                     9
Hong Kong                                  7
Portugal                                   7
Slovenia                                   6
Poland                                     5
Romania                                    5
Afghanistan                                4
Chile                                      4
Croatia                                    4
Malaysia                                   4
Singapore                                  4
Algeria                                    3
Argentina                                  3
Austria                                    3
Czech Republic                             3
Ecuador                                    3
Puerto Rico                                3
Uruguay                                    3
Albania                                    2
Andorra                                    2
Bermuda                                    2
Bulgaria                                   2
Columbia                                   2
Ghana                                      2
Iran                                       2
Malta                                      2
Peru                                       2
Saudi Arabia                               2
South Korea                                2
Thailand                                   2
Ukraine                                    2
Venezuela                                  2
Yugoslavia                                 2
Antigua                                    1
Bahamas                                    1
Barbados                                   1
Bolivia                                    1
Botswana                                   1
Brunei                                     1
Cyprus                                     1
Dominican Republic                         1
Egypt                                      1
El Salvador                                1
Guam                                       1
Guyana                                     1
Hungary                                    1
Iceland                                    1
Jamaica                                    1
Kazakhstan                                 1
Kenya                                      1
Lithuania                                  1
Luxembourg                                 1
Macao                                      1
Macedonia                                  1
Morocco                                    1
Myanmar                                    1
Nicaragua                                  1
Pakistan                                   1
Panama                                     1
Qatar                                      1
Russia                                     1
Sri Lanka                                  1
Taiwan                                     1
Vietnam                                    1

##### 9. Presented below is a total number of the matched answers from question: whether the person considers themselves a procrastinator and question: whether others consider them a procrastinator. There are 2846 people matched their perceptions to others.


```r
cleaned_data$matching <- ifelse(cleaned_data$SelfQuestion ==cleaned_data$OthQuestion,1,0)
matched <-sum(cleaned_data$matching)
matched 
```

```
## [1] 2846
```

##### 10. Presented below is a barchart displaying the top 15 nations in average procrastination scores using the measure of the GP score.Those regions are not recognized as soverign nations will have NA values to their HDI score and HDI category.


```r
top15 <- aggregate(cleaned_data$GPMean,list(cleaned_data$Country),mean)
names(top15) <- c("Country", "GPMean")

merged15 <- merge(x=top15,y=HDI,by='Country',all.x =TRUE)

merged15<- merged15[with(merged15,order(-GPMean)),]
merged15 <- merged15[1:15,]
merged15
```

```
##        Country   GPMean   HDI                    Category
## 82      Taiwan 4.800000  <NA>                        <NA>
## 69 Puerto Rico 4.266667  <NA>                        <NA>
## 70       Qatar 4.200000 0.856 Very high human development
## 64      Panama 4.000000 0.788      High human development
## 57     Myanmar 3.800000 0.556    Medium human development
## 79   Sri Lanka 3.800000 0.766      High human development
## 67      Poland 3.780000 0.855 Very high human development
## 8      Austria 3.766667 0.893 Very high human development
## 84      Turkey 3.744444 0.767      High human development
## 27     Ecuador 3.700000 0.739      High human development
## 31      France 3.684615 0.897 Very high human development
## 75    Slovenia 3.666667 0.890 Very high human development
## 88     Uruguay 3.666667 0.795      High human development
## 53    Malaysia 3.650000 0.789      High human development
## 80      Sweden 3.646667 0.913 Very high human development
```

```r
ggplot(merged15, aes(reorder(Country, GPMean),GPMean)) + 
			geom_bar(stat="identity", aes(fill=Category))+  scale_fill_hue(h = c(5, 100)) +
			ggtitle('Top 15 Nations In Average Procrastination Scores(GP)')+
			ylab('Average Procrastination Scores(GP)')+ 
			xlab('Country')+

			theme(plot.title=element_text(hjust = .5), axis.ticks.y=element_blank(),axis.ticks.x=element_blank()) +
  		theme(axis.text.x = element_text(angle=60,hjust=1))
```

![](Case_Study_2_files/figure-html/barchart5B-1.png)<!-- -->

##### 11. Presented below is a barchart displaying the top 15 nations in average procrastination scores using the measure of the AIP score.Those regions are not recognized as soverign nations will have NA values as their HDI score and HDI category.


```r
AIPtop15 <- aggregate(cleaned_data$AIPMean,list(cleaned_data$Country),mean)
names(AIPtop15) <- c("Country", "AIPMean")

AIPmerged15 <- merge(x=AIPtop15,y=HDI,by='Country',all.x =TRUE)

AIPmerged15<- AIPmerged15[with(AIPmerged15,order(-AIPMean)),]
AIPmerged15<- AIPmerged15[1:15,]
AIPmerged15
```

```
##               Country  AIPMean   HDI                    Category
## 51              Macao 4.600000  <NA>                        <NA>
## 82             Taiwan 4.600000  <NA>                        <NA>
## 26 Dominican Republic 4.500000 0.722      High human development
## 23             Cyprus 4.400000 0.856 Very high human development
## 70              Qatar 4.100000 0.856 Very high human development
## 64             Panama 4.000000 0.788      High human development
## 69        Puerto Rico 4.000000  <NA>                        <NA>
## 39            Iceland 3.900000 0.921 Very high human development
## 27            Ecuador 3.733333 0.739      High human development
## 21           Columbia 3.700000  <NA>                        <NA>
## 48              Kenya 3.700000 0.555    Medium human development
## 79          Sri Lanka 3.700000 0.766      High human development
## 84             Turkey 3.677778 0.767      High human development
## 88            Uruguay 3.600000 0.795      High human development
## 31             France 3.561538 0.897 Very high human development
```

```r
ggplot(AIPmerged15, aes(reorder(Country, AIPMean),AIPMean)) + 
			geom_bar(stat="identity", aes(fill=Category))+  scale_fill_hue(h = c(5, 100)) +
			ggtitle('Top 15 Nations In Average Procrastination Scores(AIP)')+
			ylab('Average Procrastination Scores(AIP)')+ 
			xlab('Country')+

			theme(plot.title=element_text(hjust = .5), axis.ticks.y=element_blank(),axis.ticks.x=element_blank()) +
  		theme(axis.text.x = element_text(angle=60,hjust=1))
```

![](Case_Study_2_files/figure-html/barchart5C-1.png)<!-- -->


##### 12. Presented below is a table displaying nations and regions show up both in GP and AIP plot.


```r
countrymatching<-intersect(merged15$Country,AIPmerged15$Country)
countrymatching <- data.frame(countrymatching)
names(countrymatching) <- c('Country/Region')
countrymatching
```

```
##   Country/Region
## 1         Taiwan
## 2    Puerto Rico
## 3          Qatar
## 4         Panama
## 5      Sri Lanka
## 6         Turkey
## 7        Ecuador
## 8         France
## 9        Uruguay
```

#### 13. Presented below is to show the relationship between Age and Income.

* Finding: At younger ages it appears men and women have about the same mean income, but as they get older on average men will make more money than women.


```r
#scatter plot

ggplot(data=subset(cleaned_data,Gender=="Male"|Gender=="Female"), aes(Age, Income),color=Gender) + geom_jitter(aes(color=Gender)) + 
	scale_color_manual(breaks = c("Female", "Male", ""), values=c("red", "blue", "green")) + 
	geom_smooth(method='lm',mapping=aes(x=Age,y=Income,color=Gender))+
	
	theme(plot.title=element_text(hjust = .5), axis.ticks.y=element_blank(),axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(angle=60,hjust=1))
```

![](Case_Study_2_files/figure-html/age and income-1.png)<!-- -->

```r
#linear regression
AgeIncome <- lm(Income~Age,data = cleaned_data)
lmsummary <- summary(AgeIncome)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(AgeIncome)
```

![](Case_Study_2_files/figure-html/age and income-2.png)<!-- -->

#### 14. Presented below is to show the relationship between Life Satisfaction and HDI Score.

* Finding: In countries that have lower HDI, females appear to have higher life satisfaction level than males.As the HDI increases, the life satisfaction diffenrecence between females and males is getting smaller.


```r
#scatter plot
ggplot(data=cleaned_data, aes(HDI, SWLSMean),color=Gender) + geom_jitter(aes(color=Gender)) + 
	scale_color_manual(breaks = c("Female", "Male", ""), values=c("red", "blue", "green")) + 
	geom_smooth(method='lm',mapping=aes(x=HDI,y=SWLSMean,color=Gender))+
	
	theme(plot.title=element_text(hjust = .5), axis.ticks.y=element_blank(),axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(angle=60,hjust=1))
```

```
## Warning: Removed 192 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 192 rows containing missing values (geom_point).
```

![](Case_Study_2_files/figure-html/SWLS and HDI-1.png)<!-- -->

#### 14. Presented below is to show the relationship between Life Satisfaction and HDI Category.

* Finding: 
In countries that have very high human development category, the life satisfaction mean scores are the highest. 

In countries that have low human development category, the life satisfaction mean scores are the lowest.

In countries that have medium human development category, the life satisfaction mean scores are the higher than those from high human development category. 


```r
#scatter plot of HDI category

ggplot(cleaned_data, aes(Category, SWLSMean)) + 
			geom_bar(stat="identity", aes(fill=Category))+  scale_fill_hue(h = c(5, 100)) +

			ylab('Life Satisfaction Mean Score')+ 
			xlab('HDI Category')+

			theme(plot.title=element_text(hjust = .5), axis.ticks.y=element_blank(),axis.ticks.x=element_blank()) +
  		theme(axis.text.x = element_text(angle=60,hjust=1))
```

![](Case_Study_2_files/figure-html/SWLS and category-1.png)<!-- -->

#### Outputting data

* Finalized HDI table


```r
HDIout <- write.csv(HDI, "HDI.csv", row.names=FALSE)
```

* Tidied version of the original data

```r
cleaned_data <- write.csv(cleaned_data, "cleaned_data.csv", row.names=FALSE)
```

* Top 15 nations in average procrastination scores using the measure of the GP score and AIP score 


```r
top15GP <- write.csv(merged15, "GP15.csv", row.names=FALSE)
top15AIP <- write.csv(AIPmerged15, "AIP15.csv", row.names=FALSE)
```

