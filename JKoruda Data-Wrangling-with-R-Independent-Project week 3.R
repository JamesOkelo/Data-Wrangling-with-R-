##Data Wrangling Independent Project
##1. Defining the question.
##a) Specifying the data analysis question.
##The management would like to get your assistance in understanding the subscribed customers. Your recommendations informed by your analysis will help them ##make decisions on effective customer retention programs.
##
##b) Defining metrics for success.
##The solution will have satisfied the research question when you can identify the factors that contribute to customer churn.
##
##c) Understanding the context.
##MTN Telecom offers mobile and internet services to its customers. These services include phone, multiple lines, internet, online security, online backup, ##device protection, tech support, and streaming TV and movies. The management would like to get your assistance in understanding the subscribed customers. ##Your recommendations informed by your analysis will help them make decisions on effective customer retention programs. We have provided you with the ##current customer data. Since you will be working towards a descriptive report rather than a predictive one, you decide to think critically about the kind ##of questions that would help you craft customer retention programs. You then later use the given data set to answer your questions, but before you start, ##you reading, explore, clean, and analyze your dataset.
##
##d) Recording the experimental design
##The steps to be taken include:
##
##Load dataset and preview its summarized information to get a feel of what you will be working with.
##Carry out data cleaning.
##Carry out data analysis.
##Interpret results.
##Provide recommendations based on results of analysis.
##Challenge your solution.
##e) Data Relevance
##The data contains all the information relevant for our analysis.

##2. Data Cleaning and preparation.
##Installing packages
##Let’s install tidyverse packages in Rstudio
install.packages("tidyverse")
## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.2'
## (as 'lib' is unspecified)
##Load tidyverse for use in our notebook
library(tidyverse)
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.3.6      ✔ purrr   0.3.5 
## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
## ✔ readr   2.1.3      ✔ forcats 0.5.2 
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
# Read provided dataset into data frame and preview

subscriber_df <- read_csv("https://bit.ly/2ZlpzjF")
## Rows: 7050 Columns: 21
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (17): customerID, GENDER, PARTNER, Dependents, PhoneService, MultipleLin...
## dbl  (4): SeniorCitizen, tenure, MonthlyCharges, TotalCharges
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
head(subscriber_df)
customerID
<chr>
GENDER
<chr>
SeniorCitizen
<dbl>
PARTNER
<chr>
Dependents
<chr>
tenure
<dbl>
PhoneService
<chr>
MultipleLines
<chr>
7590-VHVEG	Female	0	Yes	No	1	No	No phone service	
5575-GNVDE	Male	0	No	No	34	Yes	No	
3668-QPYBK	Male	0	No	No	2	Yes	No	
7795-CFOCW	Male	0	No	No	45	No	No phone service	
9237-HQITU	Female	0	No	No	2	Yes	No	
9305-CDSKC	Female	0	No	No	8	Yes	Yes	
6 rows | 1-8 of 21 columns
# Check the shape of the data

dim(subscriber_df)
## [1] 7050   21
# Determine the column names

names(subscriber_df)
##  [1] "customerID"       "GENDER"           "SeniorCitizen"    "PARTNER"         
##  [5] "Dependents"       "tenure"           "PhoneService"     "MultipleLines"   
##  [9] "InternetService"  "OnlineSecurity"   "OnlineBackup"     "DeviceProtection"
## [13] "TECHSUPPORT"      "StreamingTV"      "StreamingMovies"  "Contract"        
## [17] "PaperlessBilling" "PaymentMethod"    "MonthlyCharges"   "TotalCharges"    
## [21] "Churn"
# Change all column names to lower case

names(subscriber_df) <- tolower(names(subscriber_df))
names(subscriber_df)
##  [1] "customerid"       "gender"           "seniorcitizen"    "partner"         
##  [5] "dependents"       "tenure"           "phoneservice"     "multiplelines"   
##  [9] "internetservice"  "onlinesecurity"   "onlinebackup"     "deviceprotection"
## [13] "techsupport"      "streamingtv"      "streamingmovies"  "contract"        
## [17] "paperlessbilling" "paymentmethod"    "monthlycharges"   "totalcharges"    
## [21] "churn"
# Check for duplicates

subscriber_df[duplicated(subscriber_df), ]
customerid
<chr>
gender
<chr>
seniorcitizen
<dbl>
partner
<chr>
dependents
<chr>
tenure
<dbl>
phoneservice
<chr>
multiplelines
<chr>
6876-ADESB	Male	0	No	Yes	1	Yes	No	
1427-VERSM	Female	0	Yes	No	56	Yes	No	
3967-KXAPS	Male	0	Yes	No	72	Yes	Yes	
3967-KXAPS	Male	0	Yes	No	72	Yes	Yes	
2314-TNDJQ	Female	0	Yes	Yes	72	No	No phone service	
2314-TNDJQ	Female	0	Yes	Yes	72	No	No phone service	
4501-VCPFK	Male	0	No	No	26	No	No phone service	
7 rows | 1-8 of 21 columns
# Seven duplicate values found
# Remove duplicates

subscriber_df <- subscriber_df[!duplicated(subscriber_df), ]
dim(subscriber_df)
## [1] 7043   21
# Check for missing values

colSums(is.na(subscriber_df))
##       customerid           gender    seniorcitizen          partner 
##                0                1                3               12 
##       dependents           tenure     phoneservice    multiplelines 
##               10               11               15               17 
##  internetservice   onlinesecurity     onlinebackup deviceprotection 
##               16               16               15               14 
##      techsupport      streamingtv  streamingmovies         contract 
##               13               13               12               12 
## paperlessbilling    paymentmethod   monthlycharges     totalcharges 
##               12               12               12               23 
##            churn 
##               12
# Remove all missing values

subscriber_df <- na.omit(subscriber_df)
dim(subscriber_df)
## [1] 7003   21
# 40 records with missing values removed
# previewing the number of unique values in each column

unique(subscriber_df)
customerid
<chr>
gender
<chr>
seniorcitizen
<dbl>
partner
<chr>
dependents
<chr>
tenure
<dbl>
phoneservice
<chr>
multiplelines
<chr>
7590-VHVEG	Female	0	Yes	No	1	No	No phone service	
5575-GNVDE	Male	0	No	No	34	Yes	No	
3668-QPYBK	Male	0	No	No	2	Yes	No	
7795-CFOCW	Male	0	No	No	45	No	No phone service	
9237-HQITU	Female	0	No	No	2	Yes	No	
9305-CDSKC	Female	0	No	No	8	Yes	Yes	
1452-KIOVK	Male	0	No	Yes	22	Yes	Yes	
6713-OKOMC	Female	0	No	No	10	No	No phone service	
7892-POOKP	Female	0	Yes	No	28	Yes	Yes	
6388-TABGU	Male	0	No	Yes	62	Yes	No	
...
1-10 of 7,003 rows | 1-8 of 21 columns
# Checking the data types

str(subscriber_df)
## tibble [7,003 × 21] (S3: tbl_df/tbl/data.frame)
##  $ customerid      : chr [1:7003] "7590-VHVEG" "5575-GNVDE" "3668-QPYBK" "7795-CFOCW" ...
##  $ gender          : chr [1:7003] "Female" "Male" "Male" "Male" ...
##  $ seniorcitizen   : num [1:7003] 0 0 0 0 0 0 0 0 0 0 ...
##  $ partner         : chr [1:7003] "Yes" "No" "No" "No" ...
##  $ dependents      : chr [1:7003] "No" "No" "No" "No" ...
##  $ tenure          : num [1:7003] 1 34 2 45 2 8 22 10 28 62 ...
##  $ phoneservice    : chr [1:7003] "No" "Yes" "Yes" "No" ...
##  $ multiplelines   : chr [1:7003] "No phone service" "No" "No" "No phone service" ...
##  $ internetservice : chr [1:7003] "DSL" "DSL" "DSL" "DSL" ...
##  $ onlinesecurity  : chr [1:7003] "No" "Yes" "Yes" "Yes" ...
##  $ onlinebackup    : chr [1:7003] "Yes" "No" "Yes" "No" ...
##  $ deviceprotection: chr [1:7003] "No" "Yes" "No" "Yes" ...
##  $ techsupport     : chr [1:7003] "No" "No" "No" "Yes" ...
##  $ streamingtv     : chr [1:7003] "No" "No" "No" "No" ...
##  $ streamingmovies : chr [1:7003] "No" "No" "No" "No" ...
##  $ contract        : chr [1:7003] "Month-to-month" "One year" "Month-to-month" "One year" ...
##  $ paperlessbilling: chr [1:7003] "Yes" "No" "Yes" "No" ...
##  $ paymentmethod   : chr [1:7003] "Electronic check" "Mailed check" "Mailed check" "Bank transfer (automatic)" ...
##  $ monthlycharges  : num [1:7003] 29.9 57 53.9 42.3 70.7 ...
##  $ totalcharges    : num [1:7003] 29.9 1889.5 108.2 1840.8 151.7 ...
##  $ churn           : chr [1:7003] "No" "No" "Yes" "No" ...
##  - attr(*, "na.action")= 'omit' Named int [1:40] 489 754 937 960 961 976 983 1083 1188 1199 ...
##   ..- attr(*, "names")= chr [1:40] "489" "754" "937" "960" ...
# Find unique values of each variable

subscriber_df2 <- subscriber_df %>% as.data.frame()

for (i in colnames(subscriber_df2)[-c(1, 6, 19, 20)]){
  
  cat("Unique values in", i, ":", unique(subscriber_df2[,i]), "\n")
  
 }
## Unique values in gender : Female Male 
## Unique values in seniorcitizen : 0 1 
## Unique values in partner : Yes No 
## Unique values in dependents : No Yes 
## Unique values in phoneservice : No Yes 
## Unique values in multiplelines : No phone service No Yes 
## Unique values in internetservice : DSL Fiber optic No 
## Unique values in onlinesecurity : No Yes No internet service 
## Unique values in onlinebackup : Yes No No internet service 
## Unique values in deviceprotection : No Yes No internet service 
## Unique values in techsupport : No Yes No internet service 
## Unique values in streamingtv : No Yes No internet service 
## Unique values in streamingmovies : No Yes No internet service 
## Unique values in contract : Month-to-month One year Two year 
## Unique values in paperlessbilling : Yes No 
## Unique values in paymentmethod : Electronic check Mailed check Bank transfer (automatic) Credit card (automatic) Mailed checkkk Electronic checkk 
## Unique values in churn : No Yes
# Replacing values in seniorcitizen column

subscriber_df$seniorcitizen[subscriber_df$seniorcitizen == 0] <- 'No'
subscriber_df$seniorcitizen[subscriber_df$seniorcitizen == 1] <- 'Yes'

# Recheck unique values
unique(subscriber_df$seniorcitizen)
## [1] "No"  "Yes"
# Replacing values in paymentmethod column

subscriber_df$paymentmethod[subscriber_df$paymentmethod == "Mailed checkkk"] <- "Mailed check"
subscriber_df$paymentmethod[subscriber_df$paymentmethod == "Electronic checkk"] <- "Electronic check"

# Recheck unique values
unique(subscriber_df$paymentmethod)
## [1] "Electronic check"          "Mailed check"             
## [3] "Bank transfer (automatic)" "Credit card (automatic)"
# Getting outliers based on totalcharges column

lower_bound <- quantile(subscriber_df$totalcharges, 0.025)
upper_bound <- quantile(subscriber_df$totalcharges, 0.975)
lower_bound
##   2.5% 
## 24.805
upper_bound
##   97.5% 
## 7565.06
# Getting the records without outlier values
non_outlier_values <- which(subscriber_df$totalcharges > lower_bound & subscriber_df$totalcharges < upper_bound)
#non_outlier_values

subscriber_df = subscriber_df[non_outlier_values,]
dim(subscriber_df)
## [1] 6651   21
3. Data Analysis.
Since our research question requires us to focus on how we can retain customers, the main variable to work with is the “churn” variable.

a) Research specific questions.
What percentage of customers from the provided data set churned?
# grouping by and perform a count/percentage operation

churn_agg_tbl <- subscriber_df %>% group_by(churn) %>% 
  #summarise(count=n(),
            #.groups = 'drop')
  summarise( percent = 100 * n() / nrow(subscriber_df) )
churn_agg_tbl
churn
<chr>
percent
<dbl>
No	73.20704
Yes	26.79296
2 rows
Majority of the customers 73.4% are still subscribed to MTN while on 26.6% have churned.

A comparison between male and female subscribers:
# group churned customers by gender and do a count 

gender_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(gender) %>% 
  summarise(count=n(),
            .groups = 'drop')

gender_agg_tbl
gender
<chr>
count
<int>
Female	903
Male	879
2 rows
There is little difference between the number of churned males and females; therefore gender is not a factor affecting the churn rate.

Investigate the distribution of churn by senior citizen:
# Filter out churned customers and compare numbers of senior vs non senior citizens

citizen_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(seniorcitizen) %>% 
   summarise(count=n(),
            .groups = 'drop')

citizen_agg_tbl
seniorcitizen
<chr>
count
<int>
No	1314
Yes	468
2 rows
The number of senior citizens churned forms a small percentage of total churned customers; therefore whether a customer is a senior citizen is not a factor affecting churn rate.

Investigate the distribution of churn by partner:
# Filter out churned customers and compare numbers of partner vs non partner

partner_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(partner) %>% 
   summarise(count=n(),
            .groups = 'drop')

partner_agg_tbl
partner
<chr>
count
<int>
No	1137
Yes	645
2 rows
The rate of churn is higher among non partners than it is for partners.

Investigate the distribution of churn by dependents:
# Filter out churned customers and compare numbers of customers with dependents vs those without

dependents_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(dependents) %>% 
   summarise(count=n(),
            .groups = 'drop')

dependents_agg_tbl
dependents
<chr>
count
<int>
No	1471
Yes	311
2 rows
The rate of churn is higher for customers with no dependents.

##Investigate the distribution of churn by phone service:
# Filter out churned customers and compare numbers of customers with phone service vs those without

phone_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(phoneservice) %>% 
   summarise(count=n(),
            .groups = 'drop')

phone_agg_tbl
phoneservice
<chr>
count
<int>
No	162
Yes	1620
2 rows
The rate of churn is higher among customers with phone service.

##Investigate the distribution of churn by multiple lines:
# Filter out churned customers and compare numbers of customers with multiple lines vs those without

lines_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(multiplelines) %>% 
   summarise(count=n(),
            .groups = 'drop')

lines_agg_tbl
multiplelines
<chr>
count
<int>
No	791
No phone service	162
Yes	829
3 rows
The rate of churn is lowest among customers with no phone service, that for those with one or multiple lines is comparable

##Investigate the distribution of churn by internet service:
# Filter out churned customers and compare numbers of customers with internet service vs those without

internet_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(internetservice) %>% 
   summarise(count=n(),
            .groups = 'drop')

internet_agg_tbl
internetservice
<chr>
count
<int>
DSL	450
Fiber optic	1277
No	55
3 rows
The rate of churn is highest among customers with fibre connection type of internet followed by those with DSL and lowest among those with no internet service at all

Investigate the distribution of churn by online security
# Filter out churned customers and compare numbers of customers with online security vs those without

security_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(onlinesecurity) %>% 
   summarise(count=n(),
            .groups = 'drop')

security_agg_tbl
onlinesecurity
<chr>
count
<int>
No	1442
No internet service	55
Yes	285
3 rows
Rate of churn is highest among those with no no online security

##Investigate the distribution of churn by online backup
# Filter out churned customers and compare numbers of customers with online backup vs those without

backup_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(onlinebackup) %>% 
   summarise(count=n(),
            .groups = 'drop')

backup_agg_tbl
onlinebackup
<chr>
count
<int>
No	1219
No internet service	55
Yes	508
3 rows
Rate of churn is highest among customers with no online backup.

##Investigate the distribution of churn by device protection
# Filter out churned customers and compare numbers of customers with device protection vs those without

device_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(deviceprotection) %>% 
   summarise(count=n(),
            .groups = 'drop')

device_agg_tbl
deviceprotection
<chr>
count
<int>
No	1197
No internet service	55
Yes	530
3 rows
Rate of churn is highest among customers with no device protection.

##Investigate the distribution of churn by tech support
# Filter out churned customers and compare numbers of customers with tech support vs those without

support_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(techsupport) %>% 
   summarise(count=n(),
            .groups = 'drop')

support_agg_tbl
techsupport
<chr>
count
<int>
No	1430
No internet service	55
Yes	297
3 rows
Rate of churn is highest among customers with no tech support.

##Investigate the distribution of churn by streaming tv
# Filter out churned customers and compare numbers of customers with streaming tv vs those without

tv_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(streamingtv) %>% 
   summarise(count=n(),
            .groups = 'drop')

tv_agg_tbl
streamingtv
<chr>
count
<int>
No	930
No internet service	55
Yes	797
3 rows
The rate of churn is comparable for customers with streaming tv and those without.

Investigate the distribution of churn by streaming movies:
# Filter out churned customers and compare numbers of customers with streaming movies vs those without

movies_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(streamingmovies) %>% 
   summarise(count=n(),
            .groups = 'drop')

movies_agg_tbl
streamingmovies
<chr>
count
<int>
No	926
No internet service	55
Yes	801
3 rows
The rate of churn is comparable for customers with streaming movies and those without.

##Investigate the distribution of churn by contract
# Filter out churned customers and compare numbers of customers with a contract vs those without

contract_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(contract) %>% 
   summarise(count=n(),
            .groups = 'drop')

contract_agg_tbl
contract
<chr>
count
<int>
Month-to-month	1582
One year	160
Two year	40
3 rows
Churn is highest among customers on a month-to-month contract.

##Investigate the distribution of churn by paperless billing
# Filter out churned customers and compare numbers of customers on paperless billing vs those without

paperless_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(paperlessbilling) %>% 
   summarise(count=n(),
            .groups = 'drop')

paperless_agg_tbl
paperlessbilling
<chr>
count
<int>
No	428
Yes	1354
2 rows
The rate of churn is highest among customers on paperless billing.

##Investigate the distribution of churn by payment method
# Filter out churned customers and compare numbers of customers on using the various payment methods

payment_agg_tbl <- filter(subscriber_df, churn == 'Yes') %>% group_by(paymentmethod) %>% 
   summarise(count=n(),
            .groups = 'drop')

payment_agg_tbl
paymentmethod
<chr>
count
<int>
Bank transfer (automatic)	245
Credit card (automatic)	226
Electronic check	1050
Mailed check	261
4 rows
The rate of churn is highest among custommers using electronic check paymment method.

b) General Analysis.
Comparing customers by gender:
# compare customers in the data set by gender

gender_tbl <- subscriber_df %>% group_by(gender) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
gender_tbl
gender
<chr>
count
<int>
Female	3303
Male	3348
2 rows
The number of males and females is almost similar.

Comparing senior vs non senior customers:
# compare senior vs non-senior customers

senior_tbl <- subscriber_df %>% group_by(seniorcitizen) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
senior_tbl
seniorcitizen
<chr>
count
<int>
No	5556
Yes	1095
2 rows
Very few of the customers are senior citizens

##Compare customers with dependents and those without:
# customers with dependents vs those without

dependents_tbl <- subscriber_df %>% group_by(dependents) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
dependents_tbl
dependents
<chr>
count
<int>
No	4660
Yes	1991
2 rows
most of the customers have no dependents.

Comparing customers by tenure:
# distribution by tenure

tenure_tbl <- subscriber_df %>% group_by(tenure) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
tenure_tbl
tenure
<dbl>
count
<int>
1	436
2	238
3	199
4	176
5	133
6	108
7	129
8	123
9	119
10	116
...
1-10 of 78 rows
Most customers have a tenure of below 10.

Comparing customers by phone service:
# distribution by phone service

phone_tbl <- subscriber_df %>% group_by(phoneservice) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
phone_tbl
phoneservice
<chr>
count
<int>
No	664
Yes	5987
2 rows
Majority of customers have phone service.

Comparing customers by whether the have multiple lines:
# distribution by number of lines

lines_tbl <- subscriber_df %>% group_by(multiplelines) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
lines_tbl
multiplelines
<chr>
count
<int>
No	3204
No phone service	664
Yes	2783
3 rows
The number of customers with multiple lines and those without are almost similar.

Comparing customers by internet service:
# distribution by internet service

internet_tbl <- subscriber_df %>% group_by(internetservice) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
internet_tbl
internetservice
<chr>
count
<int>
DSL	2392
Fiber optic	2909
No	1350
3 rows
Majority of customers have internet service; those with DSL are slightly less than those with Fiber optic.

Comparing customers by online security:
# distribution by online security

##security_tbl <- subscriber_df %>% group_by(onlinesecurity) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
security_tbl
onlinesecurity
<chr>
count
<int>
No	3431
No internet service	1350
Yes	1870
3 rows
More than half of all customers have no online security.

##Comparing customers by online backup:
# distribution by online backup

backup_tbl <- subscriber_df %>% group_by(onlinebackup) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
backup_tbl
onlinebackup
<chr>
count
<int>
No	3045
No internet service	1350
Yes	2256
3 rows
Majority of customers have no online backup, followed by those who have. Fewer still have no internet service at all.

Comparing customers by device protection:
# distribution by device protection

device_tbl <- subscriber_df %>% group_by(deviceprotection) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
device_tbl
deviceprotection
<chr>
count
<int>
No	3057
No internet service	1350
Yes	2244
3 rows
Slightly less than half of all customers have no device protection while having internet service.

Comparing customers by tech support:
# distribution by tech support

support_tbl <- subscriber_df %>% group_by(techsupport) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
support_tbl
techsupport
<chr>
count
<int>
No	3418
No internet service	1350
Yes	1883
3 rows
More than half of the customers have no tech support.

Comparing customers by streaming tv:
# distribution by streaming tv

tv_tbl <- subscriber_df %>% group_by(streamingtv) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
tv_tbl
streamingtv
<chr>
count
<int>
No	2777
No internet service	1350
Yes	2524
3 rows
The number of customers who do not stream tv is almost similar to those who do.

Comparing customers by streaming movies:
# distribution by streaming tv

movies_tbl <- subscriber_df %>% group_by(streamingmovies) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
movies_tbl
streamingmovies
<chr>
count
<int>
No	2752
No internet service	1350
Yes	2549
3 rows
The number of customers who do not stream movies is almost similar to those who do.

Comparing customers by contract:
# ditstribution by contract

contract_tbl <- subscriber_df %>% group_by(contract) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
contract_tbl
contract
<chr>
count
<int>
Month-to-month	3685
One year	1441
Two year	1525
3 rows
Majority of customers are on month-to-month contract while those on one and two year contracts are comparable.

Comparing customers by paperless billing:
# distribution by paperless billing

paperless_tbl <- subscriber_df %>% group_by(paperlessbilling) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
paperless_tbl
paperlessbilling
<chr>
count
<int>
No	2689
Yes	3962
2 rows
Paperless billing is the most preferred method of billing.

Comparing customers by payment method:
# distribution by payment method

payment_tbl <- subscriber_df %>% group_by(paymentmethod) %>% 
  summarise(count=n(),
            .groups = 'drop')
 
payment_tbl
paymentmethod
<chr>
count
<int>
Bank transfer (automatic)	1455
Credit card (automatic)	1437
Electronic check	2295
Mailed check	1464
4 rows
Electronic check is the most popular payment method, while Bank Transfer, Credit card and Mailed check have roughly the same pouplarity.

##4. Summary of findings:
##Based on the results of the analysis, the following conclusions were arrived at:
##
##There is no significant difference in churn rate between male and female subscribers. So this is not an area management needs to worry about. Majority of ##the customers are not senior citizens so this makes this dataset biased and hard to identify whether being a senior citizen affects churn rate. Not having ##a partner increases the likelihood of churning. Not having dependents increases the likelihood of churning. Not having online security increases the ##likelihood of churning. Not having online backup increases the likelihood of churning. Not having device protection increases the likelihood of churning. ##Customers on month-to-month contracts more likely to churn
##
##5. Recommendations:
##Focus more on meeting the needs of non-senior citizens. Focus more on having customers that have partners and/or dependents since these people are less ##likely to churn. Alternatively, management can come up with services specifically designed for customers without parters and/or dependents. This would ##require additional research. Focus on customers with Fiber optic internet service as their churn is high. Focus on providing online security for more ##customers. Focus on providing online backup for more customers. Focus on providing device protection for more customers. Focus on getting more customers ##on one or two year contracts.
##
##6. Challenging your solution:
##We had the right data and questions.