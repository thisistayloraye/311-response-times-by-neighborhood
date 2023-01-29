library(tidyverse)
library(lubridate)
library(ggplot2)
library(sjPlot)
library(stargazer)

getwd()
setwd("/Users/taylor/Desktop/phd/coursework/fall 2022/Stats Fall 2022/Final Project")

# read in the raw data
data311_raw <- read.csv("311_service_Requests_summer2022.csv")
zip_demographics_raw <- read.csv("Demographic_Statistics_By_Zip_Code.csv")

#-------------------------------------------------------------------------------

# CLEAN THE DATA: combine raw datasets on zipcode. 311 calls + demographic information of the neighborhood it occurred in.

# joining the data on zip code
joined_data_raw <- data311_raw %>% 
  left_join(zip_demographics_raw, by = c("Incident.Zip" = "JURISDICTION.NAME")) %>%
  mutate_all(na_if,"")

# select only the columns that you need for analysis; code empty cells as NAs; drop NAs
joined_data_clean <- joined_data_raw %>%
  select(Unique.Key, Created.Date, Closed.Date, Agency, Agency.Name, Complaint.Type, Incident.Zip, Location.Type, City, Borough,
         PERCENT.FEMALE, PERCENT.HISPANIC.LATINO, PERCENT.ASIAN.NON.HISPANIC, PERCENT.WHITE.NON.HISPANIC, PERCENT.BLACK.NON.HISPANIC, 
         PERCENT.OTHER.ETHNICITY, PERCENT.US.CITIZEN, PERCENT.RECEIVES.PUBLIC.ASSISTANCE) %>%
  mutate_all(na_if,"")

# clean the date columns to calculate response time
# first, keep character format, but convert to 24 hr clock
joined_data_clean$Created.Date.New <- format(strptime(joined_data_clean$Created.Date, "%m/%d/%Y %I:%M:%S %p"), format="%m/%d/%Y %H:%M:%S")
joined_data_clean$Closed.Date.New <- format(strptime(joined_data_clean$Closed.Date, "%m/%d/%Y %I:%M:%S %p"), format="%m/%d/%Y %H:%M:%S")
# now convert character to date-time object
joined_data_clean$Created.Date.New <- strptime(joined_data_clean$Created.Date.New, "%m/%d/%Y %H:%M:%S")    
joined_data_clean$Closed.Date.New <- strptime(joined_data_clean$Closed.Date.New , "%m/%d/%Y %H:%M:%S")
# calculate time to completion in seconds and minutes and add to new column
joined_data_clean$Case.Completion.Time <- as.duration(joined_data_clean$Closed.Date.New - joined_data_clean$Created.Date.New)
joined_data_clean$Case.Completion.Time.Seconds <- as.numeric(joined_data_clean$Case.Completion.Time)
joined_data_clean$Case.Completion.Time.Minutes <- joined_data_clean$Case.Completion.Time.Seconds / 60 #showing it in a weird format

# clean negative and zero value completion times from the dataset (confirm these cleaning decisions?)
joined_data_clean <- joined_data_clean %>%
  mutate(Case.Completion.Time.Seconds = ifelse(Case.Completion.Time.Seconds < 0 | Case.Completion.Time.Seconds == 0, NA, Case.Completion.Time.Seconds)) %>%
  drop_na(Case.Completion.Time.Seconds)

# sanity check: confirming that there aren't any NA values; if so, now ready for analysis 
sum(is.na(joined_data_clean$Case.Completion.Time.Seconds)) 

# add column to the data distinguishing Social Conflict vs. City Services complaints
joined_data_clean <- joined_data_clean %>%
  mutate(Complaint.Binary = ifelse(Complaint.Type == "Noise - Street/Sidewalk" | 
                                     Complaint.Type == "Noise - Residential" | 
                                     Complaint.Type == "Noise - Vehicle" | 
                                     Complaint.Type == "Noise - Commercial" | 
                                     Complaint.Type == "Illegal Parking" | 
                                     Complaint.Type == "Illegal Fireworks" | 
                                     Complaint.Type == "Noise - Park" | 
                                     Complaint.Type == "Taxi Complaint" |
                                     Complaint.Type == "Consumer Complaint" |
                                     Complaint.Type == "Blocked Driveway" |
                                     Complaint.Type == "Panhandling" |
                                     Complaint.Type == "Noise" |
                                     Complaint.Type == "Encampment" |
                                     Complaint.Type == "Traffic" |
                                     Complaint.Type == "Homeless Person Assistance" |
                                     Complaint.Type == "Drinking" |
                                     Complaint.Type == "Drug Activity" |
                                     Complaint.Type == "Graffiti" |
                                     Complaint.Type == "Noise - House of Worship" |
                                     Complaint.Type == "Urinating in Public" |
                                     Complaint.Type == "Animal-Abuse" |
                                     Complaint.Type == "For Hire Vehicle Complaint" |
                                     Complaint.Type == "Bike/Roller/Skate Chronic" |
                                     Complaint.Type == "Violation of Park Rules" |
                                     Complaint.Type == "Outdoor Dining" |
                                     Complaint.Type == "Commercial Disposal Complaint" |
                                     Complaint.Type == "Sanitation Worker or Vehicle Complaint" |
                                     Complaint.Type == "Unsanitary Animal Pvt Property" |
                                     Complaint.Type == "Disorderly Youth" |
                                     Complaint.Type == "Taxi Compliment" |
                                     Complaint.Type == "Retailer Complaint" |
                                     Complaint.Type == "Private School Vaccine Mandate Non-Compliance" |
                                     Complaint.Type == "Vaccine Mandate Non-Compliance",
                                   "Social Conflict", "City Services"))

View(joined_data_clean)

#-------------------------------------------------------------------------------

# SINGLE REGRESSION(S)

# linear regression between response time (continuous) and type of complaint (categorical) - would need to bucket complaint types into less catgegories
lm2 <- lm(Case.Completion.Time.Minutes ~ Complaint.Binary, data = joined_data_clean)
summary(lm2)
# RESULTS: Social Conflict complaints get responded to faster than City Service complaints by 16,000 minutes (11 days) on average...

# linear regression between response time (continuous) and percentage Black in the neighborhood (continuous)
lm3 <- lm(Case.Completion.Time.Minutes ~ PERCENT.BLACK.NON.HISPANIC, data = joined_data_clean)
summary(lm3)
# scatterplot of percent black vs. completion time
ggplot(joined_data_clean, aes(x = PERCENT.BLACK.NON.HISPANIC, y = Case.Completion.Time.Minutes, color = Complaint.Binary)) +
  geom_point() #+
  #geom_smooth(method = lm, color="darkred", fill="blue")
plot_model(lm3, type = "pred")
# RESULTS: for every 1% increase in the Black population in the neighborhood, response time goes down (faster) by 22 minutes on average; statistically significant

# linear regression between response time (continuous) and percentage hispanic latino in the neighborhood (continuous)
lm4 <- lm(Case.Completion.Time.Minutes ~ PERCENT.HISPANIC.LATINO, data = joined_data_clean)
summary(lm4)
# scatterplot of percent Latino vs. completion time
ggplot(joined_data_clean, aes(x = PERCENT.HISPANIC.LATINO, y = Case.Completion.Time.Minutes, color = Complaint.Binary)) +
  geom_point()
plot_model(lm4, type = "pred")

# RESULTS: for every 1% increase in the latino population in the neighborhood, response time goes up (slower) by 5 minutes on average; statistically significant

# linear regression between response time (continuous) and percentage white in the neighborhood  (continuous)
lm5 <- lm(Case.Completion.Time.Minutes ~ PERCENT.WHITE.NON.HISPANIC, data = joined_data_clean)
summary(lm5)
# scatterplot of percent white vs. completion time
ggplot(joined_data_clean, aes(x = PERCENT.WHITE.NON.HISPANIC, y = Case.Completion.Time.Minutes)) +
  geom_point()
plot_model(lm5, type = "pred")

# RESULTS: for every 1% increase in the white population in the neighborhood, response time goes up (slower) by 13 minutes on average; statistically significant

# linear regression between response time (continuous) and percentage US citizen in the neighborhood  (continuous)
lm6 <- lm(Case.Completion.Time.Minutes ~ PERCENT.US.CITIZEN, data = joined_data_clean)
summary(lm6)
# scatterplot of percent U.S. citizen vs. completion time
ggplot(joined_data_clean, aes(x = PERCENT.US.CITIZEN, y = Case.Completion.Time.Minutes)) +
  geom_point()
plot_model(lm6, type = "pred")
# RESULTS: for every 1% increase in U.S. citizens in the neighborhood, response time goes down (gets faster) by 3 minutes; statistically significant

# linear regression between response time (continuous) and government agency that responded (categorical)
lm7 <- lm(Case.Completion.Time.Minutes ~ as.factor(Agency.Name), data = joined_data_clean)
summary(lm7)
# RESULTS: reference agency is Department of Buildings; want to make the reference category the NYPD. NYPD is fastest; Taxi/Limo slowest.

#-------------------------------------------------------------------------------

# MULTIPLE REGRESSION ON YOUR DATA (HW7 Q3)
## include both continuous and categorical independent variables; interpret results
## Make sure you comment your code (describing your variables or data cleaning)
## so that the reader understands why you made the decisions you made
## dependent variable = response time
## independent variables = neighborhood (cat), complaint type (cat), location type (cat), race

m1 <- lm(Case.Completion.Time.Minutes ~ as.factor(Complaint.Binary) + PERCENT.BLACK.NON.HISPANIC, data = joined_data_clean)
summary(m1)
stargazer(m1, type = "text")
# RESULTS: holding Black population % constant, Social Conflict was responded to faster on average by 16,000 minutes
# RESULTS: for the same complaint types, a 1 unit increase in the Black population in a neighborhood causes the case completion time to go up by 6.8 minutes on average (longer)

m4 <- lm(Case.Completion.Time.Minutes ~ as.factor(Complaint.Binary) + PERCENT.BLACK.NON.HISPANIC + as.factor(Agency.Name), data = joined_data_clean)
summary(m4)
stargazer(m4, type = "text")
# RESULTS: for the same types of complaints and the same responding agency, response times got longer by 6 minutes with a 1% increase in Black population in the neighborhood

m5 <- lm(Case.Completion.Time.Minutes ~ as.factor(Complaint.Binary) + PERCENT.HISPANIC.LATINO + as.factor(Agency.Name), data = joined_data_clean)
summary(m5)
stargazer(m5, type = "text")
# RESULTS: for the same types of complaints and the same responding agency, response times got 11 minutes longer with a 1% increase in Latino population in the neighborhood

m6 <- lm(Case.Completion.Time.Minutes ~ as.factor(Complaint.Binary) + PERCENT.WHITE.NON.HISPANIC + as.factor(Agency.Name), data = joined_data_clean)
summary(m6)
stargazer(m6, type = "text")
# RESULTS: for the same types of complaints and the same responding agency, response times got 2 minutes faster with a 1% increase in White population in the neighborhood


#---

m2 <- lm(Case.Completion.Time.Minutes ~ as.factor(Borough) + PERCENT.BLACK.NON.HISPANIC, data = joined_data_clean)
summary(m2)
# RESULTS: reference Borough is the Bronx; assuming the Black population percentage in the neighborhood, Brooklyn takes the longest to respond to; Queens is the fastest
# RESULTS: holding the Borough consistent, a 1% increase in the Black population causes the response time to go down by 23 minutes (faster)

m3 <- lm(Case.Completion.Time ~ as.factor(Agency.Name) + as.factor(Borough) + PERCENT.BLACK.NON.HISPANIC, data = joined_data_clean)
summary(m3)
stargazer(m3, type = "text")

# RESULTS: reference Borough is the Bronx; assuming the same Black population makeup and responding agency: Queens is fastest and Manhattan is slowest
# RESULTS: reference Agency is Department of Buildings; assuming same Borough and percentage of Black population, NYPD is the fastest respondent 
# RESULTS: holding the responding Agency and Borough constant, a 1% increase in the Black population correlates with the response time getting faster on average by 12 minutes

#-------------------------------------------------------------------------------

# MULTIPLE REGRESSION + INTERACTION ON YOUR DATA (Q3 HW8)
## include both continuous and categorical independent variables; interpret results
## Make sure you comment your code (describing your variables or data cleaning)
## so that the reader understands why you made the decisions you made
## dependent variable = response time
## independent variables = neighborhood (cat), complaint type (cat), location type (cat), race

i1 <- lm(Case.Completion.Time.Minutes ~ PERCENT.BLACK.NON.HISPANIC*Complaint.Binary, data = joined_data_clean)
summary(i1)
stargazer(i1, type = "text")
plot_model(i1, type = "pred", terms = c("PERCENT.BLACK.NON.HISPANIC", "Complaint.Binary"))
# RESULTS: in measuring case completion time, whether or not the complaint was social conflict was largely dependent on the Black population percentage of the neighborhood (?)

i4 <- lm(Case.Completion.Time.Minutes ~ PERCENT.HISPANIC.LATINO*Complaint.Binary, data = joined_data_clean)
summary(i4)
stargazer(i4, type = "text")
plot_model(i4, type = "pred", terms = c("PERCENT.HISPANIC.LATINO", "Complaint.Binary"))

i5 <- lm(Case.Completion.Time.Minutes ~ PERCENT.WHITE.NON.HISPANIC*Complaint.Binary, data = joined_data_clean)
summary(i5)
stargazer(i5, type = "text")
plot_model(i5, type = "pred", terms = c("PERCENT.WHITE.NON.HISPANIC", "Complaint.Binary"))

i2 <- lm(Case.Completion.Time.Minutes ~ PERCENT.BLACK.NON.HISPANIC*Agency.Name, data = joined_data_clean)
summary(i2)
stargazer(i2, type = "text")
plot_model(i2, type = "pred", terms = c("PERCENT.BLACK.NON.HISPANIC", "Agency.Name"))
# RESULTS: Does the racial makeup of the neighborhood affect the type of complaint (Social Conflict vs. City Services)? Yes.


#-----
i3 <- lm(Case.Completion.Time.Minutes ~ Complaint.Binary*Agency.Name, data = joined_data_clean)
summary(i3)
stargazer(i3, type = "text")
plot_model(i3, type = "pred", terms = c("Complaint.Binary", "Agency.Name"))
# RESULTS: Does the type of complaint (conflict vs. services) determine what agency responds? Definitely.
