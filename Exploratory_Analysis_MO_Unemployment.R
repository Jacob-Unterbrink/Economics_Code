# Jacob Unterbrink
# Metadata updated Feb 3, 2018 (data.gov)
# Final project: Missouri unemployment analysis

#    ------------------
#   |                  |
#   |    CLEAN DATA    |
#   |                  |
#    ------------------

# ------------
# Load packages, data and keep relevant features:

library(pastecs)
library(psych)
library(car)
library(Hmisc)
library(tidyverse)
library(nortest)

data <- read.csv("Missouri_Unemployment_Rates_For_Various_Regions.csv")

str(data)
anyNA(data)
data$RegionGeometry = NULL
data$RegionCentroid = NULL

subdat <- data %>% filter(Year <= 2012)
county_data <- subdat[subdat$Area.Type.Description=="County",]

# ------------
# There are no missing values, years after 2012 were excluded because 
# balanced data were desired .There are 114 counties and 1 independent 
# city (St. Louis). This is the dataset that all future sets will be 
# subsetted from:

county_months <- county_data[county_data$Period.Type=="Monthly",]
str(county_months)
summary(county_months)

# ------------
# Look at St. Louis, Kansas City, and Columbia by subsetting the
# associated counties:

boone_data <- county_months[county_months$Area.Name=="BOONE COUNTY",]
kc_data <- county_months %>% filter(Area.Name == "JACKSON COUNTY" |
                                    Area.Name == "CLAY COUNTY" |
                                    Area.Name == "PLATTE COUNTY" |
                                    Area.Name == "CASS COUNTY")
stl_data <- county_months %>% filter(Area.Name == "ST. LOUIS COUNTY"|
                                     Area.Name == "ST. LOUIS CITY COUNTY")


#    ----------------
#   |                |
#   |    Analysis    |
#   |                |
#    ----------------

# ------------
# Descriptive statistics:

str(county_months)
str(boone_data)
str(stl_data)
str(kc_data)

summary(county_months)
summary(boone_data)
summary(stl_data)
summary(kc_data)

describe(county_months$Unemployment.Rate)
describe(boone_data$Unemployment.Rate)
describe(stl_data$Unemployment.Rate)
describe(kc_data$Unemployment.Rate)

gen_unemp = county_months[order(county_months$Unemployment.Rate),]
head(gen_unemp)         # <-- Note counties w/ lowest unemp for q1.
tail(gen_unemp)         # <-- Note counties w/ highest unemp for q1.

# ------------
# Interestingly Boone County (Columbia) has some of the lowest
# unemployment in the state.
# Counties with some of the highest weren't located in the three
# cities considered.

boone_unemp = boone_data[order(boone_data$Unemployment.Rate),]
head(boone_unemp)
tail(boone_unemp)

stl_unemp = stl_data[order(stl_data$Unemployment.Rate),]
head(stl_unemp)
tail(stl_unemp)

kc_unemp = kc_data[order(kc_data$Unemployment.Rate),]
head(kc_unemp)
tail(kc_unemp)

# ------------
# Lowest/highest unemployment rates for each city:
# Columbia: (3.2, 7.3)
# STL: (4.7, 13.9)
# KC: (3.7, 11.2)
# The highest rates can possibly be attributed to the 2007 recession
# and some lagged effects

# ------------
# Is the business quarter a significant predictor of unemp?

county_months$Quarter = 1
for (i in 1:nrow(county_months)){
  if (county_months[i,]$Period > 3){
    county_months[i,]$Quarter = 2
  }
  if (county_months[i,]$Period > 6){
    county_months[i,]$Quarter = 3
  }
  if (county_months[i,]$Period > 9){
    county_months[i,]$Quarter = 4
  }
}


lm_1 = lm(Unemployment.Rate ~ factor(Quarter), data = county_months)
summary(lm_1)           # <-- Consider adding a plot of the lm.
hist(lm_1$residuals)    # <-- A linear model fits the data well.
plot(Unemployment.Rate ~ Quarter, data = county_months)

# ------------
# It appears that the business quarter is a significant predictor of 
# unemployment, and the unemployment rate was highest in the first quarter

# ------------
# Differences of unemployment between STL, KC, COMO, & MO?

boone_data <- county_months[county_months$Area.Name=="BOONE COUNTY",]
kc_data <- county_months %>% filter(Area.Name == "JACKSON COUNTY"|
                                    Area.Name == "CLAY COUNTY"|
                                    Area.Name == "PLATTE COUNTY"|
                                    Area.Name == "CASS COUNTY")
stl_data <- county_months %>% filter(Area.Name == "ST. LOUIS COUNTY"|
                                     Area.Name == "ST. LOUIS CITY COUNTY")

# ------------
# Check the normality of the data:

ad.test(county_months$Unemployment.Rate)       # <-- Not a normal dist.
shapiro.test(stl_data$Unemployment.Rate)       # <-- Not a normal dist.
shapiro.test(kc_data$Unemployment.Rate)        # <-- Not a normal dist.
shapiro.test(boone_data$Unemployment.Rate)     # <-- Not a normal dist.

lm_stl <- lm(Unemployment.Rate ~ factor(Year) + Laborforce + factor(Quarter), data = stl_data)
lm_kc <- lm(Unemployment.Rate ~ factor(Year) + Laborforce + factor(Quarter), data = kc_data)
lm_boone <- lm(Unemployment.Rate ~ factor(Year) + Laborforce + factor(Quarter), data = boone_data)
lm_agg <- lm(Unemployment.Rate ~ factor(Year) + Laborforce + factor(Quarter), data = county_months)

summary(lm_stl)
summary(lm_kc)
summary(lm_boone)
summary(lm_agg)

lm_stl_1 <- lm(Unemployment.Rate ~ factor(Year), data = stl_data)
lm_kc_1 <- lm(Unemployment.Rate ~ factor(Year), data = kc_data)
lm_agg_1 <- lm(Unemployment.Rate ~ factor(Year), data = county_months) 
lm_boone_1 <- lm(Unemployment.Rate ~ factor(Year), data = boone_data)

plot(lm_stl_1)   # <-- poor fit
plot(lm_kc_1)    # <-- poor fit
plot(lm_agg_1)   # <-- poor fit
plot(lm_boone_1) # <-- poor fit, but interestingly the QQ plot seems better

mean(county_months$Unemployment.Rate)
mean(stl_data$Unemployment.Rate)
mean(kc_data$Unemployment.Rate)
mean(boone_data$Unemployment.Rate)

# ------------
# Unsurprisingly, unemployment is highest in st. louis on average.
# However, I didn't expect columbia to outperform KC

# ------------
# Density plot of unemployment for each city:

plot(density(county_months$Unemployment.Rate), col = "Green", type = "l", ylim = c(0.00, 0.35),
     xlab = "Unemployment Rate",
     ylab = "Density",
     main = "Density Plot of Missouri Unemployment Rates (2008 - 2012)")
lines(density(stl_data$Unemployment.Rate), col = "Red")
lines(density(kc_data$Unemployment.Rate), col = "Blue")
lines(density(boone_data$Unemployment.Rate), col = "Orange")
legend("right", c("State Aggregate", "Columbia", "Kansas City", "St. Louis"),
       fill = c("Green", "Orange", "Blue", "Red"))

# ------------
# What is the change in labor force between major areas & is unemployment 
# sig effected by it?

# the following finds the change in the labor force from year to year:
labor_change <- function(data_frame){ # <-- practice writing functions
  
  Labor_Array = numeric(5)
  Labor_Index = 1
  for (i in 2008:2012){
    mean_i = mean(data_frame[data_frame$Year==i,]$Laborforce)
    Labor_Array[Labor_Index] = mean_i
    Labor_Index = Labor_Index + 1
  }
  
  Change_Array = numeric(4)
  Change_Index = 1
  for (i in 2:length(Labor_Array)){
    Change_i = ((Labor_Array[i] - Labor_Array[i-1])/Labor_Array[i-1])*100
    Change_Array[Change_Index] = Change_i
    Change_Index = Change_Index + 1
  }
  
  Change_Agg = sum(Change_Array)/4
  Change_List = append(Change_Array, Change_Agg)
  
  return(Change_List)
}

# The following finds the change in unemployment rate from year to year:
emp_change <- function(data_frame){
  
  Emp_Array = numeric(5)
  Emp_Index = 1
  for (i in 2008:2012){
    emp_i = mean(data_frame[data_frame$Year==i,]$Unemployment.Rate)
    Emp_Array[Emp_Index] = emp_i
    Emp_Index = Emp_Index + 1
  }
  
  Change_Array = numeric(4)
  Change_Index = 1
  for (i in 2:length(Emp_Array)){
    Change_i = ((Emp_Array[i] - Emp_Array[i-1])/Emp_Array[i-1])*100
    Change_Array[Change_Index] = Change_i
    Change_Index = Change_Index + 1
  }
  
  Change_Agg = sum(Change_Array)/4
  Change_List = append(Change_Array, Change_Agg)
  
  return(Change_List)
}

Missouri_Labor_Change <- c(labor_change(county_months))
Missouri_Unemp_Change <- c(emp_change(county_months))
Stl_Labor_change <- c(labor_change(stl_data))
Stl_Unemp_Change <- c(emp_change(stl_data))
KC_Labor_Change <- c(labor_change(kc_data))
KC_Unemp_Change <- c(emp_change(kc_data))
Columbia_Labor_Change <- c(labor_change(boone_data))
Columbia_Unemp_Change <- c(emp_change(boone_data))

# ------------
# The following table shows the change in laborforce and unemployment for
# the three cities and state aggregate:

Q3_Mat <- rbind(Missouri_Labor_Change,Missouri_Unemp_Change,Stl_Labor_change,Stl_Unemp_Change,
               KC_Labor_Change,KC_Unemp_Change,Columbia_Labor_Change,Columbia_Unemp_Change)
rownames(Q3_Mat) <- c("MO Labor Change   (in %)", "MO Unemp Change   (in %)", "STL Labor Change  (in %)",
                     "STL Unemp Change  (in %)", "KC Labor Change   (in %)", "KC Unemp Change   (in %)", 
                     "COMO Labor Change (in %)", "COMO Unemp Change (in %)")
colnames(Q3_Mat) <- c("'08 - '09", "'09 - '10", "'10 - '11", "'11 - '12", " Mean Change")
Q3_Mat

# ------------
# Is laborforce a good predictor of unemployment?

Q3_Mod_1 <- lm(Unemployment.Rate ~ Laborforce, data = county_months)
Q3_Mod_2 <- lm(Unemployment.Rate ~ Laborforce, data = stl_data)
Q3_Mod_3 <- lm(Unemployment.Rate ~ Laborforce, data = kc_data)
Q3_Mod_4 <- lm(Unemployment.Rate ~ Laborforce, data = boone_data)

summary(Q3_Mod_1) # <-- Laborforce is NOt a sig predictor
summary(Q3_Mod_2) # <-- Laborforce is a sig predictor
summary(Q3_Mod_3) # <-- Laborforce is a sig predictor
summary(Q3_Mod_4) # <-- Laborfoce is NOT a sig predictor
