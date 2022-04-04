# Jacob Unterbrink & Francis Oyebanji
# U. of Oklahoma
# DSA 5103, HW 4
# Data Wrangling - Aimes IA Housing Data

library(tidyverse)
library(knitr)
library(car)
library(VIM)
library(mice)
library(EnvStats)

#  ------------------
# |                  |
# |    Question 1    |
# |                  |
#  ------------------

housingData <- read.csv("housingData.csv")

# a.)
# Apply the alterations:
housingData <- housingData %>%
  dplyr::mutate(age = YrSold - YearBuilt,
                ageSinceRemodel = YrSold - YearRemodAdd,
                ageofGarage = YrSold - GarageYrBlt)

# b.)
# Select only the numeric variables:
housingNumeric <- as_tibble(dplyr::select(housingData, where(is.numeric)))

# c.)
# Select only the factors:
housingFactor <- as_tibble(dplyr::select(housingData, where(is.factor)))

# d.)
# Use glimpse() to look at the new tibbles:
glimpse(housingNumeric)
glimpse(housingFactor)

# e.)
# Explain the below code given in HW:
Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}
Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

# f.)
# Code given in HW:
myNumericSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}

# g.)
# Use the dplyr::summarise command and with myNumericSummary to apply to 
# every variable in the numeric tibble. Should use summarize with the 
# across() syntax.
numericSummary <- housingNumeric %>%
  summarise(across(everything(), myNumericSummary))
glimpse(numericSummary)


# h.)
# Given in HW:
numericSummary <-cbind(
  stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"),
  numericSummary)
glimpse(numericSummary)

# i.)
# Given in HW:
numericSummaryFinal <- numericSummary %>%
  pivot_longer("Id":"ageofGarage", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*missing/n,
         unique_pct = 100*unique/n) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())

options(digits=3)
options(scipen=99)
numericSummaryFinal %>% kable()

# j.)
# Create report for non-numeric data:
# The below finds the mode info (given in HW):
getmodes <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}
getmodesCnt <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl)) #1st mode freq
  }
  else if (type==2) {
    return (max(tbl[-m1])) #2nd mode freq
  }
  else if (type==-1) {
    return (min(tbl)) #least common freq
  }
  else {
    stop("Invalid type selected")
  }
}

# The below code generates a categorical summary function similar
# to the previous numeric summary function:
myCatSummary <- function(x){
  c(length(x), sum(is.na(x)), (sum(is.na(x))/10), n_distinct(x), 
    n_distinct(x)/10, getmodesCnt(x, type = 1)/getmodesCnt(x, type = 2),
    getmodes(x, type = 1), getmodesCnt(x, type = 1),
    getmodes(x, type = 2), getmodesCnt(x, type = 2),
    getmodes(x, type = -1), getmodesCnt(x, type = -1)
    )
}

# Apply the myCatSummary to every variable in the categorical dataset:
factorSummary <- housingFactor %>%
  summarize(across(everything(), myCatSummary))
glimpse(factorSummary)

# cbind the appropriate names to the set:
factorSummary <-cbind(
  stat=c('n','missing','missing_pct','unique','unique_pct','freq_ratio','1st mode',
         '1st mode freq','2nd mode','2nd mode freq','least common','least common freq'),
  factorSummary)
glimpse(factorSummary)

# Pivot the data to be more presentable:
factorSummaryFinal <- factorSummary %>%
  pivot_longer("MSZoning":"SaleType", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)

# Apply a few last changes:
options(digits=3)
options(scipen=99)
factorSummaryFinal %>% kable()

#  ------------------
# |                  |
# |    Question 2    |
# |                  |
#  ------------------

# a.)
# By visual inspection identify two variables that are highly skewed.
# Then use a transform method to make these variables more normally
# distributed:

# MasVnrArea, WoodDeckSF beofre transformation:
hist(housingNumeric$MasVnrArea, col = 'light green',
     xlab = 'Masonry Veneer Area', main = 'Histogram of Masonry Veneer Area')
boxplot(housingNumeric$MasVnrArea, col = 'light green',
        main='Boxplot of Masonry Veneer Area')

hist(housingNumeric$SalePrice, col = 'light blue',
     xlab = 'Sales Price', main = 'Histogram of Sales Price')
boxplot(housingNumeric$SalePrice, col = 'light blue',
        main = 'Boxplot of Sales Price')

# Running transformations (ladder of powers for MasVnrArea and boxcox
# for SalePrice):
symbox(housingNumeric$MasVnrArea, powers=c(3,2,1,0,-0.5,-1,-2))
logMasVnrArea <- log(housingNumeric$MasVnrArea)

boxcox(housingNumeric$SalePrice, optimize = TRUE, lambda=c(-3,3))
boxcoxSalePrice <- housingNumeric$SalePrice**(-0.09565572)/(-0.09565572)

# Plots after the transformations:
hist(logMasVnrArea, col = 'light green', xlab = 'log(Masonry Veneer Area)',
     main = 'Histogram of log(Masonry Veneer Area)')
hist(boxcoxSalePrice, col = 'light blue', xlab = 'BoxCox - Sales Price',
     main = 'Histogram of BoxCox - Sales Price')


# b - i.)
# LotFrontage has missing values impute it via mean, reg on error, pred
# mean matching. Then show visual dipictions for how the data were
# transformed:
byMean <- housingNumeric
missing <- is.na(byMean$LotFrontage)
meanLot <- mean(byMean$LotFrontage, na.rm = T)
byMean$LotFrontage[missing] <- meanLot

# b - ii.)
# Imputation by regression:
byReg <- housingNumeric
RegMissing <- is.na(byReg$LotFrontage)
RegMod <- lm(LotFrontage ~ ., data = housingNumeric)
RegSum <- summary(RegMod)
RegC <- RegSum[[4]]
RegF <- RegSum[[6]]
byReg$LotFrontage[RegMissing] <- (RegC[2] + 
                                    RegC[4]*byReg$LotFrontage[RegMissing])
# b - iii.)
# Imputation by MICE:
byMice <- housingNumeric
miceMissing <- is.na(byMice$LotFrontage)
byMice$missing <- miceMissing
byMice$LotFrontage[miceMissing] <- mice.impute.pmm(byMice$LotFrontage,
                                                   !byMice$missing,
                                                   byMice$SalePrice)

# b - iv.)
# Create visual for the transformed data (hist or density plots):
ggplot(byMean) + geom_histogram(mapping = aes(x = LotFrontage), 
                                fill = 'light green') + xlab('Lot Frontage') +
  ylab('Count') + ggtitle('Lot Frontage (Imputation by Mean)')

ggplot(byReg) + geom_histogram(mapping = aes(x = LotFrontage), 
                               fill = 'light blue') +
  xlab('Lot Frontage') + ylab('Count') + 
  ggtitle('Lot Frontage (Imputation by Regression)')

ggplot(byMice) + geom_histogram(mapping = aes(x = LotFrontage), 
                                fill = 'purple') + xlab('Lot Frontage') +
  ylab('Count') + ggtitle('Lot Frontage (Imputation by MICE)')

# c.)
# Use forcats to colapse the categorical variable Exterior1st into
# 5 groups (4 most freq and 1 other):
sort(table(housingData$Exterior1st), decreasing = T)
housingData$Exterior1st <- forcats::fct_collapse(housingData$Exterior1st,
                                                 VinylSd = c("VinylSd"),
                                                 HdBoard = c("HdBoard"),
                                                 MetalSd = c("MetalSd"),
                                                 Wd_Sdng = c("Wd Sdng"),
                                                 Other = c("Plywood",
                                                           "other",
                                                           "BrkFace",
                                                           "CemntBd"))
table(housingData$Exterior1st)

# d - i.)
# Average sales price by neighborhood:
housingData %>%
  group_by(Neighborhood) %>% 
  summarise(MEAN = mean(SalePrice))

# d - ii.)
# ggplot of neighborhood vs sales price:
ggplot(housingData) + 
  geom_boxplot(mapping = aes(x = Neighborhood, y = SalePrice),
               alpha = .5, fill = 'grey') +
  xlab("Neighborhood") +
  ylab("Sales Price") +
  ggtitle("Neighborhood vs. Sales Price")

# d - iii.)
# Re-order the neighborhoods by sales price:
Reordered <- fct_reorder(housingData$Neighborhood, housingData$SalePrice, 
                         median, .desc = T)


# d - iv.)
# Reconstruct the ggplot with the ordered neighborhoods:
ggplot(housingData) + 
  geom_boxplot(mapping = aes(x = Reordered, y = SalePrice),
               alpha = .5, fill = 'grey') +
  xlab("Neighborhood") +
  ylab("Sales Price") +
  ggtitle("Neighborhood vs. Sales Price")

