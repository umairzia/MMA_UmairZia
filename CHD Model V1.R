library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(pROC)
library(mice)

fh <- read.csv("D:\\MMA 867\\Final project\\archive\\framingham.csv", header=TRUE, sep = ",")
str(fh)
head(fh)

md.pattern(fh)

colSums(sapply(fh, is.na))

# Missing values - education, cigsPerDay, BPMeds, totChol, BMI, heartRate, glucose
# education
summary(factor(fh$education))
# cigsPerDay
summary(factor(fh$cigsPerDay))
# BPMeds
summary(factor(fh$BPMeds))
# totChol
summary(factor(fh$totChol))
# BMI
summary(factor(fh$BMI))
# heartRate
summary(factor(fh$heartRate))
# glucose
summary(factor(fh$glucose))


# convert to factor might be a good idea
my_graph0 = ggplot(fh, aes(x = education)) + geom_histogram()
my_graph0

my_graph1 = ggplot(fh, aes(x = currentSmoker)) + geom_histogram()
my_graph1

my_graph2 = ggplot(fh, aes(x = prevalentStroke)) + geom_histogram()
my_graph2

my_graph3 = ggplot(fh, aes(x = prevalentHyp)) + geom_histogram()
my_graph3

my_graph4 = ggplot(fh, aes(x = diabetes)) + geom_histogram()
my_graph4

my_graph5 = ggplot(fh, aes(x = cigsPerDay)) + geom_histogram()
my_graph5

my_graph6 = ggplot(fh, aes(x = male)) + geom_histogram()
my_graph6

# Feature Engineering 

# BMI and currentSmoker
fh$currentSmoker_BMI = fh$currentSmoker*fh$BMI

# male and BMI 
fh$male_BMI = fh$male*fh$BMI

# BMI and diabetes
fh$diabetes_BMI = fh$diabetes*fh$BMI

# currentSmoker and Hypertension 
fh$currentSmoker_hyp = fh$currentSmoker*fh$prevalentHyp

# cigsPerDay and heartRate
fh$cigsperday_heartrate = fh$cigsPerDay*fh$heartRate

# sysBP and age
fh$sysBP_age = fh$sysBP*fh$age

# diaBP and prevalentHyp
fh$prevalentHyp_diaBP = fh$prevalentHyp*fh$diaBP

# sysBP and prevalentHyp
fh$prevalentHyp_sysBP = fh$prevalentHyp*fh$sysBP

# age and prevalentHyp
fh$prevalentHyp_age = fh$prevalentHyp*fh$age

colSums(sapply(fh, is.na))

# NA values for numerical variables 
# cigsPerDay 
ggplot(data = fh,aes(x=cigsPerDay)) + geom_histogram()
fh$cigsPerDay = ifelse(is.na(fh$cigsPerDay), 0 ,fh$cigsPerDay)
ggplot(data = fh,aes(x=cigsPerDay)) + geom_histogram()

# Candidates for conversion to categorical:
# education, currentSmoker, prevalentStroke, prevalentHyp, diabetes, male

# education
ggplot(data = fh,aes(x=education))+geom_bar()
fh$education = factor(fh$education)
fh$education = ifelse(is.na(fh$education), 4 ,fh$education)
ggplot(data = fh,aes(x=education))+geom_bar()

# currentSmoker - no NA values
ggplot(data = fh,aes(x=currentSmoker))+geom_bar()
fh$currentSmoker = factor(fh$currentSmoker)
ggplot(data = fh,aes(x=currentSmoker))+geom_bar()

# prevalentStroke- no NA values
ggplot(data = fh,aes(x=prevalentStroke))+geom_bar()
fh$prevalentStroke = factor(fh$prevalentStroke)
ggplot(data = fh,aes(x=prevalentStroke))+geom_bar()

# prevalentHyp- no NA values
ggplot(data = fh,aes(x=prevalentHyp))+geom_bar()
fh$prevalentHyp = factor(fh$prevalentHyp)
ggplot(data = fh,aes(x=prevalentHyp))+geom_bar()

# diabetes- no NA values
ggplot(data = fh,aes(x=diabetes))+geom_bar()
fh$diabetes = factor(fh$diabetes)
ggplot(data = fh,aes(x=diabetes))+geom_bar()

# male- no NA values
ggplot(data = fh,aes(x=male))+geom_bar()
fh$male = factor(fh$male)
ggplot(data = fh,aes(x=male))+geom_bar()

# New features:
ggplot(data = fh,aes(x=currentSmoker_BMI)) + geom_histogram()
fh$currentSmoker_BMI = ifelse(is.na(fh$currentSmoker_BMI), 0 ,fh$currentSmoker_BMI)
ggplot(data = fh,aes(x=currentSmoker_BMI)) + geom_histogram()

ggplot(data = fh,aes(x=male_BMI)) + geom_histogram()
fh$male_BMI = ifelse(is.na(fh$male_BMI), 0 ,fh$male_BMI)
ggplot(data = fh,aes(x=male_BMI)) + geom_histogram()

ggplot(data = fh,aes(x=diabetes_BMI)) + geom_histogram()
fh$diabetes_BMI = ifelse(is.na(fh$diabetes_BMI), 0 ,fh$diabetes_BMI)
ggplot(data = fh,aes(x=diabetes_BMI)) + geom_histogram()

ggplot(data = fh,aes(x=cigsperday_heartrate)) + geom_histogram()
fh$cigsperday_heartrate = ifelse(is.na(fh$cigsperday_heartrate), 0 ,fh$cigsperday_heartrate)
ggplot(data = fh,aes(x=cigsperday_heartrate)) + geom_histogram()

colSums(sapply(fh, is.na))

str(fh)

# Split data into Train & Test
set.seed(111)
split1<- sample(c(rep(0, 0.7 * nrow(fh)), rep(1, 0.3 * nrow(fh))))
table(split1)

train <- fh[split1 == 0, ]   
test <- fh[split1== 1, ]   

str(fh)
Logmodel_v1 = glm(TenYearCHD ~ . -BMI -sysBP -prevalentHyp -age -male_BMI -education, data=train, family=binomial)
summary(Logmodel_v1)

test_prob = predict(Logmodel_v1, newdata = test, type = "response")
test_roc = roc(test$TenYearCHD ~ test_prob, plot = TRUE, print.auc = TRUE)
# AUC = 0.725


# Corrplot
#Numeric Dataset
fh_Num <- fh %>% select_if(is.numeric)
# correlations on numeric variables

correlations <- cor(fh_Num)
corrplot(correlations, method = 'color')

