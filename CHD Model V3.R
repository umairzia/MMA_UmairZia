library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(pROC)
library(mice)

fh <- read.csv("D:\\MMA 867\\Final project\\archive\\framingham.csv", header=TRUE, sep = ",")
str(fh)
head(fh)

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

my_graph5 = ggplot(fh, aes(x = male)) + geom_histogram()
my_graph5

md.pattern(fh)

colSums(sapply(fh, is.na))

# Missing values - education, cigsPerDay, BPMeds, totChol, BMI, heartRate, glucose
# education
summary(factor(fh$education))
ggplot(data = fh,aes(x=education))+geom_bar()
fh$education = factor(fh$education)
fh$education = ifelse(is.na(fh$education), 4 ,fh$education)
summary(factor(fh$education))

# cigsPerDay - replace NA with zero, patients may have seen the question and written NA to indicate
# they are in fact not smokers at all, hence NA made sense to them in their mind
summary(factor(fh$cigsPerDay))
#ggplot(data = fh,aes(x=cigsPerDay)) + geom_histogram()
fh$cigsPerDay = ifelse(is.na(fh$cigsPerDay), 0 ,fh$cigsPerDay) # equivalent to replace with mode
summary(factor(fh$cigsPerDay))

# BPMeds
summary(factor(fh$BPMeds))
fh$BPMeds = ifelse(is.na(fh$BPMeds), 0 ,fh$BPMeds) # equivalent to replace with mode
summary(factor(fh$BPMeds))

# totChol
summary(factor(fh$totChol))
val <- unique(fh$totChol[!is.na(fh$totChol)])                   
my_mode <- val[which.max(tabulate(match(fh$totChol, val)))] 
fh$totChol = ifelse(is.na(fh$totChol), 240 ,fh$totChol) # Impute the mode since its a categorical var
summary(factor(fh$totChol))

# BMI - assume response mechanism is MCAR
summary(factor(fh$BMI))
fh$BMI = ifelse(is.na(fh$BMI), mean(fh$BMI, na.rm = TRUE) ,fh$BMI)
summary(factor(fh$BMI))

# heartRate
summary(factor(fh$heartRate))
val1 <- unique(fh$heartRate[!is.na(fh$heartRate)])                   
my_mode1 <- val1[which.max(tabulate(match(fh$heartRate, val1)))] 
fh$heartRate = ifelse(is.na(fh$heartRate), 75, fh$heartRate)
summary(factor(fh$heartRate))

# glucose
summary(factor(fh$glucose))
fh$glucose = ifelse(is.na(fh$glucose), mean(fh$glucose, na.rm = TRUE) ,fh$glucose)
summary(factor(fh$glucose))

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

colSums(sapply(fh, is.na)) # NO NA values

str(fh)

# Feature Engineering 

# BMI and currentSmoker 
fh$currentSmoker_BMI = as.numeric(fh$currentSmoker)*fh$BMI

# currentSmoker and Hypertension 
fh$currentSmoker_hyp = as.numeric(fh$currentSmoker)*as.numeric(fh$prevalentHyp)

# sysBP and prevalentHyp 
fh$prevalentHyp_sysBP = as.numeric(fh$prevalentHyp)*fh$sysBP

# cigsPerday and BMI 
fh$cigs_bmi = fh$cigsPerDay*fh$BMI

# cigsPerday and currentSmoker
fh$cigs_smoke = fh$cigsPerDay*as.numeric(fh$currentSmoker)

#cor(fh$sysBP, fh$BMI) 0.33
#cor(fh$currentSmoker, fh$BMI)
# male and BMI no effect
#fh$male_BMI = as.numeric(fh$male)*(fh$BMI)
# BMI and diabetes no effect
#fh$diabetes_BMI = as.numeric(fh$diabetes)*fh$BMI
# cigsPerDay and heartRate no effect
#fh$cigsperday_heartrate = fh$cigsPerDay*fh$heartRate
# sysBP and age no effect
#fh$sysBP_age = fh$sysBP*as.numeric(fh$age)
# diaBP and prevalentHyp no effect
#fh$prevalentHyp_diaBP = as.numeric(fh$prevalentHyp)*fh$diaBP
# age and prevalentHyp no effect
#fh$prevalentHyp_age = as.numeric(fh$prevalentHyp)*fh$age
# sysBP and diaBP remove
# Stroke and Hypertension remove
# fh$str_hyp = as.numeric(fh$prevalentStroke)*as.numeric(fh$prevalentHyp)
#fh$sysBP_diaBP = fh$diaBP*fh$sysBP
# age and currentSmoker remove
#fh$age_CHD = as.numeric(fh$currentSmoker)*fh$age
# BPMeds and sysBP no effect
#fh$BPMeds_sysBP = fh$BPMeds*fh$sysBP
# BPMeds and diaBP no effect
#fh$BPMeds_diaBP = fh$BPMeds*fh$diaBP
# BPMeds and Hypertension no effect
#fh$BPMeds_prevalentHyp = fh$BPMeds*as.numeric(fh$prevalentHyp)
#  -currentSmoker_str -age_str -t_gluc -gluc_bmi -sysBP_BMI

colSums(sapply(fh, is.na))

# NA values for numerical variables 
# cigsPerDay 

# Candidates for conversion to categorical:
# education, currentSmoker, prevalentStroke, prevalentHyp, diabetes, male

# Split data into Train & Test
set.seed(110)
split1<- sample(c(rep(0, 0.7 * nrow(fh)), rep(1, 0.3 * nrow(fh))))
table(split1)

train <- fh[split1 == 0, ]   
test <- fh[split1== 1, ]   

#str(fh)
Logmodel_v1 = glm(TenYearCHD ~ . -sysBP -diaBP -prevalentHyp -education -heartRate -currentSmoker
                  , data=train, family=binomial)
summary(Logmodel_v1)

test_prob = predict(Logmodel_v1, newdata = test, type = "response")
test_roc = roc(test$TenYearCHD ~ test_prob, plot = TRUE, print.auc = TRUE)
# AUC = 0.737
