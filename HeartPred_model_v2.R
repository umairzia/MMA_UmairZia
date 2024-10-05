library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(pROC)

# for MAc users only 
#path1 <- '/Users/umairzia786/Documents/MMA 867/Umair/framingham.csv'
#fh <- read_csv(path1)

# Windows
fh <- read.csv("D:\\MMA 867\\Final project\\archive\\framingham.csv", header=TRUE, sep = ",")
str(fh)
head(fh)

# Data Cleaning

# Missing values?
md.pattern(fh)

# convert to factor might be a good idea
my_graph0 = ggplot(fh, aes(x = education)) + geom_histogram()
my_graph0


# Feature Engineering

# Hypertension and age; slight strong +ve correlation
fh$Hyp_age = fh$prevalentHyp*fh$age

# age and current Smoker; slight -ve correlation  
fh$currentSmoker_age = fh$currentSmoker*fh$age

# Hypertension and current Smoker; weak -ve correlation  
fh$currentSmoker_Hyp = fh$currentSmoker*fh$prevalentHyp

# age and sysBP; strong +ve correlation
fh$sysBP_age = fh$ager*fh$sysBP


#Split data into Train & Test
set.seed(111)
fh_Sample <- sample.int(n=nrow(fh),size=floor(.7*nrow(fh)),replace = F)
train <- fh[fh_Sample,]
test <- fh[-fh_Sample,]
#split1<- sample(c(rep(0, 0.7 * nrow(fh)), rep(1, 0.3 * nrow(fh))))
#table(split1)

#train <- fh[split1 == 0, ]   
#test <- fh[split1== 1, ]   

# remove age, currentSmoke and prevalentHyp
str(fh)
Logmodel_v2 = glm(TenYearCHD ~ . -age -currentSmoker -prevalentHyp, data=train, family=binomial)
summary(Logmodel_v2)

test_prob = predict(Logmodel_v2, newdata = test, type = "response")
test_roc = roc(test$TenYearCHD ~ test_prob, plot = TRUE, print.auc = TRUE)
# AUC = 0.695
# TenYearCHD =  ten year risk of coronary heart disease risk
# Insurance business value 
# patient preventive plan 

# Corrplot
#Numeric Dataset
fh_Num <- fh %>% select_if(is.numeric)
# correlations on numeric variables
# talk about confusion matrix - max sensitivity, specificity, error rate
# can mention in both project and presentation 
# come up with the right T, different cause behind type 1 and type 2
# don't recommend PCA for everything, PCA for sub-components possible

correlations <- cor(fh_Num)
corrplot(correlations, method = 'color')
# dont explain to seniors the correlation 

