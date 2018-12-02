##LOAD PACKAGES##
library(tidyverse)
library(DataExplorer)
library(stargazer)
library(car)
library(InformationValue)
library(MASS)
##READ IN DATA
InsuranceI <- read.csv(file = "insurance_training_data.csv", header = TRUE, sep = ",")
INSURANCE <- select(InsuranceI, -INDEX) # Remove INDEX for convenience
# Checking that variables were read in properly
str(INSURANCE)
### Replacing Blanks in JOB with NA
is.na(INSURANCE$JOB[INSURANCE$JOB == ""]) <- TRUE
##Recode number variables properly by removing dollars and commas from data set##
INSURANCE$TARGET_FLAG = as.factor(INSURANCE$TARGET_FLAG)
INSURANCE$BLUEBOOK = as.numeric(gsub("\\$|,","", INSURANCE$BLUEBOOK))
INSURANCE$HOME_VAL = as.numeric(gsub("\\$|,","", INSURANCE$HOME_VAL))
INSURANCE$INCOME= as.numeric(gsub("\\$|,","", INSURANCE$INCOME))
INSURANCE$OLDCLAIM = as.numeric(gsub("\\$|,","",INSURANCE$OLDCLAIM))
#RECODE 2 level Factor variables for more appropriate scope
INSURANCE <- INSURANCE %>%
  mutate(MSTATUS = as.factor(ifelse(MSTATUS == "Yes", 1, 0)),
         SEX = as.factor(ifelse(SEX == "M", 0, 1)))
sjp.corr(INSURANCE)

##INITIAL DATA ANALYSIS##
plot_missing(INSURANCE_CMPLT) # Generate missing data profile
stargazer(INSURANCE, type = "html", title = "Descriptive Statistics", digits = 3, out = "Table1.htm") #Produce table of descriptive statistics
plot_histogram(INSURANCE) # Generate histograms of numeric variables
#
#Isolate Target_AMT and view revised distribution#
Insurance_Cost <- filter(INSURANCE, TARGET_FLAG == 1)
plot_density(Insurance_Cost$TARGET_AMT)
#Bar Plots for categorical variables
plot_bar(INSURANCE, nrow = 4L)


###DATA PREPARATION###
#Identifying cause of NA's in JOB#
INSURANCE %>%
  select(INCOME, JOB, HOME_VAL, CAR_AGE, YOJ) %>%
  filter((INCOME == 0 | is.na(INCOME)) & is.na(JOB))
INSURANCE %>%
  select(INCOME, JOB, HOME_VAL, CAR_AGE, YOJ) %>%
  filter(YOJ == 0  & is.na(JOB))
##JOB Imputation#

levels(INSURANCE$JOB) <- c(levels(INSURANCE$JOB), "Other")
INSURANCE$JOB[is.na(INSURANCE$JOB)] <- "Other"

plot_bar(INSURANCE$JOB) #New Bar Plot
#Dropping rows where AGE is NA#
INSURANCE_CMPLT <- INSURANCE[!is.na(INSURANCE$AGE),]
#
#Identifying cause of NA's in INCOME and YOJ#
INSURANCE %>%
  select(INCOME, YOJ) %>%
  group_by(YOJ) %>%
  filter(INCOME == 0) %>%
  count(INCOME)
#Replace NA's in YOJ corresponding with INCOME
INSURANCE_CMPLT <- INSURANCE_CMPLT %>%
  mutate(INCOME = ifelse(is.na(YOJ) & is.na(INCOME), 0, INCOME), 
         YOJ = ifelse(is.na(YOJ) & INCOME == 0, 0, YOJ), 
         INCOME = ifelse(is.na(INCOME) & (JOB == "Student"|JOB == "Home Maker"), 0, INCOME),
         UNEMP = as.factor(ifelse(INCOME != 0 | is.na(INCOME) , 0, 1)))
#REPLACE REMAINING INCOME NA VALUES WITH MEDIANS OF GROUPS##
INSURANCE_CMPLT <- INSURANCE_CMPLT %>%
  group_by(JOB, EDUCATION, SEX, UNEMP) %>%
  mutate(INCOME = ifelse(is.na(INCOME), median(INCOME, na.rm = TRUE), INCOME)) %>%
  ungroup(INSURANCE_CMPLT)


##Fix Negative value for CAR_AGE and replace NA'S for remaining variables
INSURANCE_CMPLT <- INSURANCE_CMPLT %>%
  mutate(CAR_AGE = ifelse(CAR_AGE == -3, 3, CAR_AGE),
         CAR_AGE = ifelse(is.na(CAR_AGE), 0, CAR_AGE),
         YOJ = ifelse(is.na(YOJ), 0, YOJ),
         HOME_VAL = ifelse(is.na(HOME_VAL), 0, HOME_VAL))


##RECODE FACTOR VARIABLES##
INSURANCE_CMPLT <- INSURANCE_CMPLT %>%
  mutate(KIDSDRIV = as.factor(ifelse(KIDSDRIV > 0, 1, 0)),
         HOMEKIDS = as.factor(ifelse(HOMEKIDS > 0, 1, 0)),
         YNG = as.factor(ifelse(AGE <= 25, 1, 0 )),
         ELDER = as.factor(ifelse(AGE >= 65, 1, 0)),
         NEW_CAR = as.factor(ifelse(CAR_AGE <= 1, 1, 0)),
         RENT = as.factor(ifelse(HOME_VAL == 0, 1, 0)),
         JOB = relevel(JOB, ref = 9))

plot_density(INSURANCE_CMPLT_T$logBLUEBOOK)
plot_density(INSURANCE_CMPLT_T$logINCOME)

######BUILDING MODELS############
###MODEL 1###
LOGIT_INSURANCE <- select(INSURANCE_CMPLT, - TARGET_AMT) #Seperate out Target_AMT
#
LOGIT_1 <- glm(TARGET_FLAG ~ ., family = binomial(link = "logit"), data = LOGIT_INSURANCE) #Full model
BACKWARD_SELECT <- step(LOGIT_1) #Backward selection
summary(BACKWARD_SELECT) # Examine results of step
#Rebuild model without insignificant variables
LOGIT_1a <- glm(TARGET_FLAG ~ KIDSDRIV + HOMEKIDS + MSTATUS + EDUCATION + JOB + TRAVTIME + CAR_USE + 
                  TIF + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + URBANICITY + YNG + CAR_TYPE + 
                  RENT + BLUEBOOK + UNEMP + INCOME + RENT,family = binomial(link = "logit"), LOGIT_INSURANCE)
options(scipen = 999)
summary(LOGIT_1a)
tab_model(LOGIT_1a, show.ci = FALSE, p.style = "numeric", digits = 4, emph.p = FALSE, group.terms = TRUE) #EXPORT RESULTS

PRED1a <- predict(LOGIT_1a, LOGIT_INSURANCE, type = "response") #get predicted scores
OPTTHRESH <- optimalCutoff(LOGIT_INSURANCE$TARGET_FLAG, PRED1a) #Retune threshold for probability cutoff
misClassError(LOGIT_INSURANCE$TARGET_FLAG, PRED1a, threshold = OPTTHRESH) #Classification Error
confusionMatrix(LOGIT_INSURANCE$TARGET_FLAG, PRED1a, threshold = OPTTHRESH) #Correlation matrix info
sensitivity(LOGIT_INSURANCE$TARGET_FLAG, PRED1a, threshold = OPTTHRESH) #Sensitivity
specificity(LOGIT_INSURANCE$TARGET_FLAG, PRED1a, threshold = OPTTHRESH) #Specificity
plotROC(LOGIT_INSURANCE$TARGET_FLAG, PRED1a)#ROC PLOT

###MODEL 2###
LOGIT_INSURANCE2 <- LOGIT_INSURANCE
LOGIT_INSURANCE2 <- LOGIT_INSURANCE2 %>%
  mutate(COLLEGE = as.factor(ifelse(EDUCATION != "<High School" & EDUCATION != "z_High School", 1, 0)),
         WHITECOL = as.factor(ifelse(JOB != "z_Blue Collar", 1, 0)),
         RESP = as.factor(ifelse(WHITECOL == 0, "Low",ifelse(JOB == "Doctor" | JOB == "Manager", "HIGH", "MED"))),
         RESP = relevel(RESP, ref = 2),
         HIGHED = as.factor(ifelse(COLLEGE == 0,0,ifelse(EDUCATION == "Bachelors", 1, ifelse(EDUCATION == "Masters", 2, 3)))),
         SPORT = as.factor(ifelse(CAR_TYPE == "z_SUV" | CAR_TYPE == "Sports Car", 1, 0)),
         CLM_AVG = ifelse(CLM_FREQ != 0, OLDCLAIM/CLM_FREQ, 0),
         SEVERE = as.factor(ifelse(CLM_AVG < 15710, 0, 1)),
         INCBRAC = cut(INCOME, c(-1,24999,34999,99999,367030), labels = c("POV", "Low", "Mid", "High")),
         LOYALTY = cut(TIF, c(0,2,5,9,25), labels = c("New", "Regular", "Loyal", "Long-Time")),
         DIST = cut(TRAVTIME, c(0,15,45,142),labels = c("Short", "Average", "Long")),
         EXPCAR = as.factor(ifelse(BLUEBOOK < 40000, 0,1)),
         WEALTHY = as.factor(ifelse(INCBRAC == "High", 1, 0)),
         TCUST = as.integer(ifelse(LOYALTY == "New", 0,ifelse(LOYALTY == "REGULAR", 1, ifelse(LOYALTY == "Loyal", 2,3)))),
         MANAG = as.factor(ifelse(JOB == "Manager", 1, 0)))





LOGIT_2 <- glm(TARGET_FLAG ~ KIDSDRIV + HOMEKIDS + MSTATUS + EDUCATION + JOB + TRAVTIME + CAR_USE + 
                   TIF + OLDCLAIM + CLM_FREQ  + MVR_PTS + REVOKED + URBANICITY + YNG + 
                   RENT + UNEMP + INCOME + BLUEBOOK + DIST + EXPCAR + SEVERE + HIGHED + WEALTHY + SPORT,family = binomial(link = "logit"), LOGIT_INSURANCE2)
summary(LOGIT_2)
LOGIT_INSURANCE2 <- LOGIT_INSURANCE2 %>%
  mutate(EDLVL = as.factor(ifelse(HIGHED == 1, "UNDERGRAD",ifelse(HIGHED == 2 | HIGHED == 3, "GRAD", 0))),
         DECIS = as.factor(ifelse(RESP == "HIGH", 3,ifelse(JOB == "Lawyer" | JOB == "Professional", 2, ifelse(JOB == "Student" | JOB == "Home Maker" | JOB == "Other", 1,0)))),
         AUTONO = as.factor(ifelse(RESP == "HIGH", 2, ifelse(JOB == "Student" | JOB == "Home Maker", 1,0))),
         BLUEBOOKSQ = BLUEBOOK^2)
         
LOGIT_2a <- glm(TARGET_FLAG ~ KIDSDRIV + HOMEKIDS + MSTATUS + EDLVL + 
                  AUTONO + TRAVTIME + CAR_USE + TIF + OLDCLAIM + CLM_FREQ + MVR_PTS + 
                  REVOKED + URBANICITY + YNG + RENT + UNEMP + INCOME + BLUEBOOK + BLUEBOOKSQ + SPORT, family = binomial(link = "logit"), LOGIT_INSURANCE2)
summary(LOGIT_2a)
tab_model(LOGIT_2a, show.ci = FALSE, p.style = "numeric", digits = 4, emph.p = FALSE, group.terms = TRUE) 
PRED2 <- predict(LOGIT_2a, LOGIT_INSURANCE2, type = "response")
OPTTHRESH2 <- optimalCutoff(LOGIT_INSURANCE3$TARGET_FLAG, PRED2)
misClassError(LOGIT_INSURANCE3$TARGET_FLAG, PRED2, threshold = OPTTHRESH2)
confusionMatrix(LOGIT_INSURANCE3$TARGET_FLAG, PRED2, threshold = OPTTHRESH2)
sensitivity(LOGIT_INSURANCE3$TARGET_FLAG, PRED2, threshold = OPTTHRESH2)
specificity(LOGIT_INSURANCE3$TARGET_FLAG, PRED2, threshold = OPTTHRESH2)
plotROC(LOGIT_INSURANCE3$TARGET_FLAG, PRED2)



###MODEL 3###
LOGIT_INSURANCE3 <- select(LOGIT_INSURANCE2, -CAR_TYPE)

  
Logit_3 <- glm(TARGET_FLAG ~ ., family = binomial(link = "logit"), LOGIT_INSURANCE3)
library(MASS)
BOTH_SELECT <- stepAIC(Logit_3, direction = "both")
summary(BOTH_SELECT)

LOGIT_3a <-glm(TARGET_FLAG ~ KIDSDRIV + HOMEKIDS + INCOME + PARENT1 + 
                 MSTATUS + SEX + EDUCATION + JOB + TRAVTIME + CAR_USE + BLUEBOOK + 
                 OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + URBANICITY + UNEMP + 
                 YNG + NEW_CAR + RENT + SPORT + CLM_AVG + SEVERE + LOYALTY + 
                 BLUEBOOKSQ, family = binomial(link = "logit"), LOGIT_INSURANCE3)

summary(LOGIT_3a)
tab_model(LOGIT_3a, show.ci = FALSE, p.style = "numeric", digits = 4, emph.p = FALSE, group.terms = TRUE) 
PRED3 <- predict(LOGIT_3a, LOGIT_INSURANCE3, type = "response")
OPTTHRESH3 <- optimalCutoff(LOGIT_INSURANCE3$TARGET_FLAG, PRED3)
misClassError(LOGIT_INSURANCE3$TARGET_FLAG, PRED3, threshold = OPTTHRESH3)
confusionMatrix(LOGIT_INSURANCE3$TARGET_FLAG, PRED3, threshold = OPTTHRESH3)
sensitivity(LOGIT_INSURANCE3$TARGET_FLAG, PRED3, threshold = OPTTHRESH3)
specificity(LOGIT_INSURANCE3$TARGET_FLAG, PRED3, threshold = OPTTHRESH3)
plotROC(LOGIT_INSURANCE3$TARGET_FLAG, PRED3)

vif(LOGIT_3a)


###LINEAR MODELS###
LINEAR_INSURANCE <- select(INSURANCE_CMPLT, -TARGET_FLAG)
LINEAR_INSURANCE <- LINEAR_INSURANCE %>%
  filter(TARGET_AMT != 0) %>%
  mutate(COLLEGE = as.factor(ifelse(EDUCATION != "<High School" & EDUCATION != "z_High School", 1, 0)),
         WHITECOL = as.factor(ifelse(JOB != "z_Blue Collar", 1, 0)),
         RESP = as.factor(ifelse(WHITECOL == 0, "Low",ifelse(JOB == "Doctor" | JOB == "Manager", "HIGH", "MED"))),
         RESP = relevel(RESP, ref = 2),
         HIGHED = as.factor(ifelse(COLLEGE == 0,0,ifelse(EDUCATION == "Bachelors", 1, ifelse(EDUCATION == "Masters", 2, 3)))),
         SPORT = as.factor(ifelse(CAR_TYPE == "z_SUV" | CAR_TYPE == "Sports Car", 1, 0)),
         CLM_AVG = ifelse(CLM_FREQ != 0, OLDCLAIM/CLM_FREQ, 0),
         SEVERE = as.factor(ifelse(CLM_AVG < 15710, 0, 1)),
         INCBRAC = cut(INCOME, c(-1,24999,34999,99999,367030), labels = c("POV", "Low", "Mid", "High")),
         LOYALTY = cut(TIF, c(0,2,5,9,25), labels = c("New", "Regular", "Loyal", "Long-Time")),
         DIST = cut(TRAVTIME, c(0,15,45,142),labels = c("Short", "Average", "Long")),
         EXPCAR = as.factor(ifelse(BLUEBOOK < 40000, 0,1)),
         WEALTHY = as.factor(ifelse(INCBRAC == "High", 1, 0)),
         TCUST = as.integer(ifelse(LOYALTY == "New", 0,ifelse(LOYALTY == "REGULAR", 1, ifelse(LOYALTY == "Loyal", 2,3)))),
         MANAG = as.factor(ifelse(JOB == "Manager", 1, 0)),
         EDLVL = as.factor(ifelse(HIGHED == 1, "UNDERGRAD",ifelse(HIGHED == 2 | HIGHED == 3, "GRAD", 0))),
         DECIS = as.factor(ifelse(RESP == "HIGH", 3,ifelse(JOB == "Lawyer" | JOB == "Professional", 2, ifelse(JOB == "Student" | JOB == "Home Maker" | JOB == "Other", 1,0)))),
         AUTONO = as.factor(ifelse(RESP == "HIGH", 2, ifelse(JOB == "Student" | JOB == "Home Maker", 1,0))),
         BLUEBOOKSQ = BLUEBOOK^2)
##LINEAR MODEL 1##
LINEAR_1 <- lm(TARGET_AMT ~ ., LINEAR_INSURANCE)
LINEAR_STEP <- stepAIC(LINEAR_1, direction = "both")
summary(LINEAR_STEP)
plot(LINEAR_STEP)
LINEAR_1a <- lm(log(TARGET_AMT) ~ MSTATUS + SEX + BLUEBOOK + REVOKED + MVR_PTS + CAR_AGE + NEW_CAR + SEVERE + BLUEBOOKSQ + MANAG + EDLVL + SPORT, LINEAR_INSURANCE)
sm <- summary(LINEAR_1a)
par(mfrow=c(2,2))
plot(LINEAR_1a)
tab_model(LINEAR_1a, show.ci = FALSE, p.style = "numeric", digits = 4, emph.p = FALSE, group.terms = TRUE) 
mean(sm$residuals^2)
sm



##LINEAR MODEL 2##
LINEAR_INSURANCE2 <- LINEAR_INSURANCE %>%
  mutate(logBLUEBOOK = log(BLUEBOOK),
         logINCOME = log(INCOME + 1),
         logOLDCLAIM = log(OLDCLAIM + 1),
         logTIF = log(TIF),
         logMVR_PTS = log(MVR_PTS + 1),
         logCAR_AGE = log(CAR_AGE + 1),
         logCLM_FREQ = log(CLM_FREQ + 1))
LINEAR_INSURANCE2 <- LINEAR_INSURANCE2[,-c(6,13,15,22,23,25,26)]
LINEAR_2 <- lm(log(TARGET_AMT) ~ ., LINEAR_INSURANCE2)
LINEAR2_STEP <- stepAIC(LINEAR_2, direction = "both")
summary(LINEAR2_STEP)
plot(LINEAR2_STEP)
LINEAR_INSURANCE3 <- LINEAR_INSURANCE2[-c(939,1372,1744), ]
LINEAR_2b <- lm(log(TARGET_AMT) ~ MSTATUS + SEX + CLM_FREQ + REVOKED + 
    SEVERE + EXPCAR + logBLUEBOOK + logMVR_PTS + EDLVL, LINEAR_INSURANCE3)
sm2 <- summary(LINEAR_2b)
par(mfrow=c(2,2))
plot(LINEAR_2b)
tab_model(LINEAR_2b, show.ci = FALSE, p.style = "numeric", digits = 4, emph.p = FALSE, group.terms = TRUE) 
mse <- function(sm2) 
mean(sm2$residuals^2)
####PREDICTIONS#########################################################################################################
InsuranceEVAL <- read.csv(file = "insurance-evaluation-data.csv", header = TRUE, sep = ",")
INSURANCE_EVAL <- select(InsuranceEVAL, -INDEX)
is.na(INSURANCE_EVAL$JOB[INSURANCE_EVAL$JOB == ""]) <- TRUE
##APPLY NECSESSARY MODIFICATION TO EVAL (NA REMOVAL, VARIABLE CREATION)##
INSURANCE_EVAL <- INSURANCE_EVAL %>%
  mutate(BLUEBOOK = as.numeric(gsub("\\$|,","", BLUEBOOK)),
         HOME_VAL = as.numeric(gsub("\\$|,","", HOME_VAL)),
         INCOME= as.numeric(gsub("\\$|,","", INCOME)),
         OLDCLAIM = as.numeric(gsub("\\$|,","",OLDCLAIM)),
         MSTATUS = as.factor(ifelse(MSTATUS == "Yes", 1, 0)),
         SEX = as.factor(ifelse(SEX == "M", 0, 1)))

levels(INSURANCE_EVAL$JOB) <- c(levels(INSURANCE_EVAL$JOB), "Other")
INSURANCE_EVAL$JOB[is.na(INSURANCE_EVAL$JOB)] <- "Other"


INSURANCE_EVAL <- INSURANCE_EVAL %>%
  mutate(INCOME = ifelse(is.na(YOJ) & is.na(INCOME), 0, INCOME), 
         YOJ = ifelse(is.na(YOJ) & INCOME == 0, 0, YOJ), 
         INCOME = ifelse(is.na(INCOME) & (JOB == "Student"|JOB == "Home Maker"), 0, INCOME),
         UNEMP = as.factor(ifelse(INCOME != 0 | is.na(INCOME) , 0, 1)))
INSURANCE_EVAL <- INSURANCE_EVAL %>%
  group_by(JOB, EDUCATION, SEX, UNEMP) %>%
  mutate(INCOME = ifelse(is.na(INCOME), median(INCOME, na.rm = TRUE), INCOME)) %>%
  ungroup(INSURANCE_CMPLT)


INSURANCE_EVAL <- INSURANCE_EVAL %>%
  mutate(CAR_AGE = ifelse(is.na(CAR_AGE), 0, CAR_AGE),
         YOJ = ifelse(is.na(YOJ), 0, YOJ),
         HOME_VAL = ifelse(is.na(HOME_VAL), 0, HOME_VAL))
INSURANCE_EVAL <- INSURANCE_EVAL[!is.na(INSURANCE_EVAL$AGE),]
plot_missing(INSURANCE_EVAL)
##RECODE FACTOR VARIABLES##
INSURANCE_EVAL_F <- INSURANCE_EVAL %>%
  mutate(KIDSDRIV = as.factor(ifelse(KIDSDRIV > 0, 1, 0)),
         HOMEKIDS = as.factor(ifelse(HOMEKIDS > 0, 1, 0)),
         YNG = as.factor(ifelse(AGE <= 25, 1, 0 )),
         RENT = as.factor(ifelse(HOME_VAL == 0, 1, 0)),
         JOB = relevel(JOB, ref = 9),
         COLLEGE = as.factor(ifelse(EDUCATION != "<High School" & EDUCATION != "z_High School", 1, 0)),
         WHITECOL = as.factor(ifelse(JOB != "z_Blue Collar", 1, 0)),
         RESP = as.factor(ifelse(WHITECOL == 0, "Low",ifelse(JOB == "Doctor" | JOB == "Manager", "HIGH", "MED"))),
         RESP = relevel(RESP, ref = 2),
         HIGHED = as.factor(ifelse(COLLEGE == 0,0,ifelse(EDUCATION == "Bachelors", 1, ifelse(EDUCATION == "Masters", 2, 3)))),
         SPORT = as.factor(ifelse(CAR_TYPE == "z_SUV" | CAR_TYPE == "Sports Car", 1, 0)),
         CLM_AVG = ifelse(CLM_FREQ != 0, OLDCLAIM/CLM_FREQ, 0),
         SEVERE = as.factor(ifelse(CLM_AVG < 15710, 0, 1)),
         INCBRAC = cut(INCOME, c(-1,24999,34999,99999,367030), labels = c("POV", "Low", "Mid", "High")),
         LOYALTY = cut(TIF, c(0,2,5,9,25), labels = c("New", "Regular", "Loyal", "Long-Time")),
         DIST = cut(TRAVTIME, c(0,15,45,142),labels = c("Short", "Average", "Long")),
         EXPCAR = as.factor(ifelse(BLUEBOOK < 40000, 0,1)),
         WEALTHY = as.factor(ifelse(INCBRAC == "High", 1, 0)),
         TCUST = as.integer(ifelse(LOYALTY == "New", 0,ifelse(LOYALTY == "REGULAR", 1, ifelse(LOYALTY == "Loyal", 2,3)))),
         MANAG = as.factor(ifelse(JOB == "Manager", 1, 0)),
         EDLVL = as.factor(ifelse(HIGHED == 1, "UNDERGRAD",ifelse(HIGHED == 2 | HIGHED == 3, "GRAD", 0))),
         DECIS = as.factor(ifelse(RESP == "HIGH", 3,ifelse(JOB == "Lawyer" | JOB == "Professional", 2, ifelse(JOB == "Student" | JOB == "Home Maker" | JOB == "Other", 1,0)))),
         AUTONO = as.factor(ifelse(RESP == "HIGH", 2, ifelse(JOB == "Student" | JOB == "Home Maker", 1,0))),
         BLUEBOOKSQ = BLUEBOOK^2)

##LOGISTIC PREDICTION##
Evaluation_Target_Flag_NUM <- predict(LOGIT_2a, INSURANCE_EVAL_F, type = "response")
Evaluation_Target_Flag_FAC <- ifelse(Evaluation_Target_Flag_NUM > OPTTHRESH2, 1, 0)

INSURANCE_EVAL_F <- INSURANCE_EVAL_F %>%
  mutate(logBLUEBOOK = log(BLUEBOOK),
         logINCOME = log(INCOME + 1),
         logOLDCLAIM = log(OLDCLAIM + 1),
         logTIF = log(TIF),
         logMVR_PTS = log(MVR_PTS + 1),
         logCAR_AGE = log(CAR_AGE + 1),
         logCLM_FREQ = log(CLM_FREQ + 1))
Evaluation_Target_Amt <- predict(LINEAR_2b,INSURANCE_EVAL_F[Evaluation_Target_Flag_FAC == 1,], type = "response" )
Evaluation_Target_Amt <- exp(Evaluation_Target_Amt)
Evaluation_Target_Amt_ZEROS <- rep(0,nrow(INSURANCE_EVAL_F))
Evaluation_Target_Amt_ZEROS[Evaluation_Target_Flag_FAC == 1] <- Evaluation_Target_Amt
Evaluation_Predictions <- data.frame(Probability_of_Crash = round(Evaluation_Target_Flag_NUM, digits = 4),
                                     TARGET_FLAG = Evaluation_Target_Flag_FAC,
                                     TARGET_AMT = round(Evaluation_Target_Amt_ZEROS, digits = 2),
                                     check.names = FALSE)
write.table(Evaluation_Predictions, file = "Insurance_Predictions.csv", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ",")
summary(Evaluation_Predictions)
