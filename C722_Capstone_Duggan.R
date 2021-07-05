#load all necessary packages

library(lubridate)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(pedometrics)
library(reshape2)
library(colorspace)
library(MASS)
library(pROC)

# Set current working directory.
work_dir <- "C:/Users/19496/Desktop/C772"
setwd(work_dir)
print(getwd())


#load the Excel file into a variable
NYC_Arrests <- read.csv("NYPD_Arrest_Data__Year_to_Date_.csv", header = TRUE)

#Summary of the data
summary(NYC_Arrests)

##
#Data cleaning of necessary columns/rows
sum(NYC_Arrests$LAW_CAT_CD == "")
NYC_Cleaned <- filter(NYC_Arrests, NYC_Arrests$LAW_CAT_CD != "")
NYC_Cleaned <- NYC_Cleaned[-c(15, 16, 17, 18, 19)]
drop <- c("ARREST_KEY", "PD_CD", "PD_DESC", "KY_CD", "OFNS_DESC", "LAW_CODE", "JURISDICTION_CODE")
NYC_Cleaned <- NYC_Cleaned[,!names(NYC_Cleaned) %in% drop]
##

##
#Data Continuity and dummy variables
NYC_Cleaned$ARREST_BORO <-ifelse(NYC_Cleaned$ARREST_BORO == "B", "Bronx", ifelse(NYC_Cleaned$ARREST_BORO == "S", "Staten", ifelse(NYC_Cleaned$ARREST_BORO == "K", "Brooklyn", ifelse(NYC_Cleaned$ARREST_BORO == "M", "Manhattan", ifelse(NYC_Cleaned$ARREST_BORO == "Q", "Queens", "Unknown")))))
borofactor <- as.factor(NYC_Cleaned$ARREST_BORO)

NYC_Cleaned$FelonyCharge <-ifelse(NYC_Cleaned$LAW_CAT_CD == "F", "Y", ifelse(NYC_Cleaned$LAW_CAT_CD == "M", "N", "N"))
felonyfactor <- as.factor(NYC_Cleaned$FelonyCharge)

NYC_Cleaned$ARREST_DATE <- month(mdy(NYC_Cleaned$ARREST_DATE))
arrestMonth <- as.factor(NYC_Cleaned$ARREST_DATE)
NYC_Cleaned$ARREST_DATE <- month.name[NYC_Cleaned$ARREST_DATE]

arrestAge <- as.factor(NYC_Cleaned$AGE_GROUP)

NYC_Cleaned$PERP_RACE <-ifelse(NYC_Cleaned$PERP_RACE == "AMERICAN INDIAN/ALASKAN NATIVE", "AI/AN", ifelse(NYC_Cleaned$ARREST_BORO == "HAWAIIAN/PACIFIC ISLANDER", "HI/PI", ifelse(NYC_Cleaned$PERP_RACE == "BLACK", "BL", ifelse(NYC_Cleaned$PERP_RACE == "BLACK HISPANIC", "BL HIS", ifelse(NYC_Cleaned$PERP_RACE == "WHITE", "WH", ifelse(NYC_Cleaned$PERP_RACE == "WHITE HISPANIC", "WH HIS", "Unknown"))))))
arrestEthnicity <- as.factor(NYC_Cleaned$PERP_RACE)

arrestSex <- as.factor(NYC_Cleaned$PERP_SEX)

drop2 <- c("ARREST_PRECINCT", "LAW_CAT_CD")
NYC_Cleaned <- NYC_Cleaned[,!names(NYC_Cleaned) %in% drop2]

###
#normality test
par(xpd=FALSE)
qqnorm(NYC_Arrests$ARREST_PRECINCT, col="blue", main = "QQ Plot of ARREST_PRECINCT")
qqline(NYC_Arrests$ARREST_PRECINCT, col="black", lwd=3)
qqnorm(NYC_Arrests$PD_CD, col="blue", main = "QQ Plot of PD_CD")
qqline(NYC_Arrests$PD_CD, col="black", lwd=3)
qqnorm(NYC_Arrests$KY_CD, col="blue", main = "QQ Plot of KY_CD")
qqline(NYC_Arrests$KY_CD, col="black", lwd=3)

###
#Descriptive Analysis
plot(felonyfactor, ylim=c(1,70000), col="blue4", main = "Count of Felony Charges")
plot(borofactor, ylim=c(1,30000), xlab="Borough", col="blue4", main = "Arrest Counts by Boroughs")
plot(arrestMonth, ylim=c(0, 20000), col="blue4", main = "Arrest Counts by Month")
plot(arrestSex, col="blue4", main = "Arrest Counts by Sex")
plot(arrestAge, ylim=c(0, 60000), col="blue4", main = "Arrest Counts by Age Group")
plot(arrestEthnicity, col="blue4", las=2, main = "Arrest Counts by Ethnicity")

#Start MCA
arrest_data.mca = MCA(NYC_Cleaned,quali.sup=6,graph=TRUE)

summary(arrest_data.mca)

fviz_screeplot(arrest_data.mca, addlabels=TRUE)

fviz_pca_var(arrest_data.mca, col.var="contrib", axes = 1:2, repel = TRUE, select.var = list(cos2 = 10), gradient.cols=c("blue","green","red"))

fviz_contrib(arrest_data.mca, choice = "var", axes = 1:3)

###
#Check for co-linearity through GG plot/heat map
cmatrix <- cramer(NYC_Cleaned)
mcmatrix = melt(cmatrix)
ggplot(data = mcmatrix, aes(x=Var1 ,y=Var2, fill=value))+geom_tile() +
  scale_fill_gradient(low = "white",high="blue") + theme(
    axis.text.x = element_text(angle=-90))

###
#Create training/test data sets from cleaned data
set.seed(999)
sample <- floor(0.7 * nrow(NYC_Cleaned))
partition <- sample(seq_len(nrow(NYC_Cleaned)),size=sample)
arrest_train <- NYC_Cleaned[partition,]
arrest_test <- NYC_Cleaned[-partition,]

##
# Start a logit model with the full training data
full_arrest.model <- glm(formula = as.factor(FelonyCharge) ~ ., family = binomial(link = "logit"), data = arrest_train)

# Create a null model - uses no variables (intercept-only model)
null_arrest.model <- glm(formula = as.factor(FelonyCharge) ~ 1, family = binomial(link = "logit"), data = arrest_train)

# Use the full and null model to create an optimal solution
step_arrest.model <- stepAIC(null_arrest.model,scope =list(lower=null_arrest.model,upper=full_arrest.model),direction="both")

##
summary(step_arrest.model)
coef(step_arrest.model)

##
#Evaluate the regression fit of the model
anova(step_arrest.model)

###
#AUC and AIC
pred_arrest <- predict(step_arrest.model, newdata = arrest_test, type = "response")

roc.arrest <-plot.roc(arrest_test$FelonyCharge,pred_arrest,
                    identity.col = "slateblue4",
                    print.auc=TRUE,auc.polygon=TRUE,auc.polygon.col="azure2")

roc.arrest$sensitivities[1662*.601]
roc.arrest$specificities[1662*.601]
