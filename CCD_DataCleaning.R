############################################################################################
###   Model Name              : Credit_Card_Default_Rates                                ###
###   Purpose of the script   : Data cleaning on the full data set                       ###
###   Model Owners            : Dinelka Nanayakkara, Manosha Hansini, Ravindi Wijeratne  ###
###   Model Development Date  : 2020-02-15                                               ###
###   Model Version           : Version_01                                               ###
############################################################################################

############################################################################################
##  Inputting the data from the source #####################################################
############################################################################################

Train_Data        <- read.csv("credit_card_default_train.csv")
Test_Data         <- read.csv("credit_card_default_test.csv")

Full_Data         <- bind_rows(Train_Data,Test_Data)

############################################################################################
##  Converting the factor variable characters to numeric codings(categorical)  #############
############################################################################################

##  Gender

Full_Data$Gender_Num[Full_Data$Gender == 'M'] <- 1
Full_Data$Gender_Num[Full_Data$Gender == 'F'] <- 0

##  Education_Status

Full_Data$Education_Num[Full_Data$EDUCATION_STATUS == 'High School']  <- 0
Full_Data$Education_Num[Full_Data$EDUCATION_STATUS == 'Graduate']     <- 1
Full_Data$Education_Num[Full_Data$EDUCATION_STATUS == 'Other']        <- 2

##  Age

Full_Data$Age_Num[Full_Data$AGE == 'Less than 30']     <- 0
Full_Data$Age_Num[Full_Data$AGE == '31-45']            <- 1
Full_Data$Age_Num[Full_Data$AGE == '46-65']            <- 2
Full_Data$Age_Num[Full_Data$AGE == 'More than 65']     <- 3

## Marital_Status

Full_Data$Marital_Num[Full_Data$MARITAL_STATUS == 'Single']    <- 0
Full_Data$Marital_Num[Full_Data$MARITAL_STATUS == 'Other']     <- 1


##  Adjusting the Credit Limit

ssub <- function (x) gsub("M","*1e6", gsub("K", "*1e3", x))
evalp<- function (x) eval(parse(text=x))

Full_Data$Balance_Limit_V1      <- (sapply(ssub(Full_Data$Balance_Limit_V1), evalp))
Train_Data$Balance_Limit_V1     <- (sapply(ssub(Train_Data$Balance_Limit_V1), evalp))
Test_Data$Balance_Limit_V1      <- (sapply(ssub(Test_Data$Balance_Limit_V1), evalp))


############################################################################################
##  Normalizing the data  ##################################################################
############################################################################################

##  Balance Limit
Full_Data$Balance_Limit_V1 <- (Full_Data$Balance_Limit_V1 - min(Full_Data$Balance_Limit_V1)) / 
      (max(Full_Data$Balance_Limit_V1) - min(Full_Data$Balance_Limit_V1))

##  Due Amounts

Full_Data$DUE_AMT_JULY <- (Full_Data$DUE_AMT_JULY - min(Full_Data$DUE_AMT_JULY)) / 
      (max(Full_Data$DUE_AMT_JULY) - min(Full_Data$DUE_AMT_JULY))

Full_Data$DUE_AMT_AUG <- (Full_Data$DUE_AMT_AUG - min(Full_Data$DUE_AMT_AUG)) / 
      (max(Full_Data$DUE_AMT_AUG) - min(Full_Data$DUE_AMT_AUG))

Full_Data$DUE_AMT_SEP <- (Full_Data$DUE_AMT_SEP - min(Full_Data$DUE_AMT_SEP)) / 
      (max(Full_Data$DUE_AMT_SEP) - min(Full_Data$DUE_AMT_SEP))

Full_Data$DUE_AMT_OCT <- (Full_Data$DUE_AMT_OCT - min(Full_Data$DUE_AMT_OCT)) / 
      (max(Full_Data$DUE_AMT_OCT) - min(Full_Data$DUE_AMT_OCT))

Full_Data$DUE_AMT_NOV <- (Full_Data$DUE_AMT_NOV - min(Full_Data$DUE_AMT_NOV)) / 
      (max(Full_Data$DUE_AMT_NOV) - min(Full_Data$DUE_AMT_NOV))

Full_Data$DUE_AMT_DEC <- (Full_Data$DUE_AMT_DEC - min(Full_Data$DUE_AMT_DEC)) / 
      (max(Full_Data$DUE_AMT_DEC) - min(Full_Data$DUE_AMT_DEC))


##  Paid Amounts

Full_Data$PAID_AMT_JULY <- (Full_Data$PAID_AMT_JULY - min(Full_Data$PAID_AMT_JULY)) / 
      (max(Full_Data$PAID_AMT_JULY) - min(Full_Data$PAID_AMT_JULY))

Full_Data$PAID_AMT_AUG <- (Full_Data$PAID_AMT_AUG - min(Full_Data$PAID_AMT_AUG)) / 
      (max(Full_Data$PAID_AMT_AUG) - min(Full_Data$PAID_AMT_AUG))

Full_Data$PAID_AMT_SEP <- (Full_Data$PAID_AMT_SEP - min(Full_Data$PAID_AMT_SEP)) / 
      (max(Full_Data$PAID_AMT_SEP) - min(Full_Data$PAID_AMT_SEP))

Full_Data$PAID_AMT_OCT <- (Full_Data$PAID_AMT_OCT - min(Full_Data$PAID_AMT_OCT)) / 
      (max(Full_Data$PAID_AMT_OCT) - min(Full_Data$PAID_AMT_OCT))

Full_Data$PAID_AMT_NOV <- (Full_Data$PAID_AMT_NOV - min(Full_Data$PAID_AMT_NOV)) / 
      (max(Full_Data$PAID_AMT_NOV) - min(Full_Data$PAID_AMT_NOV))

Full_Data$PAID_AMT_DEC <- (Full_Data$PAID_AMT_DEC-min(Full_Data$PAID_AMT_DEC)) / 
      (max(Full_Data$PAID_AMT_DEC)-min(Full_Data$PAID_AMT_DEC))



############################################################################################
##  Checking for missing values ############################################################
############################################################################################

Missing_Values <- table(is.na(Full_Data))

##  Combining columns
Payments_Made <- data.frame(Full_Data$PAID_AMT_JULY, Full_Data$PAID_AMT_AUG, 
                            Full_Data$PAID_AMT_SEP, Full_Data$PAID_AMT_OCT,
                            Full_Data$PAID_AMT_NOV, Full_Data$PAID_AMT_DEC)


Past_Payments <- data.frame(Full_Data$PAY_JULY,Full_Data$PAY_AUG, Full_Data$PAY_SEP,
                            Full_Data$PAY_OCT, Full_Data$PAY_NOV, Full_Data$PAY_DEC)


Due_Amount    <- data.frame(Full_Data$DUE_AMT_JULY, Full_Data$DUE_AMT_AUG,
                            Full_Data$DUE_AMT_SEP, Full_Data$DUE_AMT_OCT,
                            Full_Data$DUE_AMT_NOV,Full_Data$DUE_AMT_DEC)


##  Checking for unusal observations here

table(Payments_Made>=0)

Negative_Due_Payments <- Due_Amount %>%
      filter(Due_Amount$Full_Data.DUE_AMT_JULY<0 | Due_Amount$Full_Data.DUE_AMT_AUG<0 |
            Due_Amount$Full_Data.DUE_AMT_SEP<0 | Due_Amount$Full_Data.DUE_AMT_OCT<0 | 
            Due_Amount$Full_Data.DUE_AMT_NOV<0|Due_Amount$Full_Data.DUE_AMT_DEC<0)


##  Logging the paid and Due amounts

Full_Data$PAID_AMT_JULY    <- log10(Full_Data$PAID_AMT_JULY)
Full_Data$PAID_AMT_AUG     <- log10(Full_Data$PAID_AMT_AUG)
Full_Data$PAID_AMT_SEP     <- log10(Full_Data$PAID_AMT_SEP)
Full_Data$PAID_AMT_OCT     <- log10(Full_Data$PAID_AMT_OCT)
Full_Data$PAID_AMT_NOV     <- log10(Full_Data$PAID_AMT_NOV)
Full_Data$PAID_AMT_DEC     <- log10(Full_Data$PAID_AMT_DEC)


############################################################################################
##  Visualizing the data to see the dependence of the variables  ###########################
############################################################################################

par(mfcol = c(6,5))

##  Credit Limit

ggplot(Full_Data,aes(x = Balance_Limit_V1, fill = factor(NEXT_MONTH_DEFAULT))) +
      geom_bar() 

ggplot(Full_Data,aes(x = Balance_Limit_V1, fill = factor(NEXT_MONTH_DEFAULT))) +
      geom_density()

##  Gender

ggplot(Full_Data,aes(x = Gender, fill = factor(NEXT_MONTH_DEFAULT))) +
      geom_bar()


##  Education

ggplot(Full_Data,aes(x = EDUCATION_STATUS, fill = factor(NEXT_MONTH_DEFAULT))) +
      geom_bar()

##  Marital Status

ggplot(Full_Data,aes(x = MARITAL_STATUS, fill = factor(NEXT_MONTH_DEFAULT))) +
      geom_bar()

##  Age

ggplot(Full_Data,aes(x = AGE, fill = factor(NEXT_MONTH_DEFAULT))) +
      geom_bar()


##  Payments made in each month

ggplot(Full_Data,aes(x = PAY_JULY, fill = factor(NEXT_MONTH_DEFAULT))) +
      geom_density()


plot_correlation(Clean_Train_Data_V1, type = "all",maxcat = 10 )

par(mfrow = c(3,5))
plot_histogram(Train_Data)

############################################################################################

##  Uniquness of the data

nrow(data.frame(unique(Full_Data$Client_ID)))

##  Cleaned Data

Clean_Data_V1 <- Full_Data[-c(3:6)]

##  Breaking into the training and test data set

Clean_Train_Data_V1     <- Clean_Data_V1[1:24000,]
Clean_Test_Data_V1      <- Clean_Data_V1[24001:30000,]
Clean_Test_Data_V1      <- Clean_Test_Data_V1[-c(21)]

##  Incorrect age with relevance to the educational level

High_School_Incorrect_Train <- Clean_Train_Data_V1 %>%
      filter(Clean_Train_Data_V1$Education_Num == 0 & Train_Data$AGE != 0
      & Clean_Train_Data_V1$Balance_Limit_V1 > 200000)

High_School_Incorrect_Train_84 <- Clean_Train_Data_V1 %>%
   filter(Clean_Train_Data_V1$Education_Num == 0 & Train_Data$AGE != 0)



Client_ID_Train                      <- data.frame(Clean_Train_Data_V1$Client_ID)

Client_ID_Incorrect                  <- data.frame(High_School_Incorrect_Train$Client_ID)

Client_ID_Correct_Train              <- data.frame(Client_ID_Train[!(
      Client_ID_Train$Clean_Train_Data_V1.Client_ID %in% 
      Client_ID_Incorrect$High_School_Incorrect_Train.Client_ID),])

colnames(Client_ID_Correct_Train)[1] <- 'Client_ID'


Clean_Train_Data_V1                  <- subset(Clean_Train_Data_V1, Client_ID %in% 
                                              Client_ID_Correct_Train$Client_ID)

############################################################################################
##  Normalizing the data  ##################################################################
############################################################################################

##  Train Data

##  Balance Limit
Clean_Train_Data_V1$Balance_Limit_V1 <- (Clean_Train_Data_V1$Balance_Limit_V1-min(
      Clean_Train_Data_V1$Balance_Limit_V1)) / (max(Clean_Train_Data_V1$Balance_Limit_V1) - 
      min(Clean_Train_Data_V1$Balance_Limit_V1))

##  Due Amounts

Clean_Train_Data_V1$DUE_AMT_JULY <- (Clean_Train_Data_V1$DUE_AMT_JULY - min(
      Clean_Train_Data_V1$DUE_AMT_JULY)) / (max(Clean_Train_Data_V1$DUE_AMT_JULY) - min(
      Clean_Train_Data_V1$DUE_AMT_JULY))

Clean_Train_Data_V1$DUE_AMT_AUG <- (Clean_Train_Data_V1$DUE_AMT_AUG - min(
      Clean_Train_Data_V1$DUE_AMT_AUG)) / (max(Clean_Train_Data_V1$DUE_AMT_AUG) - min(
      Clean_Train_Data_V1$DUE_AMT_AUG))

Clean_Train_Data_V1$DUE_AMT_SEP <- (Clean_Train_Data_V1$DUE_AMT_SEP - min(
      Clean_Train_Data_V1$DUE_AMT_SEP)) / (max(Clean_Train_Data_V1$DUE_AMT_SEP) - min(
      Clean_Train_Data_V1$DUE_AMT_SEP))

Clean_Train_Data_V1$DUE_AMT_OCT <- (Clean_Train_Data_V1$DUE_AMT_OCT - min(
      Clean_Train_Data_V1$DUE_AMT_OCT)) / (max(Clean_Train_Data_V1$DUE_AMT_OCT) - min(
      Clean_Train_Data_V1$DUE_AMT_OCT))

Clean_Train_Data_V1$DUE_AMT_NOV <- (Clean_Train_Data_V1$DUE_AMT_NOV - min(
      Clean_Train_Data_V1$DUE_AMT_NOV)) / (max(Clean_Train_Data_V1$DUE_AMT_NOV) - min(
      Clean_Train_Data_V1$DUE_AMT_NOV))

Clean_Train_Data_V1$DUE_AMT_DEC <- (Clean_Train_Data_V1$DUE_AMT_DEC - min(
      Clean_Train_Data_V1$DUE_AMT_DEC)) / (max(Clean_Train_Data_V1$DUE_AMT_DEC) - min(
      Clean_Train_Data_V1$DUE_AMT_DEC))


##  Paid Amounts

Clean_Train_Data_V1$PAID_AMT_JULY <- (Clean_Train_Data_V1$PAID_AMT_JULY - min(
      Clean_Train_Data_V1$PAID_AMT_JULY)) / (max(Clean_Train_Data_V1$PAID_AMT_JULY) - min(
      Clean_Train_Data_V1$PAID_AMT_JULY))

Clean_Train_Data_V1$PAID_AMT_AUG <- (Clean_Train_Data_V1$PAID_AMT_AUG - min(
      Clean_Train_Data_V1$PAID_AMT_AUG)) / (max(Clean_Train_Data_V1$PAID_AMT_AUG) - min(
      Clean_Train_Data_V1$PAID_AMT_AUG))

Clean_Train_Data_V1$PAID_AMT_SEP <- (Clean_Train_Data_V1$PAID_AMT_SEP - min(
      Clean_Train_Data_V1$PAID_AMT_SEP)) / (max(Clean_Train_Data_V1$PAID_AMT_SEP) - min(
      Clean_Train_Data_V1$PAID_AMT_SEP))

Clean_Train_Data_V1$PAID_AMT_OCT <- (Clean_Train_Data_V1$PAID_AMT_OCT - min(
      Clean_Train_Data_V1$PAID_AMT_OCT)) / (max(Clean_Train_Data_V1$PAID_AMT_OCT) - min(
      Clean_Train_Data_V1$PAID_AMT_OCT))

Clean_Train_Data_V1$PAID_AMT_NOV <- (Clean_Train_Data_V1$PAID_AMT_NOV - min(
      Clean_Train_Data_V1$PAID_AMT_NOV)) / (max(Clean_Train_Data_V1$PAID_AMT_NOV) - min(
      Clean_Train_Data_V1$PAID_AMT_NOV))

Clean_Train_Data_V1$PAID_AMT_DEC <- (Clean_Train_Data_V1$PAID_AMT_DEC - min(
      Clean_Train_Data_V1$PAID_AMT_DEC)) / (max(Clean_Train_Data_V1$PAID_AMT_DEC) - min(
      Clean_Train_Data_V1$PAID_AMT_DEC))


## Test Data

##  Balance Limit
Clean_Test_Data_V1$Balance_Limit_V1 <- (Clean_Test_Data_V1$Balance_Limit_V1 - min(
      Clean_Test_Data_V1$Balance_Limit_V1)) / (max(Clean_Test_Data_V1$Balance_Limit_V1) - min(
      Clean_Test_Data_V1$Balance_Limit_V1))

##  Due Amounts

Clean_Test_Data_V1$DUE_AMT_JULY <- (Clean_Test_Data_V1$DUE_AMT_JULY - min(
      Clean_Test_Data_V1$DUE_AMT_JULY)) / (max(Clean_Test_Data_V1$DUE_AMT_JULY) - min(
      Clean_Test_Data_V1$DUE_AMT_JULY))

Clean_Test_Data_V1$DUE_AMT_AUG <- (Clean_Test_Data_V1$DUE_AMT_AUG - min(
      Clean_Test_Data_V1$DUE_AMT_AUG)) / (max(Clean_Test_Data_V1$DUE_AMT_AUG) - min(
      Clean_Test_Data_V1$DUE_AMT_AUG))

Clean_Test_Data_V1$DUE_AMT_SEP <- (Clean_Test_Data_V1$DUE_AMT_SEP - min(
      Clean_Test_Data_V1$DUE_AMT_SEP)) / (max(Clean_Test_Data_V1$DUE_AMT_SEP) - min(
      Clean_Test_Data_V1$DUE_AMT_SEP))

Clean_Test_Data_V1$DUE_AMT_OCT <- (Clean_Test_Data_V1$DUE_AMT_OCT - min(
      Clean_Test_Data_V1$DUE_AMT_OCT)) / (max(Clean_Test_Data_V1$DUE_AMT_OCT) - min(
      Clean_Test_Data_V1$DUE_AMT_OCT))

Clean_Test_Data_V1$DUE_AMT_NOV <- (Clean_Test_Data_V1$DUE_AMT_NOV - min(
      Clean_Test_Data_V1$DUE_AMT_NOV)) / (max(Clean_Test_Data_V1$DUE_AMT_NOV) - min(
      Clean_Test_Data_V1$DUE_AMT_NOV))

Clean_Test_Data_V1$DUE_AMT_DEC <- (Clean_Test_Data_V1$DUE_AMT_DEC - min(
      Clean_Test_Data_V1$DUE_AMT_DEC)) / (max(Clean_Test_Data_V1$DUE_AMT_DEC) - min(
      Clean_Test_Data_V1$DUE_AMT_DEC))


##  Paid Amounts

Clean_Test_Data_V1$PAID_AMT_JULY <- (Clean_Test_Data_V1$PAID_AMT_JULY - min(
      Clean_Test_Data_V1$PAID_AMT_JULY)) / (max(Clean_Test_Data_V1$PAID_AMT_JULY) - min(
      Clean_Test_Data_V1$PAID_AMT_JULY))

Clean_Test_Data_V1$PAID_AMT_AUG <- (Clean_Test_Data_V1$PAID_AMT_AUG - min(
      Clean_Test_Data_V1$PAID_AMT_AUG)) / (max(Clean_Test_Data_V1$PAID_AMT_AUG) - min(
      Clean_Test_Data_V1$PAID_AMT_AUG))

Clean_Test_Data_V1$PAID_AMT_SEP <- (Clean_Test_Data_V1$PAID_AMT_SEP - min(
      Clean_Test_Data_V1$PAID_AMT_SEP)) / (max(Clean_Test_Data_V1$PAID_AMT_SEP) - min(
      Clean_Test_Data_V1$PAID_AMT_SEP))

Clean_Test_Data_V1$PAID_AMT_OCT <- (Clean_Test_Data_V1$PAID_AMT_OCT - min(
      Clean_Test_Data_V1$PAID_AMT_OCT)) / (max(Clean_Test_Data_V1$PAID_AMT_OCT) - min(
      Clean_Test_Data_V1$PAID_AMT_OCT))

Clean_Test_Data_V1$PAID_AMT_NOV <- (Clean_Test_Data_V1$PAID_AMT_NOV - min(
      Clean_Test_Data_V1$PAID_AMT_NOV)) / (max(Clean_Test_Data_V1$PAID_AMT_NOV) - min(
      Clean_Test_Data_V1$PAID_AMT_NOV))

Clean_Test_Data_V1$PAID_AMT_DEC <- (Clean_Test_Data_V1$PAID_AMT_DEC - min(
      Clean_Test_Data_V1$PAID_AMT_DEC)) / (max(Clean_Test_Data_V1$PAID_AMT_DEC) - min(
      Clean_Test_Data_V1$PAID_AMT_DEC))










