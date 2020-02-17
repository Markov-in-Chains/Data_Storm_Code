############################################################################################
###   Model Name              : Credit_Card_Default_Rates                                ###
###   Purpose of the script   : Predictions                                              ###
###   Model Owners            : Dinelka Nanayakkara, Manosha Hansini, Ravindi Wijeratne  ###
###   Model Development Date  : 2020-02-15                                               ###
###   Model Version           : Version_01                                               ###
############################################################################################

############################################################################################
##  Building the model using a Random Forest Model  ########################################
############################################################################################

rf_model <- randomForest(factor(NEXT_MONTH_DEFAULT) ~ Balance_Limit_V1 + Gender_Num + 
                           Education_Num + Marital_Num + PAY_JULY + PAY_AUG + 
                           PAY_SEP + PAY_OCT + PAY_NOV + PAY_DEC, data =  Clean_Train_Data_V1)

colnames(Clean_Train_Data_V1)

Prediction <- predict(rf_model, Clean_Test_Data_V1)
Solution   <- data.frame(Clean_Test_Data_V1$Client_ID, Prediction)

write.csv(Solution, file= 'rf_model.csv', row.names = F)

table(Solution$Prediction)