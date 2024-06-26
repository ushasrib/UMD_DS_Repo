setwd("C:/Users/bhoga/Desktop/Desktop/UMD FALL'23/INST737/Milestone-3/M3.Data.Files/")
install.packages("kernlab")
install.packages("mlr")
install.packages("data.table")
install.packages("mltools")
install.packages("dummies")
install.packages("e1071")
install.packages("neuralnet")

library(caret)
library(kernlab)
library(dplyr)
library(mlr)
library(data.table)
library(mltools)
library(dummies)


### Base file loading to derive MS3 Analysis file###
data <- read.csv("New_FM2022Q4.csv")
data <- data[sample(nrow(data)), ]
set.seed(123)
# Splitting the data into three subsets based on the outcome classes
data_P <- data[data$Occupancy.Status == 'P', ]
data_I <- data[data$Occupancy.Status == 'I', ]
data_S <- data[data$Occupancy.Status == 'S', ]
# Sampling equal number of observations from each subset
samples_per_class <- 920
sampled_data <- rbind(
  data_P[1:samples_per_class, ],
  data_I[1:samples_per_class, ],
  data_S[1:samples_per_class, ]
)
dim(sampled_data)
View(sampled_data)
sampled_data <- sampled_data[sample(nrow(sampled_data)), ]
View(sampled_data)
prop.table(table(sampled_data$Occupancy.Status))
glimpse(data)
### subsetting non-multicollinear variables#########
sampled_data <- subset(sampled_data,
                       select = c(Original.Interest.Rate,
                                  bondRate,
                                  fedFundsRate,
                                  Borrower.Credit.Score.at.Origination,
                                  Original.UPB,
                                  Number.of.Borrowers,
                                  Original.Loan.to.Value.Ratio..LTV.,
                                  Original.Combined.Loan.to.Value.Ratio..CLTV.,
                                  Debt.To.Income..DTI., 
                                  Seller.Name,
                                  Loan.Purpose,
                                  Property.Type,
                                  Property.State,
                                  Occupancy.Status))
write.csv(sampled_data, file = "MS3sample.csv", row.names = FALSE)
data <- read.csv("MS3sample.csv")
dim(data)
prop.table(table(data$Occupancy.Status))

##### creating an encoded dataset (except outcome variable)
data$Seller.Name <- as.factor(data$Seller.Name)
levels(data$Seller.Name)
data$Property.State <- as.factor(data$Property.State)

data$Property.Type <- as.factor(data$Property.Type)
data$Loan.Purpose <- as.factor(data$Loan.Purpose)
data$Occupancy.Status <- data$Occupancy.Status

##"CitiMortgage, Inc." is the reference level
dim(data)

dmy <-dummyVars(~Seller.Name, data = data, fullRank = T)
encoded_data <- data.frame(predict(dmy, newdata= data))
dim(encoded_data)

dmy1 <-dummyVars(~Property.Type, data = data, fullRank = T)
encoded_data1 <- data.frame(predict(dmy1, newdata= data))
dim(encoded_data1)

dmy2 <-dummyVars(~Property.State, data = data, fullRank = T)
encoded_data2 <- data.frame(predict(dmy2, newdata= data))
dim(encoded_data2)

dmy3 <-dummyVars(~Loan.Purpose, data = data, fullRank = T)
encoded_data3 <- data.frame(predict(dmy3, newdata= data))
dim(encoded_data3)



encoded_MS3dataset <- cbind(data, encoded_data,
                            encoded_data1,
                            encoded_data2,
                            encoded_data3)



dim(encoded_MS3dataset)
glimpse(encoded_MS3dataset)

write.csv(encoded_MS3dataset, file = "encoded_MS3sample.csv", row.names = FALSE)
data <- read.csv("encoded_MS3sample.csv")
dim(data)
glimpse(data)
## REMOVING ORIGINAL COLUMNS AS WE HAVE ENCODED COLUMNS
data <- data[, -which(names(data)== 'Seller.Name')]
dim(data)

data <- data[, -which(names(data)== 'Property.Type')]
dim(data)

data <- data[, -which(names(data)== 'Property.State')]
dim(data)

data <- data[, -which(names(data)== 'Loan.Purpose')]
dim(data)
write.csv(data, file = "final_MS3dataset.csv", row.names = FALSE)

#### CREATING TRAINING AND TEST DATASETS FOR SVM MODELS
data <- read.csv("final_MS3dataset.csv")
dim(data)

data$Occupancy.Status <- as.factor(data$Occupancy.Status)
str(data)

#########SPLITTING DATA

data <- data[sample(nrow(data)), ]
set.seed(123)
train <- data[1:2070, ]
dim(train)
test <- data[2071:2760, ]
dim(test)

#### TRAINING LINEAR SVM CLASSIFIER USING VANILLADOT KERNEL
library(kernlab)
svm_classifier <- ksvm(Occupancy.Status~.,
                       data = train, 
                       kernel = "vanilladot")
svm_classifier
#PREDICTING USING THE CLASSIFIER
svm_predictions <- predict(svm_classifier,test)
head(svm_predictions)
svm_predictions

####confusion matrix
table(svm_predictions, test$Occupancy.Status)
#####count 
agreement <- svm_predictions == test$Occupancy.Status
table(agreement)
prop.table(table(agreement))

confusion_matrix <- confusionMatrix(svm_predictions, test$Occupancy.Status)
confusion_matrix


## CALCULATING F1 MEASURE
#F1 <- 2*(Precision * Recall)/(Precision + Recall)
#Precision
p.i <- 0.5726
p.p <- 0.6933
p.s <-0.5403
#Recall
r.i <- 0.5923
r.p <- 0.7237
r.s <- 0.4978

f1_i <- 2*(p.i * r.i)/(p.i + r.i)
f1_i
f1_p <- 2*(p.p * r.p)/(p.p + r.p)
f1_p
f1_s <- 2*(p.s * r.s)/(p.s + r.s)
f1_s

###USING RBF KERNEL

svm_classifier_rbf <-ksvm(Occupancy.Status~., data = train, kernel = "rbfdot")
svm_classifier_rbf
##PREDICTING WITH RBF CLASSIFIER
svm_predictions_rbf <- predict(svm_classifier_rbf, test)
svm_predictions_rbf

agreement_rbf <-svm_predictions_rbf == test$Occupancy.Status
table(agreement_rbf)#Number of TRUE is more
prop.table(table(agreement_rbf))#TRUE percentage increased


confusion_matrix_rbf <- confusionMatrix(svm_predictions_rbf, test$Occupancy.Status)
confusion_matrix_rbf


######################################################################

#### NEURAL NETWORKS TO TRAIN FOR REGRESSION QUESTION#########
data <- read.csv("final_MS3dataset.csv")
glimpse(data)
#Above encoded dataset does not have encoding for Occupancy Status
dmy4 <-dummyVars(~Occupancy.Status, data = data, fullRank = T) #I is taken as reference class
encoded_data4 <- data.frame(predict(dmy4, newdata= data))
dim(encoded_data4)
View(encoded_data4)#Has only 2 classes encoded, one is reference class
encoded_MS3dataset_sn <- cbind(data, encoded_data4)
dim(encoded_MS3dataset_sn)
encoded_MS3dataset_sn <- encoded_MS3dataset_sn[, -which(names(encoded_MS3dataset_sn)== 'Occupancy.Status')]
dim(encoded_MS3dataset_sn)

write.csv(encoded_MS3dataset_sn, file = "all_cat_encoded_MS3dataset.csv", row.names = FALSE)
data <- read.csv("all_cat_encoded_MS3dataset.csv")

dim(data)

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

###applying the function to the numeric variables

# Identify the numeric columns in your dataset
numeric_cols <- sapply(data, is.numeric)

# Normalize only the numeric columns
data[, numeric_cols] <- lapply(data[, numeric_cols], normalize)

View(data)
write.csv(data, file = "Normalized_MS3dataset.csv", row.names = FALSE)
data <- read.csv("Normalized_MS3dataset.csv")
dim(data)

##SPLITTING DATA INTO TRAINING AND TEST DATASETS

train_NN <- data[1:2070, ]
test_NN <- data[2071:2760, ]
str(train_NN)
library(neuralnet)
########## USING THE SAME SET OF VARIABLES AS THE BEST MULTIVARIATE MODEL

model_NN_4var_SN <- neuralnet(formula = Original.Interest.Rate ~ bondRate + fedFundsRate +
                                Borrower.Credit.Score.at.Origination +
                                Original.UPB +
                                Seller.Name.CrossCountry.Mortgage..LLC +
                                Seller.Name.DHI.Mortgage.Company..Ltd. + 
                                Seller.Name.Fairway.Independent.Mortgage.Corporation +
                                Seller.Name.Fifth.Third.Bank..National.Association + 
                                Seller.Name.Guaranteed.Rate..Inc.+
                                Seller.Name.Guild.Mortgage.Company.LLC +
                                Seller.Name.JPMorgan.Chase.Bank..National.Association +
                                Seller.Name.Lakeview.Loan.Servicing..LLC+
                                Seller.Name.Lennar.Mortgage..LLC +
                                Seller.Name.loanDepot.com..LLC +
                                Seller.Name.Movement.Mortgage..LLC +
                                Seller.Name.NationStar.Mortgage..LLC +
                                Seller.Name.NewRez.LLC +
                                Seller.Name.NexBank +
                                Seller.Name.Other +
                                Seller.Name.PennyMac.Corp.+
                                Seller.Name.PennyMac.Loan.Services..LLC +
                                Seller.Name.PHH.Mortgage.Corporation +
                                Seller.Name.Planet.Home.Lending..LLC +
                                Seller.Name.Rocket.Mortgage..LLC +
                                Seller.Name.Truist.Bank..formerly.SunTrust.Bank.+
                                Seller.Name.U.S..Bank.N.A.+
                                Seller.Name.United.Wholesale.Mortgage..LLC +
                                Seller.Name.Wells.Fargo.Bank..N.A.,
                              data = train_NN)

plot(model_NN_4var_SN)
View(test_NN)
model_NN_4var_SN_results <-compute(model_NN_4var_SN,test_NN[, c(2:5,10:33)])
predicted_IR_4var_SN <- model_NN_4var_SN_results$net.result
cor(predicted_IR_4var_SN, test_NN$Original.Interest.Rate)
mse <- mean((predicted_IR_4var_SN - test_NN$Original.Interest.Rate)^2)
mse
rmse <- sqrt(mse)
rmse


##########using ALL THE NUMERIC variables IN THE DATASET# ##########
glimpse(train_NN)
model_NN_numvar <- neuralnet(formula = Original.Interest.Rate ~ bondRate+fedFundsRate+
                               Borrower.Credit.Score.at.Origination +
                               Original.UPB+
                               Number.of.Borrowers+
                               Original.Loan.to.Value.Ratio..LTV.+
                               Original.Combined.Loan.to.Value.Ratio..CLTV.+
                               Debt.To.Income..DTI.,
                             data = train_NN)
plot(model_NN_numvar)
glimpse(test_NN)
View(test_NN)
model_NN_numvar_results <-compute(model_NN_numvar,test_NN[2:9])
model_NN_numvar_results
model_NN_numvar_results$net.result #Gives predicted values
predicted_IR_numvar <- model_NN_numvar_results$net.result

cor(predicted_IR_numvar, test_NN$Original.Interest.Rate)
mse <- mean((predicted_IR_numvar - test_NN$Original.Interest.Rate)^2)
mse
rmse <- sqrt(mse)
rmse

##### USING SELLER NAME VARIABLE along with numeric variable from best multivariate model#######
glimpse(train_NN)
model_NN_SN <- neuralnet(formula = Original.Interest.Rate ~ bondRate + fedFundsRate +
                           Borrower.Credit.Score.at.Origination +
                           Original.UPB +
                           Number.of.Borrowers +
                           Original.Loan.to.Value.Ratio..LTV. +
                           Original.Combined.Loan.to.Value.Ratio..CLTV. +
                           Debt.To.Income..DTI. +
                           Seller.Name.CrossCountry.Mortgage..LLC +
                           Seller.Name.DHI.Mortgage.Company..Ltd. + 
                           Seller.Name.Fairway.Independent.Mortgage.Corporation +
                           Seller.Name.Fifth.Third.Bank..National.Association + 
                           Seller.Name.Guaranteed.Rate..Inc.+
                           Seller.Name.Guild.Mortgage.Company.LLC +
                           Seller.Name.JPMorgan.Chase.Bank..National.Association +
                           Seller.Name.Lakeview.Loan.Servicing..LLC+
                           Seller.Name.Lennar.Mortgage..LLC +
                           Seller.Name.loanDepot.com..LLC +
                           Seller.Name.Movement.Mortgage..LLC +
                           Seller.Name.NationStar.Mortgage..LLC +
                           Seller.Name.NewRez.LLC +
                           Seller.Name.NexBank +
                           Seller.Name.Other +
                           Seller.Name.PennyMac.Corp.+
                           Seller.Name.PennyMac.Loan.Services..LLC +
                           Seller.Name.PHH.Mortgage.Corporation +
                           Seller.Name.Planet.Home.Lending..LLC +
                           Seller.Name.Rocket.Mortgage..LLC +
                           Seller.Name.Truist.Bank..formerly.SunTrust.Bank.+
                           Seller.Name.U.S..Bank.N.A.+
                           Seller.Name.United.Wholesale.Mortgage..LLC +
                           Seller.Name.Wells.Fargo.Bank..N.A.,
                         data = train_NN)

plot(model_NN_SN)

model_NN_SN_results <-compute(model_NN_SN,test_NN[2:33])
predicted_IR_SN <- model_NN_SN_results$net.result
cor(predicted_IR_SN, test_NN$Original.Interest.Rate)
mse <- mean((predicted_IR_SN - test_NN$Original.Interest.Rate)^2)
mse
rmse <- sqrt(mse)
rmse



## model nn all variables ########## BEST MODEL####
library(neuralnet)
model_NN <- neuralnet(formula = Original.Interest.Rate~.,data = train_NN)
model_NN
model_NN$act.fct
model_NN$err.fct
# THE MODEL OUTPUT WAS LARGE AND COULD NOT BE SEEN IN THE CONSOLE, capturing to afile
sink("model_NN_output.txt")
model_NN
sink()

# Viewing the content of the file
cat(readLines("model_NN_output.txt"), sep = "\n")


plot(model_NN)
model_NN_results <-compute(model_NN,test_NN[2:93])
model_NN_results
model_NN_results$net.result #Gives predicted values
predicted_IR <- model_NN_results$net.result

cor(predicted_IR, test_NN$Original.Interest.Rate)
mse <- mean((predicted_IR - test_NN$Original.Interest.Rate)^2)
mse
rmse <- sqrt(mse)
rmse
summary(model_NN)

# Calculation of residuals (errors) and sse
residuals <- predicted_IR - test_NN$Original.Interest.Rate
sse <- sum(residuals^2)
sse


###### USING HIDDEN LAYERS=2 specified by c(3,3)###############
model_NN_H2 <- neuralnet(formula = Original.Interest.Rate~.,data = train_NN, hidden=c(3,3))
model_NN_H2_results <- compute(model_NN_H2, test_NN[2:93])
predicted_IR_H2 <- model_NN_H2_results$net.result
cor(predicted_IR_H2, test_NN$Original.Interest.Rate)
mse <- mean((predicted_IR_H2 - test_NN$Original.Interest.Rate)^2)
mse
rmse <- sqrt(mse)
rmse

plot(model_NN_H2)
# Calculation of residuals (errors) and sse
residuals <- predicted_IR_H2 - test_NN$Original.Interest.Rate
sse <- sum(residuals^2)
sse


###### USING HIDDEN LAYERS=3 specified by c(1,2,3)###############
model_NN_H3 <- neuralnet(formula = Original.Interest.Rate~.,data = train_NN, hidden=c(3,2,1))
model_NN_H3_results <- compute(model_NN_H3, test_NN[2:93])
predicted_IR_H3 <- model_NN_H3_results$net.result
cor(predicted_IR_H3, test_NN$Original.Interest.Rate)
mse <- mean((predicted_IR_H3 - test_NN$Original.Interest.Rate)^2)
mse
rmse <- sqrt(mse)
rmse

plot(model_NN_H3)
residuals <- predicted_IR_H3 - test_NN$Original.Interest.Rate
sse <- sum(residuals^2)
sse

###### USING HIDDEN LAYERS=3 specified by c(2,1,1)###############
model_NN_H3 <- neuralnet(formula = Original.Interest.Rate~.,data = train_NN, hidden=c(2,1,1))
model_NN_H3_results <- compute(model_NN_H3, test_NN[2:93])
predicted_IR_H3 <- model_NN_H3_results$net.result
cor(predicted_IR_H3, test_NN$Original.Interest.Rate)
mse <- mean((predicted_IR_H3 - test_NN$Original.Interest.Rate)^2)
mse
rmse <- sqrt(mse)
rmse
plot(model_NN_H3)
residuals <- predicted_IR_H3 - test_NN$Original.Interest.Rate
sse <- sum(residuals^2)
sse


###### USING HIDDEN LAYERS=3 specified by c(1,1,1)###############
model_NN_H3 <- neuralnet(formula = Original.Interest.Rate~.,data = train_NN, hidden=c(1,1,1))
model_NN_H3_results <- compute(model_NN_H3, test_NN[2:93])
predicted_IR_H3 <- model_NN_H3_results$net.result
cor(predicted_IR_H3, test_NN$Original.Interest.Rate)
mse <- mean((predicted_IR_H3 - test_NN$Original.Interest.Rate)^2)
mse
rmse <- sqrt(mse)
rmse
plot(model_NN_H3)
residuals <- predicted_IR_H3 - test_NN$Original.Interest.Rate
sse <- sum(residuals^2)
sse

###### USING HIDDEN LAYERS=4 specified by c(1,1,1,1)###############
model_NN_H4 <- neuralnet(formula = Original.Interest.Rate~.,data = train_NN, hidden=c(1,1,1,1))
model_NN_H4_results <- compute(model_NN_H4, test_NN[2:93])
predicted_IR_H4 <- model_NN_H4_results$net.result
cor(predicted_IR_H4, test_NN$Original.Interest.Rate)
mse <- mean((predicted_IR_H4 - test_NN$Original.Interest.Rate)^2)
mse
rmse <- sqrt(mse)
rmse
residuals <- predicted_IR_H4 - test_NN$Original.Interest.Rate
sse <- sum(residuals^2)
sse
plot(model_NN_H4)

###### USING HIDDEN LAYERS=5 specified by c(1,1,1,1,1)###############
model_NN_H5 <- neuralnet(formula = Original.Interest.Rate~.,data = train_NN, hidden=c(1,1,1,1,1))
model_NN_H5_results <- compute(model_NN_H5, test_NN[2:93])
predicted_IR_H5 <- model_NN_H5_results$net.result
cor(predicted_IR_H5, test_NN$Original.Interest.Rate)
mse <- mean((predicted_IR_H5 - test_NN$Original.Interest.Rate)^2)
mse
rmse <- sqrt(mse)
rmse
residuals <- predicted_IR_H5 - test_NN$Original.Interest.Rate
sse <- sum(residuals^2)
sse
plot(model_NN_H5)











########## CLUSTERING##################









