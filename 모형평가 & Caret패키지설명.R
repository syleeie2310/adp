#data : https://www.kaggle.com/c/expedia-hotel-recommendations/download/train.csv.gz
install.packages('xgboost')
install.packages('extraTrees')
install.packages('caret')
install.packages('e1071')
install.packages('foreach')
install.packages('AUC')
install.packages('deepnet')

setwd('/Users/syleeie/Downloads/kaggle/expedia')
rm(list=ls())
gc()
library(data.table)
library(rJava)
library(xgboost)
library(ranger)
library(AUC)
library(extraTrees)
library(caret)
library(e1071)
library(foreach)
library(doParallel)
library(randomForest)
library(deepnet)

expedia <- fread("train.csv")
head(expedia)
dim(expedia)
str(expedia)

# 1. Booking 데이터만 필터링
expedia_booking <- subset(expedia, is_booking %in% c(1))
head(expedia_booking)
dim(expedia_booking)
str(expedia_train2)

setkey(expedia_booking, date_time)
tables()
rm(expedia)

# 2. 이진 분류로 변경
expedia_booking$hotel_cluster <- ifelse(expedia_booking$hotel_cluster %in% rep(0:49), 0,1)
expedia_booking$hotel_cluster <- as.factor(expedia_booking$hotel_cluster)
table(expedia_booking$hotel_cluster)
#save(expedia_booking, file='expedia_booking.Rdata')


# 3. 훈련/평가셋으로 나눔 (데이터가 커서 5%만 가지고 실험)
set.seed(2310)
n = nrow(expedia_booking)
idx = sample(1:n, size=floor(0.5*n/10), replace=FALSE)
expedia_booking2 <- expedia_booking[idx,]
dim(expedia_booking2)

set.seed(2310)
n = nrow(expedia_booking2)
idx = sample(1:n, size=floor(7*n/10), replace=FALSE)
expedia_train <- expedia_booking2[idx,]
expedia_vaild <- expedia_booking2[-idx,]
dim(expedia_train)
dim(expedia_vaild)

save(expedia_booking2, file='expedia_booking2.Rdata')

#4-1 모델 학습 ranger 39초, glm 1.1초
#system.time(ranger_expedia <- ranger(hotel_cluster ~ site_name + posa_continent + user_location_country + user_location_region + user_location_city + is_mobile + is_package + channel + srch_adults_cnt + srch_children_cnt + srch_rm_cnt + hotel_continent + hotel_country + hotel_market, data=expedia_train, num.trees = 100))
#system.time(caret_expedia <- train(x=expedia_train[, c('site_name', 'posa_continent', 'user_location_country', 'user_location_region', 'user_location_city', 'is_mobile', 'is_package', 'channel', 'srch_adults_cnt', 'srch_children_cnt', 'srch_rm_cnt', 'hotel_continent', 'hotel_country', 'hotel_market'),with=FALSE], y=expedia_train$hotel_cluster, method='rf', ntree=100, allowParallel=TRUE))
#system.time(extraTrees_expedia <- extraTrees(expedia_train[, c('site_name', 'posa_continent', 'user_location_country', 'user_location_region', 'user_location_city', 'is_mobile', 'is_package', 'channel', 'srch_adults_cnt', 'srch_children_cnt', 'srch_rm_cnt', 'hotel_continent', 'hotel_country', 'hotel_market'),with=FALSE], expedia_train$hotel_cluster, ntree = 100, numThreads=4))

#4-1-1 로지스틱 회귀분석 예제
system.time(glm_expedia <- glm(hotel_cluster ~ site_name + posa_continent + user_location_country + user_location_region + user_location_city + is_mobile + is_package + channel + srch_adults_cnt + srch_children_cnt + srch_rm_cnt + hotel_continent + hotel_country + hotel_market, data=expedia_train, family=binomial(link='logit')))
glmbackward_expedia <- step(glm_expedia, direction = "backward")
str(glmbackward_expedia)
summary(glmbackward_expedia)$aic
summary(glm_expedia)$aic

summary(glm_expedia)

#4-1-2 랜덤포레스트 예제
registerDoParallel(cores = 4)
system.time(rf_expedia <- foreach(ntree=rep(25, 4), .combine=combine, .packages='randomForest') %dopar%
              randomForest(x=expedia_train[, c('site_name', 'posa_continent', 'user_location_country', 'user_location_region', 'user_location_city', 'is_mobile', 'is_package', 'channel', 'srch_adults_cnt', 'srch_children_cnt', 'srch_rm_cnt', 'hotel_continent', 'hotel_country', 'hotel_market'),with=FALSE], y=expedia_train$hotel_cluster, ntree=ntree))
rf_expedia
varImpPlot(rf_expedia)

#ctrl <- trainControl(method="repeatedcv",repeats = 1)
#system.time(caret_expedia <- train(x=expedia_train[, c('site_name', 'posa_continent', 'user_location_country', 'user_location_region', 'user_location_city', 'is_mobile', 'is_package', 'channel', 'srch_adults_cnt', 'srch_children_cnt', 'srch_rm_cnt', 'hotel_continent', 'hotel_country', 'hotel_market'),with=FALSE], y=expedia_train$hotel_cluster, method='knn', trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20, allowParallel=TRUE))
#caret_expedia

#4-1-3 모델 학습 xgboost (매트릭스 변환 필요, numeric 데이터로)
expedia_train2 <- expedia_train[, c('site_name', 'posa_continent', 'user_location_country', 'user_location_region', 'user_location_city', 'is_mobile', 'is_package', 'channel', 'srch_adults_cnt', 'srch_children_cnt', 'srch_rm_cnt', 'hotel_continent', 'hotel_country', 'hotel_market'),with=FALSE]
expedia_vaild2 <- expedia_vaild[, c('site_name', 'posa_continent', 'user_location_country', 'user_location_region', 'user_location_city', 'is_mobile', 'is_package', 'channel', 'srch_adults_cnt', 'srch_children_cnt', 'srch_rm_cnt', 'hotel_continent', 'hotel_country', 'hotel_market'),with=FALSE]
expedia_train3 <- expedia_train2[, lapply(.SD, as.numeric)]
expedia_vaild3 <- expedia_vaild2[, lapply(.SD, as.numeric)]

expedia_dtrain <- xgb.DMatrix(as.matrix(expedia_train3), label = as.numeric(as.character(expedia_train$hotel_cluster)))
expedia_dvaild <- xgb.DMatrix(as.matrix(expedia_vaild3))
param <- list(max.depth = 3, eta = 0.1, silent = 1, objective="binary:logistic")
system.time(bst <- xgb.train(param, expedia_dtrain, nrounds=100))
bst

#4-1-3 xgboost importance plot
names <- dimnames(expedia_dtrain)[[2]]
importance_matrix <- xgb.importance(names, model = bst)
xgb.plot.importance(importance_matrix[1:10,])

#4-1-4 Caret 이용 (nnet, size, decay parameter), 428초
system.time(nnet_expedia <- train(hotel_cluster ~ site_name + posa_continent + user_location_country + user_location_region + user_location_city + is_mobile + is_package + channel + srch_adults_cnt + srch_children_cnt + srch_rm_cnt + hotel_continent + hotel_country + hotel_market, data=expedia_train, 
                                 method='nnet', 
                                 preProcess=c("center","scale"),
                                 allowParallel = T, 
                                 maxit=100, trace=F,
                                 trControl = trainControl(method = "CV", number = 10)))
nnet_expedia

#4-3 모델 예측
#pred_ranger <- predict(ranger_expedia, expedia_vaild2, type='response')
#options( java.parameters = "-Xmx4g" )
gc()

#pred_extraTrees <- predict(extraTrees_expedia, expedia_vaild2, probability=T)
pred_rf <- predict(rf_expedia, expedia_vaild2, "prob")
pred_glm <- predict(glm_expedia, expedia_vaild2, type='response')
pred_xgboost <- predict(bst, expedia_dvaild)
pred_nnet <- predict(nnet_expedia, expedia_vaild2, 'prob')

#4-4 accuracy, roc
auc(accuracy(pred_xgboost, expedia_vaild$hotel_cluster))
auc(accuracy(pred_glm, expedia_vaild$hotel_cluster))
auc(accuracy(pred_rf[,2], expedia_vaild$hotel_cluster))
auc(accuracy(pred_nnet[,2], expedia_vaild$hotel_cluster))

auc(roc(pred_xgboost, expedia_vaild$hotel_cluster))
auc(roc(pred_glm, expedia_vaild$hotel_cluster))
auc(roc(pred_rf[,2], expedia_vaild$hotel_cluster))
auc(roc(pred_nnet[,2], expedia_vaild$hotel_cluster))

confusionMatrix(data = ifelse(pred_xgboost>0.5, 1, 0), reference = expedia_vaild$hotel_cluster)
confusionMatrix(data = ifelse(pred_glm>0.5, 1, 0), reference = expedia_vaild$hotel_cluster)
confusionMatrix(data = ifelse(pred_rf[,2]>0.5, 1, 0), reference = expedia_vaild$hotel_cluster)
confusionMatrix(data = ifelse(pred_nnet[,2]>0.5, 1, 0), reference = expedia_vaild$hotel_cluster)

#4-5 ROC Plot
plot(roc(pred_xgboost, as.factor(expedia_vaild$hotel_cluster)), type="S")
plot(roc(pred_glm, as.factor(expedia_vaild$hotel_cluste)), add=TRUE, col="green")
plot(roc(pred_rf[,2], as.factor(expedia_vaild$hotel_cluste)), add=TRUE, col="red")
plot(roc(pred_nnet[,2], as.factor(expedia_vaild$hotel_cluste)), add=TRUE, col="blue")

legend("bottomright", legend=c("XGboost","GLM", "Random Forest", "nnet"), pch=c(1,1), col=c("black", "green", "red", 'blue'))


#5-1 Parameter CV (Caret package)
fitControl <- trainControl(## 5-fold CV
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  allowParallel = TRUE,
)

xgb_grid_1 <- expand.grid(
  nrounds = c(100,200), 
  max_depth = c(5, 10, 15), 
  eta = 0.001, 
  gamma = 1, 
  colsample_bytree = 1.0, 
  min_child_weight = 1,
  subsample = 1
)

registerDoParallel(cores = 5)
head(iris)

xgboost_cvFit <- train(Species ~ ., 
                       data = iris, 
                       method = "xgbTree", 
                       eval_metric = 'mlogloss',
                       Objective = 'multi:softprob',
                       trControl = fitControl,
                       tuneGrid = xgb_grid_1,
                       verbose = T)

xgboost_cvFit
plot(xgboost_cvFit, metric = "Kappa")


#5-2 Parameter CV (xgboost package)
dim(expedia_dtrain)

xgb.params <- list(
  "objective"  = "binary:logistic"
  , "eval_metric" = "auc"
  , "eta" = 0.07
  , "subsample" = 0.9
  , "colsample_bytree" = 0.8
  , "max_depth" = 4
)

fit.xgb.cv <- xgb.cv(params=xgb.params, data=expedia_dtrain, nrounds=300, nthread=4,
                     nfold=5, prediction=TRUE,
                     verbose=TRUE, showsd=FALSE, print.every.n=10,
                     maximize=FALSE)

str(fit.xgb.cv)

plot(fit.xgb.cv$evaluation_log$train_auc_mean, type="S", col='red')
plot(fit.xgb.cv$evaluation_log$test_auc_mean, type="S", col='blue')
fit.xgb.cv$evaluation_log$test_auc_mean

max_auc = max(fit.xgb.cv$evaluation_log[, test_auc_mean])
max_auc_index = which.max(fit.xgb.cv$evaluation_log[, test_auc_mean])
max_auc
max_auc_index


#6 Caret packages functions
#1. Dummyvars
#install.packages('earth')
library(earth)
data(etitanic)
head(etitanic)

dim(etitanic)
str(etitanic)
dummies <- dummyVars(survived ~ ., data = etitanic)
etitanic2 <- predict(dummies, newdata = etitanic)
dim(etitanic2)

#2 Subsampling
imbal_train <- twoClassSim(10000, intercept = -20, linearVars = 20)
imbal_test  <- twoClassSim(10000, intercept = -20, linearVars = 20)
table(imbal_train$Class)

down_train <- downSample(x = imbal_train[, -ncol(imbal_train)],
                         y = imbal_train$Class)
table(down_train$Class)   

up_train <- upSample(x = imbal_train[, -ncol(imbal_train)],
                     y = imbal_train$Class)                         
table(up_train$Class) 

install.packages('DMwR')
library(DMwR)
smote_train <- SMOTE(Class ~ ., data  = imbal_train)                         
table(smote_train$Class) 

install.packages('ROSE')
library(ROSE)
rose_train <- ROSE(Class ~ ., data  = imbal_train)$data                         
table(rose_train$Class) 
