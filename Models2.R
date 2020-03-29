#Put Necessary Libraries Here
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(dplyr)
library(leaps)
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(modelr)
library(glmnet)
library(aod)
library(e1071)  # For svm
library(pls)
library(randomForest)
library(mlr)
library(caret)
library(keras)

#Load cleaned data from Variable_Addition.R
nhl.clean=read.csv('nhl-game-data/cleaned_NHL.csv')
pregame.data = select(nhl.clean, Type, game_id, team_id, Season, away_team_id, home_team_id, first_goal_win, game_type, away_team_name, home_team_name, contains("avg"), away_rest, home_rest, away_game_in_week, home_game_in_week, away_last5_win, home_last5_win, away_last10_win, home_last10_win, away_last20_win, home_last20_win, away_win_percent, home_win_percent)
pregame.data$first_goal_win = as.factor(pregame.data$first_goal_win)
str(pregame.data)
#Make each game a single line

away = filter(pregame.data, Type == "Away")
home = filter(pregame.data, Type != "Away") %>%
  dplyr::select(Type, game_id, contains("avg"))

singleline20 = na.omit(left_join(away, home, by = "game_id"))
singleline10 = left_join(away, home, by = "game_id") %>% dplyr::select(-c(contains("20")))
singleline10 = na.omit(singleline10)
singleline5 = left_join(away, home, by = "game_id")  %>% dplyr::select(-c(contains("20"), contains("10")))
singleline5 = na.omit(singleline5)
dif20 = singleline20
dif20$rest_dif = dif20$away_rest - dif20$home_rest
dif20$game_week_dif = dif20$away_game_in_week - dif20$home_game_in_week
dif20$last5dif = dif20$away_last5_win - dif20$home_last5_win
dif20$last10dif = dif20$away_last10_win - dif20$home_last10_win
dif20$last20dif = dif20$away_last20_win - dif20$home_last20_win
dif20$win_percent_dif = dif20$away_win_percent - dif20$home_win_percent

a = data.frame(singleline20[,11:74]-singleline20[,88:151])

dif20[,88:151] = a

finalclean.func = function(data){
  data = dplyr::select(data, -c(Type.x, game_id, team_id, away_team_id, home_team_id, Season, home_team_name, away_team_name, Type.y, contains("home")))
}
singleline20 = finalclean.func(singleline20)
singleline10 = finalclean.func(singleline10)
singleline5 = finalclean.func(singleline5)
dif20 = finalclean.func(dif20)

train.dif = sample(1:dim(dif20)[1], dim(dif20)[1]/3*2)
traindif = dif20[train.dif,]
testdif = dif20[-train.dif,]

difference = dif20[,73:142]
difference$first_goal_win = dif20$first_goal_win
train.dif1 = sample(1:dim(difference)[1], dim(difference)[1]/3*2)
traindif1 = difference[train.dif1,]
testdif1 = difference[-train.dif1,]

train.full = sample(1:dim(singleline20)[1], dim(singleline20)[1]/100*75)
train20 = singleline20[train.full,]
test20 = singleline20[-train.full,]


train.10 = sample(1:dim(singleline10)[1], dim(singleline10)[1]/100*75)
train10 = singleline10[train.10,]
test10 = singleline10[-train.10,]

train.5 = sample(1:dim(singleline5)[1], dim(singleline5)[1]/100*75)
train5 = singleline5[train.5,]
test5 = singleline5[-train.5,]

#All tasks with avg20 stats
traindif = makeClassifTask(data = train, target = "first_goal_win", positive = "1")
testdif = makeClassifTask(data = testdif, target = "first_goal_win", positive = "1")

traindif = normalizeFeatures(traindif,method = "standardize")
testdif = normalizeFeatures(testdif,method = "standardize")

library(FSelector)
important20 = generateFilterValuesData(traindif, method = "information.gain")
plotFilterValues(important20, n.show = 100)

#QDA
qda.learner = makeLearner("classif.qda", predict.type = "response")
qda.model = mlr::train(qda.learner, traindif)
qda.predict = predict(qda.model, testdif)
QDA20 = confusionMatrix(qda.predict$data$response, testdif$env$data$first_goal_win)

#Logistic Regression
logistic.learner = makeLearner("classif.logreg",predict.type = "response")
cv.logistic = crossval(learner = logistic.learner,task = traindif,iters = 5,stratify = TRUE,measures = acc,show.info = T)
logit.mod = mlr::train(logistic.learner, traindif)
getLearnerModel(logit.mod)
logit.pred = predict(logit.mod, testdif)
logit20 = confusionMatrix(logit.pred$data$response, testdif$env$data$first_goal_win)

#Decision Tree
library(rpart)
getParamSet("classif.rpart")
tree = makeLearner("classif.rpart", predict.type = "response")
set_cv = makeResampleDesc("CV",iters = 5L)
tree.param = makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2))
gscontrol = makeTuneControlGrid()
tuned = tuneParams(learner = tree, resampling = set_cv, task = traindif, par.set = tree.param, control = gscontrol, measures = acc)
tree.param = setHyperPars(tree, par.vals = tuned$x)
tree.full = mlr::train(tree.param, traindif)
getLearnerModel(tree.full)
dtfull.pred = predict(tree.full, testdif)

decisionT20 = confusionMatrix(dtfull.pred$data$response, testdif$env$data$first_goal_win)
#Random Forest
getParamSet("classif.randomForest")

forestLearner = makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
forestLearner$par.vals = list(importance = T)

rf_param = makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)
randcontrol= makeTuneControlRandom(maxit = 50L)
set.cv = makeResampleDesc("CV", iters = 3L)
rf.tuned = tuneParams(learner = forestLearner, resampling = set.cv, task = traindif, par.set = rf_param, control = randcontrol, measures = acc)

rf.tree = setHyperPars(forestLearner, par.vals = rf.tuned$x)
rf.trained = mlr::train(rf.tree, traindif)

getLearnerModel(rf.trained)

rf.pred = predict(rf.trained, testdif)
randfor20 = confusionMatrix(rf.pred$data$response, testdif$env$data$first_goal_win)

#Support Vector Machines
install.packages(kernlab)
library(kernlab)
library(gbm)
library(xgboost)

#load svm
svm = makeLearner("classif.ksvm", predict.type = "response")
#Set parameters
param.svm = makeParamSet(
  makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), #cost parameters
  makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4)) #RBF Kernel Parameter
)

#specify search function
control = makeTuneControlGrid()

#tune model
svm.tuned = tuneParams(svm, task = traindif, resampling = set.cv, par.set = param.svm, control = control,measures = acc)

#set the model with best params
set.svm = setHyperPars(svm, par.vals = svm.tuned$x)

#train
trained.svm = mlr::train(svm, traindif)

#test
svm.pred = predict(trained.svm, testdif)
svm20 = confusionMatrix(svm.pred$data$response, testdif$env$data$first_goal_win)


#gbm
g.gbm = makeLearner("classif.gbm", predict.type = "response")

#specify tuning method
rancontrol = makeTuneControlRandom(maxit = 50L)

#3 fold cross validation
set_cv = makeResampleDesc("CV",iters = 3L)

#parameters
gbm_par= makeParamSet(
  makeDiscreteParam("distribution", values = "bernoulli"),
  makeIntegerParam("n.trees", lower = 100, upper = 1000), #number of trees
  makeIntegerParam("interaction.depth", lower = 2, upper = 10), #depth of tree
  makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
  makeNumericParam("shrinkage",lower = 0.01, upper = 1)
)

#tune parameters
tune_gbm = tuneParams(learner = g.gbm, task = traindif,resampling = set_cv,measures = acc,par.set = gbm_par,control = rancontrol)

#set parameters
final_gbm = setHyperPars(learner = g.gbm, par.vals = tune_gbm$x)

#train
to.gbm = mlr::train(final_gbm, traindif)

#test
pr.gbm = predict(to.gbm, testdif)
gbm20 = confusionMatrix(pr.gbm$data$response, testdif$env$data$first_goal_win)

#XGBoost

#load xgboost
set.seed(1001)
getParamSet("classif.xgboost")

#make learner with inital parameters
xg_set = makeLearner("classif.xgboost", predict.type = "response")
xg_set$par.vals = list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 250
)

#define parameters for tuning
xg_ps = makeParamSet(
  makeIntegerParam("nrounds",lower=200,upper=600),
  makeIntegerParam("max_depth",lower=3,upper=20),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("eta", lower = 0.001, upper = 0.5),
  makeNumericParam("subsample", lower = 0.10, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)

#define search function
rancontrol = makeTuneControlRandom(maxit = 100L) #do 100 iterations

#3 fold cross validation
set_cv = makeResampleDesc("CV",iters = 3L)

#tune parameters
xg_tune = tuneParams(learner = xg_set, task = traindif, resampling = set_cv,measures = acc,par.set = xg_ps, control = rancontrol)

#set parameters
xg_new = setHyperPars(learner = xg_set, par.vals = xg_tune$x)

#train model
xgmodel = mlr::train(xg_new, traindif)

#test model
predict.xg = predict(xgmodel, testdif)
xgb20 = confusionMatrix(predict.xg$data$response, testdif$env$data$first_goal_win)

#CNN
X_train <- traindif %>% 
  select(-first_goal_win) %>% 
  scale()

y_train <- to_categorical(traindif$first_goal_win)

X_test <- testdif %>% 
  select(-first_goal_win) %>% 
  scale()

y_test <- to_categorical(testdif$first_goal_win)

model <- keras_model_sequential() 

model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = ncol(X_train)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

model %>% fit(
  X_train, y_train, 
  epochs = 100, 
  batch_size = 5,
  validation_split = 0.3
)

summary(model)

model %>% evaluate(X_test, y_test)


plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="orange", type="l")
lines(history$metrics$val_loss, col="skyblue")
legend("topright", c("Training","Testing"), col=c("orange", "skyblue"), lty=c(1,1))


plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="orange", type="l")
lines(history$metrics$val_acc, col="skyblue")
legend("topleft", c("Training","Testing"), col=c("orange", "skyblue"), lty=c(1,1))

predictions <- model %>% predict_classes(X_test)

confusionMatrix()


##repeat for 10 game variables
traindif1 = makeClassifTask(data = traindif1, target = "first_goal_win", positive = "1")
testdif1 = makeClassifTask(data = testdif1, target = "first_goal_win", positive = "1")

traindif1 = normalizeFeatures(traindif1,method = "standardize")
testdif1 = normalizeFeatures(testdif1,method = "standardize")

library(FSelector)
important = generateFilterValuesData(traindif1, method = "information.gain")
plotFilterValues(important, n.show = 100)


#QDA
qda.learner = makeLearner("classif.qda", predict.type = "response")
qda.model = mlr::train(qda.learner, traindif1)
qda.predict = predict(qda.model, testdif1)
QDA10 = confusionMatrix(qda.predict$data$response, testdif1$env$data$first_goal_win)

#Logistic Regression
logistic.learner = makeLearner("classif.logreg",predict.type = "response")
cv.logistic = crossval(learner = logistic.learner,task = traindif1,iters = 5,stratify = TRUE,measures = acc,show.info = T)
logit.mod = mlr::train(logistic.learner, traindif1)
getLearnerModel(logit.mod)
logit.pred = predict(logit.mod, testdif1)
logit10 = confusionMatrix(logit.pred$data$response, testdif1$env$data$first_goal_win)

#Decision Tree
library(rpart)
getParamSet("classif.rpart")
tree = makeLearner("classif.rpart", predict.type = "response")
set_cv = makeResampleDesc("CV",iters = 5L)
tree.param = makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2))
gscontrol = makeTuneControlGrid()
tuned = tuneParams(learner = tree, resampling = set_cv, task = traindif1, par.set = tree.param, control = gscontrol, measures = acc)
tree.param = setHyperPars(tree, par.vals = tuned$x)
tree.full = mlr::train(tree.param, traindif1)
getLearnerModel(tree.full)
dtfull.pred = predict(tree.full, testdif1)

decisionT10=confusionMatrix(dtfull.pred$data$response, testdif1$env$data$first_goal_win)
#Random Forest
getParamSet("classif.randomForest")

forestLearner = makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 100, mtry = 3))
forestLearner$par.vals = list(importance = T)

rf_param = makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)
randcontrol= makeTuneControlRandom(maxit = 50L)
set.cv = makeResampleDesc("CV", iters = 3L)
rf.tuned = tuneParams(learner = forestLearner, resampling = set.cv, task = traindif1, par.set = rf_param, control = randcontrol, measures = acc)

rf.tree = setHyperPars(forestLearner, par.vals = rf.tuned$x)
rf.trained = mlr::train(rf.tree, traindif1)

getLearnerModel(rf.trained)

rf.pred = predict(rf.trained, testdif1)
randfor10=confusionMatrix(rf.pred$data$response, testdif1$env$data$first_goal_win)

#load svm
svm = makeLearner("classif.ksvm", predict.type = "response")

#Set parameters
param.svm = makeParamSet(
  makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), #cost parameters
  makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4)) #RBF Kernel Parameter
)

#specify search function
control = makeTuneControlGrid()

#tune model
svm.tuned = tuneParams(svm, task = traindif1, resampling = set.cv, par.set = param.svm, control = control,measures = acc)

#set the model with best params
set.svm = setHyperPars(svm, par.vals = svm.tuned$x)

#train
trained.svm = mlr::train(svm, traindif1)

#test
svm.pred = predict(trained.svm, testdif1)
svm10 = confusionMatrix(svm.pred$data$response, testdif1$env$data$first_goal_win)


#gbm
g.gbm = makeLearner("classif.gbm", predict.type = "response")

#specify tuning method
rancontrol = makeTuneControlRandom(maxit = 50L)

#3 fold cross validation
set_cv = makeResampleDesc("CV",iters = 3L)

#parameters
gbm_par= makeParamSet(
  makeDiscreteParam("distribution", values = "bernoulli"),
  makeIntegerParam("n.trees", lower = 100, upper = 1000), #number of trees
  makeIntegerParam("interaction.depth", lower = 2, upper = 10), #depth of tree
  makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
  makeNumericParam("shrinkage",lower = 0.01, upper = 1)
)

#tune parameters
tune_gbm = tuneParams(learner = g.gbm, task = traindif1,resampling = set_cv,measures = acc,par.set = gbm_par,control = rancontrol)

#set parameters
final_gbm = setHyperPars(learner = g.gbm, par.vals = tune_gbm$x)

#train
to.gbm = mlr::train(final_gbm, traindif1)

#test
pr.gbm = predict(to.gbm, testdif1)
gbm10=confusionMatrix(pr.gbm$data$response, testdif1$env$data$first_goal_win)

#XGBoost

#load xgboost
set.seed(1001)
getParamSet("classif.xgboost")

#make learner with inital parameters
xg_set = makeLearner("classif.xgboost", predict.type = "response")
xg_set$par.vals = list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 250
)

#define parameters for tuning
xg_ps = makeParamSet(
  makeIntegerParam("nrounds",lower=200,upper=600),
  makeIntegerParam("max_depth",lower=3,upper=20),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("eta", lower = 0.001, upper = 0.5),
  makeNumericParam("subsample", lower = 0.10, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)

#define search function
rancontrol = makeTuneControlRandom(maxit = 100L) #do 100 iterations

#3 fold cross validation
set_cv = makeResampleDesc("CV",iters = 3L)

#tune parameters
xg_tune = tuneParams(learner = xg_set, task = traindif1, resampling = set_cv,measures = acc,par.set = xg_ps, control = rancontrol)

#set parameters
xg_new = setHyperPars(learner = xg_set, par.vals = xg_tune$x)

#train model
xgmodel = mlr::train(xg_new, traindif1)

#test model
predict.xg = predict(xgmodel, testdif1)
xgb10=confusionMatrix(predict.xg$data$response, testdif1$env$data$first_goal_win)

##repeat for 5 game variables
train5 = makeClassifTask(data = train5, target = "first_goal_win", positive = "1")
test5 = makeClassifTask(data = test5, target = "first_goal_win", positive = "1")

train5 = normalizeFeatures(train5,method = "standardize")
test5 = normalizeFeatures(test5,method = "standardize")

library(FSelector)
important = generateFilterValuesData(train5, method = "information.gain")
plotFilterValues(important, n.show = 100)


#QDA
qda.learner = makeLearner("classif.qda", predict.type = "response")
qda.model = mlr::train(qda.learner, train5)
qda.predict = predict(qda.model, test5)
QDA5 = confusionMatrix(qda.predict$data$response, test5$env$data$first_goal_win)

#Logistic Regression
logistic.learner = makeLearner("classif.logreg",predict.type = "response")
cv.logistic = crossval(learner = logistic.learner,task = train5,iters = 5,stratify = TRUE,measures = acc,show.info = T)
logit.mod = mlr::train(logistic.learner, train5)
getLearnerModel(logit.mod)
logit.pred = predict(logit.mod, test5)
logit5 = confusionMatrix(logit.pred$data$response, test5$env$data$first_goal_win)

#Decision Tree
library(rpart)
getParamSet("classif.rpart")
tree = makeLearner("classif.rpart", predict.type = "response")
set_cv = makeResampleDesc("CV",iters = 5L)
tree.param = makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2))
gscontrol = makeTuneControlGrid()
tuned = tuneParams(learner = tree, resampling = set_cv, task = train5, par.set = tree.param, control = gscontrol, measures = acc)
tree.param = setHyperPars(tree, par.vals = tuned$x)
tree.full = mlr::train(tree.param, train5)
getLearnerModel(tree.full)
dtfull.pred = predict(tree.full, test5)

decisionT5=confusionMatrix(dtfull.pred$data$response, test5$env$data$first_goal_win)
#Random Forest
getParamSet("classif.randomForest")

forestLearner = makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 100, mtry = 3))
forestLearner$par.vals = list(importance = T)

rf_param = makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)
randcontrol= makeTuneControlRandom(maxit = 50L)
set.cv = makeResampleDesc("CV", iters = 3L)
rf.tuned = tuneParams(learner = forestLearner, resampling = set.cv, task = train5, par.set = rf_param, control = randcontrol, measures = acc)

rf.tree = setHyperPars(forestLearner, par.vals = rf.tuned$x)
rf.trained = mlr::train(rf.tree, train5)

getLearnerModel(rf.trained)

rf.pred = predict(rf.trained, test5)
randfor5=confusionMatrix(rf.pred$data$response, test5$env$data$first_goal_win)

#load svm
svm = makeLearner("classif.ksvm", predict.type = "response")

#Set parameters
param.svm = makeParamSet(
  makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), #cost parameters
  makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4)) #RBF Kernel Parameter
)

#specify search function
control = makeTuneControlGrid()

#tune model
svm.tuned = tuneParams(svm, task = train5, resampling = set.cv, par.set = param.svm, control = control,measures = acc)

#set the model with best params
set.svm = setHyperPars(svm, par.vals = svm.tuned$x)

#train
trained.svm = mlr::train(svm, train5)

#test
svm.pred = predict(trained.svm, test5)
svm5 = confusionMatrix(svm.pred$data$response, test5$env$data$first_goal_win)


#gbm
g.gbm = makeLearner("classif.gbm", predict.type = "response")

#specify tuning method
rancontrol = makeTuneControlRandom(maxit = 50L)

#3 fold cross validation
set_cv = makeResampleDesc("CV",iters = 3L)

#parameters
gbm_par= makeParamSet(
  makeDiscreteParam("distribution", values = "bernoulli"),
  makeIntegerParam("n.trees", lower = 100, upper = 1000), #number of trees
  makeIntegerParam("interaction.depth", lower = 2, upper = 10), #depth of tree
  makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
  makeNumericParam("shrinkage",lower = 0.01, upper = 1)
)

#tune parameters
tune_gbm = tuneParams(learner = g.gbm, task = train5,resampling = set_cv,measures = acc,par.set = gbm_par,control = rancontrol)

#set parameters
final_gbm = setHyperPars(learner = g.gbm, par.vals = tune_gbm$x)

#train
to.gbm = mlr::train(final_gbm, train5)

#test
pr.gbm = predict(to.gbm, test5)
gbm5=confusionMatrix(pr.gbm$data$response, test5$env$data$first_goal_win)

#XGBoost

#load xgboost
set.seed(1001)
getParamSet("classif.xgboost")

#make learner with inital parameters
xg_set = makeLearner("classif.xgboost", predict.type = "response")
xg_set$par.vals = list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 250
)

#define parameters for tuning
xg_ps = makeParamSet(
  makeIntegerParam("nrounds",lower=200,upper=600),
  makeIntegerParam("max_depth",lower=3,upper=20),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("eta", lower = 0.001, upper = 0.5),
  makeNumericParam("subsample", lower = 0.10, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)

#define search function
rancontrol = makeTuneControlRandom(maxit = 100L) #do 100 iterations

#3 fold cross validation
set_cv = makeResampleDesc("CV",iters = 3L)

#tune parameters
xg_tune = tuneParams(learner = xg_set, task = train5, resampling = set_cv,measures = acc,par.set = xg_ps, control = rancontrol)

#set parameters
xg_new = setHyperPars(learner = xg_set, par.vals = xg_tune$x)

#train model
xgmodel = mlr::train(xg_new, train5)

#test model
predict.xg = predict(xgmodel, test5)
xgb5=confusionMatrix(predict.xg$data$response, test5$env$data$first_goal_win)


xgb20
svm20
randfor20
QDA20
logit20
decisionT20
gbm20

xgb10
svm10
randfor10
QDA10
logit10
decisionT10
gbm10

xgb5
svm5
randfor5
QDA5
logit5
decisionT5
gbm5

