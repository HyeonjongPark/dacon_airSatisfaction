
#rm(list = ls())
source("./src/04_dataMart.R", encoding = "UTF-8")



set.seed(3885)

## 제출용


#######################################################################
################## split -> train, test     ########################### 
################## lm   + 다중공선성 확인   ########################### 
#######################################################################



y_train = train2$target
y_test = test2$target

mod.lm1 = lm(target ~., data = train2[,-1])
summary(mod.lm1)

# 변수 선택법
mod.lm2 = step(mod.lm1, direction = "both")
summary(mod.lm2)


pred.lm = predict(mod.lm2, test2[,-c(1,ncol(test2))])

out.lm = data.frame(id = test2$id,
                    pred = pred.lm)


train2 = train2[,c("id", names(mod.lm2$coefficients)[-1], "target")]
test2 = test2[,c("id", names(mod.lm2$coefficients)[-1], "target")]


#######################################################################
############################ RF ####################################### 
#######################################################################



mod.rdf = randomForest(target ~ ., data = train2[,-1], 
                       ntree=1000,importance=T)

pred.rdf = predict(mod.rdf, test2)


out.rdf = data.frame(id = test2$id,
                     pred = pred.rdf)
out.rdf %>% head

#######################################################################

#######################################################################
############################ GBM ###################################### 
#######################################################################

trainSparse <- sparse.model.matrix(target~. , data = train2[,-1])
testSparse <- sparse.model.matrix(~. , data = test2[,-c(1, ncol(test2))])


mod.gbm <- gbm.fit(x = as.matrix(trainSparse), y = y_train, n.trees = 50,
                   shrinkage = 0.1 ,interaction.depth = 3, n.minobsinnode = 10,
                   distribution = "gaussian",bag.fraction = 0.5)

# Predict
pred.gbm <- predict(mod.gbm, newdata = as.matrix(testSparse), n.trees = 50)

out.gbm = data.frame(id = test2$id,
                     pred = pred.gbm)


#######################################################################

#######################################################################
############################ XGB ###################################### 
#######################################################################


# 
# 
# trainSparse = xgb.DMatrix(data.matrix(train2_train[,-c(1,length(train2_train))]), label=y_train, missing=NA)
# testSparse  = xgb.DMatrix(data.matrix(train2_valid[,-c(1,length(train2_valid))]), missing = NA)
# 
# foldsCV <- createFolds(y_train, k=20, list=TRUE, returnTrain=FALSE)
# 
# param.xgb <- list(subsample = 1
#                   , max_depth = 5
#                   , colsample_bytree = 0.5
#                   , eta = 0.08
#                   , eval_metric = 'rmse'#average
#                   , min_child_weight = 1.2)
# 
# 
# 
# mod.xgb = xgboost(data = trainSparse,
#                   eta = 0.05,
#                   nfold = 10, 
#                   max_depth = 8, 
#                   nround = 500, 
#                   subsample = 0.8,
#                   colsample_bytree = 0.8,
#                   eval_metric = 'rmse',
#                   verbose = 1)
# 
# 
# 
# # 변수 중요도
# xgb_imp = xgb.importance(model = mod.xgb)
# xgb.ggplot.importance(importance_matrix = xgb_imp)
# 
# 
# pred.xgb = predict(mod.xgb,testSparse)
# 
# out.xgb = data.frame(id = train2_valid$id,
#                      real = train2_valid$target,
#                      pred = pred.xgb)
# 
# out.xgb %>% group_by(real) %>% 
#   summarise(mean = mean(pred),
#             min = min(pred),
#             max = max(pred))
# 
# confusionMatrix(as.factor(as.integer(out.xgb$real)), as.factor(as.integer(ifelse(normalize(out.xgb$pred) >= 0.45 , 1, 0))))
# confusionMatrix(as.factor(as.integer(out.xgb$real)), as.factor(as.integer(ifelse(out.xgb$pred >= 0.55 , 1, 0))))




trainSparse = xgb.DMatrix(data.matrix(train2[,-c(1,length(train2))]), label=y_train, missing=NA)
testSparse  = xgb.DMatrix(data.matrix(test2[,-c(1,length(test2))]), missing = NA)

foldsCV <- createFolds(y_train, k=20, list=TRUE, returnTrain=FALSE)


param.xgb <- list(subsample = 1
                  , max_depth = 5
                  , colsample_bytree = 0.5
                  , eta = 0.08
                  , eval_metric = 'rmse'#average
                  , min_child_weight = 1.2)



# xgb_cv <- xgb.cv(data=trainSparse,
#                  params=param.xgb,
#                  nrounds=100,
#                  prediction=TRUE,
#                  maximize=TRUE,
#                  folds=foldsCV,
#                  #early_stopping_rounds = 50,
#                  print_every_n = 5
# )

mod.xgb = xgboost(data = trainSparse,
                  eta = 0.05,
                  nfold = 10, 
                  max_depth = 8, 
                  nround = 500, 
                  subsample = 0.8,
                  colsample_bytree = 0.8,
                  eval_metric = 'rmse',
                  verbose = 1)


# 변수 중요도
xgb_imp = xgb.importance(model = mod.xgb)
xgb.ggplot.importance(importance_matrix = xgb_imp)


pred.xgb = predict(mod.xgb,testSparse)

out.xgb = data.frame(id = test2$id,
                     pred = pred.xgb)

out.xgb = data.frame(id = train2_valid$id,
                     real = train2_valid$target,
                     pred = pred.xgb)





#######################################################################


#######################################################################
############################ LGB ###################################### 
#######################################################################


train_sparse = Matrix(as.matrix(train2[,-c(1,length(train2))]), sparse=TRUE)
test_sparse  = Matrix(as.matrix(test2[,-c(1,length(test2))]), sparse=TRUE)

lgb.train = lgb.Dataset(data=train_sparse, label=y_train)


lgb.param = list(objective = "regression", 
                 metric = "rmse", 
                 learning_rate= 0.08,
                 num_leaves= 5,
                 max_depth= 10,
                 min_child_samples= 100,
                 max_bin= 100,
                 subsample= 0.5, 
                 subsample_freq= 1,
                 colsample_bytree= 0.5,
                 min_child_weight= 0,
                 min_split_gain= 0,
                 scale_pos_weight= 99.7)


lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}



best.iter = 700

# Train final model
mod.lgb = lgb.train(params = lgb.param, data = lgb.train,
                    num_threads = 10 , nrounds = best.iter,
                    eval_freq = 10, eval = lgb.normalizedgini)


pred.lgb = predict(mod.lgb, test_sparse)

out.lgb = data.frame(id = test2$id,
                     pred = pred.lgb)






#######################################################################


#######################################################################
############################ SVR ###################################### 
#######################################################################


paramsvr <- list(gamma = 1e-4, cost = 100, epsilon = 0.001)


trainSparse <- sparse.model.matrix(target~. , data = train2[,-1])
testSparse <- sparse.model.matrix(~. , data = test2[,-c(1,ncol(test2))])




mod.svr <- svm(x = as.matrix(trainSparse) , y = y_train, type = "eps-regression",
               kernel = "radial",cost = paramsvr[2], gamma = paramsvr[1], 
               epsilon = paramsvr[3])


pred.svr <- predict(mod.svr, newdata = as.matrix(testSparse)) %>% as.vector()


out.svr = data.frame(id = test2$id,
                     pred = pred.svr)





#######################################################################




#######################################################################
############################ NGB ###################################### 
#######################################################################

pred.ngb = read.csv("./out/ngb/ngb1.csv")
pred.ngb = pred.ngb$target

out.ngb = data.frame(id = test2$id,
                     pred = pred.ngb)


#######################################################################
