
rm(list = ls())

source("./src/04_dataMart.R", encoding = "UTF-8")
source("./src/99_function.R", encoding = "UTF-8")



## 검증용
#######################################################################
################## split -> train, test     ########################### 
################## lm   + 다중공선성 확인   ########################### 
#######################################################################
set.seed(3885)

proportion = 0.7
idx = sample(1:nrow(train2), size = round(proportion * nrow(train2)))
train2_train = train2[idx, ]
train2_valid = train2[-idx, ]

train3_train = train2_train
test3_valid = train2_valid
submission_valid = test3_valid %>% select(id, target)
submission_valid$target = NA

test3_valid$target = NULL
fwrite(train3_train, "./data/prep/train3_train.csv")
fwrite(test3_valid, "./data/prep/test3_valid.csv")
fwrite(submission_valid, "./data/prep/submission_valid.csv")


y_train = train2_train$target
y_test = train2_valid$target

mod.lm1 = glm(target ~., data = train2_train[,-1], family = binomial)
summary(mod.lm1)

# 변수 선택법
mod.lm2 = step(mod.lm1, direction = "both")
summary(mod.lm2)



pred.lm = predict(mod.lm2, train2_valid[,-c(1,ncol(train2_valid))])

out.lm = data.frame(id = train2_valid$id,
                    real = train2_valid$target,
                    pred = pred.lm)

confusionMatrix(as.factor(as.integer(out.lm$real)), as.factor(as.integer(ifelse(normalize(out.lm$pred) >= 0.45 , 1, 0))))
confusionMatrix(as.factor(as.integer(out.lm$real)), as.factor(as.integer(ifelse(out.lm$pred >= 0.55 , 1, 0))))

# train2_train = train2_train[,c("id", names(mod.lm2$coefficients)[-1], "target")]
# train2_valid = train2_valid[,c("id", names(mod.lm2$coefficients)[-1], "target")]

#######################################################################
############################ RF ####################################### 
#######################################################################


#set.seed(123)


mod.rdf = randomForest(target ~ ., data = train2_train[,-1],
                       ntree=1000,importance=T)

pred.rdf = predict(mod.rdf, train2_valid)


out.rdf = data.frame(id = train2_valid$id,
                     real = train2_valid$target,
                     pred = pred.rdf)


#confusionMatrix(as.factor(as.integer(out.rdf$real)), as.factor(as.integer(ifelse(normalize(out.rdf$pred) >= 0.45 , 1, 0))))
confusionMatrix(as.factor(as.integer(out.rdf$real)), as.factor(as.integer(ifelse(out.rdf$pred >= 0.45 , 1, 0))))

#######################################################################

#######################################################################
############################ GBM ###################################### 
#######################################################################

# GBM
trainSparse <- sparse.model.matrix(target~. , data = train2_train[,-1])
testSparse <- sparse.model.matrix(target~. , data = train2_valid[,-1])


mod.gbm <- gbm.fit(x = as.matrix(trainSparse), y = y_train, n.trees = 50,
                   shrinkage = 0.1 ,interaction.depth = 3, n.minobsinnode = 10,
                   distribution = "gaussian",bag.fraction = 0.5)

# Predict
pred.gbm <- predict(mod.gbm, newdata = as.matrix(testSparse), n.trees = 50)

out.gbm = data.frame(id = train2_valid$id,
                     real = train2_valid$target,
                     pred = pred.gbm)

confusionMatrix(as.factor(as.integer(out.gbm$real)), as.factor(ifelse(normalize(out.gbm$pred) >= 0.45 , 1, 0)))
confusionMatrix(as.factor(as.integer(out.gbm$real)), as.factor(as.integer(ifelse(out.gbm$pred >= 0.55 , 1, 0))))

#######################################################################

#######################################################################
############################ XGB ###################################### 
#######################################################################



trainSparse = xgb.DMatrix(data.matrix(train2_train[,-c(1,length(train2_train))]), label=y_train, missing=NA)
testSparse  = xgb.DMatrix(data.matrix(train2_valid[,-c(1,length(train2_valid))]), missing = NA)

foldsCV <- createFolds(y_train, k=20, list=TRUE, returnTrain=FALSE)

# param.xgb <- list(subsample = 1
#                   , max_depth = 5
#                   , colsample_bytree = 0.5
#                   , eta = 0.08
#                   , eval_metric = 'rmse'#average
#                   , min_child_weight = 1.2)



mod.xgb = xgboost(data = trainSparse,
                  eta = 0.08,
                  nfold = 10, 
                  max_depth = 8, 
                  nround = 300, 
                  subsample = 0.8,
                  colsample_bytree = 0.8,
                  eval_metric = 'rmse',
                  verbose = 1)



# 변수 중요도
xgb_imp = xgb.importance(model = mod.xgb)
xgb.ggplot.importance(importance_matrix = xgb_imp)


pred.xgb = predict(mod.xgb,testSparse)

out.xgb = data.frame(id = train2_valid$id,
                     real = train2_valid$target,
                     pred = pred.xgb)

out.xgb %>% group_by(real) %>% 
  summarise(mean = mean(pred),
            min = min(pred),
            max = max(pred))

out.xgb %>% mutate(pred_norm = normalize(out.xgb$pred),
                   pred_01 = ifelse(pred_norm > 0.5, 1, 0)) %>% 
  filter(real != pred_01)

confusionMatrix(as.factor(as.integer(out.xgb$real)), as.factor(as.integer(ifelse(normalize(out.xgb$pred) >= 0.5 , 1, 0))))
confusionMatrix(as.factor(as.integer(out.xgb$real)), as.factor(as.integer(ifelse(out.xgb$pred >= 0.51 , 1, 0))))


#######################################################################


#######################################################################
############################ LGB ###################################### 
#######################################################################


train_sparse = Matrix(as.matrix(train2_train[,-c(1,length(train2_train))]), sparse=TRUE)
test_sparse  = Matrix(as.matrix(train2_valid[,-c(1,length(train2_valid))]), sparse=TRUE)

lgb.train = lgb.Dataset(data=train_sparse, label=y_train)


lgb.param = list(objective = "regression", 
                 metric = "error", 
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



best.iter = 1300

# Train final model
mod.lgb = lgb.train(params = lgb.param, data = lgb.train,
                    num_threads = 10 , nrounds = best.iter,
                    eval_freq = 10, eval = lgb.normalizedgini)


pred.lgb = predict(mod.lgb, test_sparse)

out.lgb = data.frame(id = train2_valid$id,
                     real = train2_valid$target,
                     pred = pred.lgb)

confusionMatrix(as.factor(as.integer(out.lgb$real)), as.factor(as.integer(ifelse(normalize(out.lgb$pred) >= 0.55 , 1, 0))))
confusionMatrix(as.factor(as.integer(out.lgb$real)), as.factor(as.integer(ifelse(out.lgb$pred >= 0.55 , 1, 0))))




#######################################################################
############################ SVR ###################################### 
#######################################################################


paramsvr <- list(gamma = 1e-4, cost = 100, epsilon = 0.001)

trainSparse <- sparse.model.matrix(target~. , data = train2_train[,-1])
testSparse <- sparse.model.matrix(~. , data = train2_valid[,-c(1,ncol(train2_valid))])




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


# 
# pred.ngb = read.csv("./out/ngb/ngb1_valid.csv")
# pred.ngb = pred.ngb$target
# out.ngb = data.frame(id = train2_valid$id,
#                      real = train2_valid$target,
#                      pred = pred.ngb)
# out.ngb = as.data.frame(out.ngb)

#######################################################################


#############################################
######### 예측 모형 평가지표 종합 ###########
#############################################


## 검증용

colnames(out.lm)[3] = "lm_pred"
colnames(out.rdf)[3] = "rdf_pred"
colnames(out.gbm)[3] = "gbm_pred"
colnames(out.xgb)[3] = "xgb_pred"
colnames(out.lgb)[3] = "lgb_pred"
# colnames(out.svr)[3] = "svr_pred"
# colnames(out.cb)[3] = "cb_pred"
# colnames(out.ngb)[3] = "ngb_pred"

out = do.call("cbind", list(out.lm,
                            out.rdf,
                            out.gbm,
                            out.xgb,
                            out.lgb))

out = out[c(1,2,grep("pred",colnames(out)))]
#out[,2:ncol(out)] = exp(out[,2:ncol(out)])-1
out$lm_pred = normalize(out$lm_pred)
out$rdf_pred = normalize(out$rdf_pred)
out$gbm_pred = normalize(out$gbm_pred)
out$xgb_pred = normalize(out$xgb_pred)
out$lgb_pred = normalize(out$lgb_pred)


out = out %>% mutate(pred = (lm_pred+rdf_pred+gbm_pred+xgb_pred+lgb_pred)/5)

pcla = data1[data1$division == "train",c("id", "total.satisfaction")]
pcla$id = as.factor(pcla$id)
pcla$total.satisfaction = ifelse(pcla$total.satisfaction > mean(pcla$total.satisfaction) ,1, 0)
out = left_join(out, pcla)

out %>% filter(pred > 0.45) %>% filter()

out$pred = ifelse(out$pred >= 0.5 , 1, 0)

confusionMatrix(as.factor(out$real), as.factor(out$pred))
# NMAE(out$real, out$svr_pred)
# NMAE(out$real, out$cb_pred)
# NMAE(out$real, out$ngb_pred)



# 
# pred_rate = seq(0.05,1,0.05)
# 
# rate_df = data.frame()
# for(lm_rate in pred_rate) {
#   for(rdf_rate in pred_rate) {
#     for(gbm_rate in pred_rate) {
#       for(xgb_rate in pred_rate) {
#         for(lgb_rate in pred_rate) {
#           eval_accu = confusionMatrix(as.factor(as.integer(out$real)), 
#                                       as.factor(as.integer(ifelse(normalize(out$lm_pred*lm_rate + out$rdf_pred*rdf_rate + out$gbm_pred*gbm_rate + out$xgb_pred*xgb_rate + out$lgb_pred*lgb_rate)>=0.45, 1, 0))))
#           eval_accu = as.double(eval_accu$byClass["Balanced Accuracy"])
#           
#             if(lm_rate + rdf_rate + gbm_rate + xgb_rate + lgb_rate == 1) {
#               tmp = data.frame(lm_rate = lm_rate,
#                                rdf_rate = rdf_rate,
#                                gbm_rate = gbm_rate,
#                                xgb_rate = xgb_rate,
#                                lgb_rate = lgb_rate,
#                                accu = eval_accu)
#               rate_df = rbind(rate_df, tmp)
# 
#           }
#         }
#       }
#     }
#   }
#   print(lm_rate)
# }
# rate_df %>% arrange(accu) %>% tail
# 
# 
# 
# 
# 
# pred_rate = seq(0.05,1,0.05)
# 
# rate_df = data.frame()
# 
# for(rdf_rate in pred_rate) {
#   for(gbm_rate in pred_rate) {
#     for(xgb_rate in pred_rate) {
#       for(lgb_rate in pred_rate) {
#         eval_accu = confusionMatrix(as.factor(as.integer(out$real)), 
#                                     as.factor(as.integer(ifelse(normalize(out$rdf_pred*rdf_rate + out$gbm_pred*gbm_rate + out$xgb_pred*xgb_rate + out$lgb_pred*lgb_rate)>=0.45, 1, 0))))
#         eval_accu = as.double(eval_accu$byClass["Balanced Accuracy"])
# 
#           if(rdf_rate + gbm_rate + xgb_rate + lgb_rate == 1) {
#             tmp = data.frame(rdf_rate = rdf_rate,
#                              gbm_rate = gbm_rate,
#                              xgb_rate = xgb_rate,
#                              lgb_rate = lgb_rate,
#                              accu = eval_accu)
#             rate_df = rbind(rate_df, tmp)
#         }
#       }
#     }
#   }
#   print(rdf_rate)
# }
# rate_df %>% arrange(accu) %>% tail
# rate_df %>% tail


