

## 제출용

# colnames(out.lm)[2] = "lm_pred"
# colnames(out.rdf)[2] = "rdf_pred"
# colnames(out.gbm)[2] = "gbm_pred"
colnames(out.xgb)[2] = "xgb_pred"
# colnames(out.lgb)[2] = "lgb_pred"
# colnames(out.svr)[2] = "svr_pred"
# colnames(out.ngb)[2] = "ngb_pred"


# out = do.call("cbind", list(out.lm,
#                             out.rdf,
#                             out.gbm,
#                             out.xgb,
#                             out.ngb,
#                             out.lgb,
#                             out.svr))
out = out.xgb
out = out[c(1,grep("pred",colnames(out)))]
# out[,2:ncol(out)] = exp(out[,2:ncol(out)])-1


# out = out %>% 
#   mutate(target = out$rdf_pred *0.40+ out$ngb_pred*0.60)

out = out %>%
  mutate(target = out$xgb_pred *1.0)

submission = out %>% 
  select(id, target)

submission %>% head

write.csv(submission, paste0("./out/submission/sub1.csv"), row.names = FALSE)
