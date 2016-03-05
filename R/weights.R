weights = function(tr, te, method=c("randomForest", "glmnet"), lambda=1){

  tt = combine_dfs(tr, te)

  switch(method[1],
         glmnet=weights.glmnet(tt, lambda),
         randomForest=weights.randomForest(tt, lambda))

}

weights.glmnet = function(tt){
  require(glmnet)

  x_balanced = model.matrix(~.-1, data=tt$balanced)

  m = cv.glmnet(x_balanced,
                tt$label,
                keep=T,
                family="binomial",
                nfolds=10)


  x_tr = model.matrix(~.-1, data=tt$tr)
  p = predict(m, newx=x_tr, type="response", s="lambda.min")[,1]^lambda

  return(p)
}

weights.randomForest = function(tt){
  require(randomForest)

  x_balanced = model.matrix(~.-1, data=tt$balanced)

  m = randomForest(x_balanced, tt$label)

  x_tr = model.matrix(~.-1, data=tt$tr)
  p = predict(m, newdata=x_tr, type="prob")[,2]^lambda
  return(p)
}
