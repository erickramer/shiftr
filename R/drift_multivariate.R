detect_drift = function(tr, te, sample=NULL){

  # merge training and testing
  combined = combine_dfs(tr, te)
  df = combined$balanced
  label = combined$label

  # run univariate statistics
  stats = drift_stats(df, label)

  # create multivariate model to
  # distinguish testing vs. training
  x = model.matrix(~.-1, data=df)
  rf = randomForest(x, factor(label))

  # run statistical test
  test = wilcox.test(rf$votes[,1] ~ label, alternative="greater")

  drift = list(stats=stats,
               rf=rf,
               test=test)

  class(drift) = "drift"

  return(drift)
}
