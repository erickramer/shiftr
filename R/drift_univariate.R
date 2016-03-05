drift_stats = function(df, y){

  univariate_results = lapply(df, drift_univariate, y=y)

  # create "tidy" data.frame with results
  final_stats = do.call(rbind, univariate_results)
  final_stats$variable = names(univariate_results)

  # rearranging so the variables are
  # in descending order of significance
  final_stats[order(final_stats$statistic, decreasing=T),
              c("variable", "statistic", "p_value")]
}

drift_univariate = function(x, y){
  if(is.numeric(x)) drift_numeric(x, y) else drift_categorical(x, y)
}

drift_numeric = function(x, y){
  require(randomForest)

  # preparing data
  x = matrix(x, ncol=1)
  y = factor(y)

  # training random forest
  rf = randomForest(x, y)

  # testing for significance of predictions
  test = wilcox.test(rf$votes[,2] ~ y, alternative="greater")

  # return 1-row data.frame
  return(data.frame(statistic=test$statistic,
                    p_value=test$p.value,
                    type="numeric",
                    row.names=NULL))
}

drift_categorical = function(x, y){
  require(randomForest)

  # preparing data
  x = create_matrix(x)
  y = factor(y)

  # train random forest
  rf = randomForest(x, y)

  # run test
  test = wilcox.test(rf$votes[,1] ~ y, alternative="greater")

  return(data.frame(statistic=test$statistic,
                    p_value=test$p.value,
                    type="categorical",
                    row.names=NULL))
}

create_matrix = function(x){
  x = as.character(x)

  # remove low frequency categories
  frequencies = table(x) / length(x)
  low_frequency_categories = colnames(frequencies[frequencies < 0.01])
  x[x %in% low_frequency_categories] = "low_freq_category"

  f = factor(x)
  if(length(levels(f)) == 1){
    warning("Variable has too few or too many categories")
  }

  return(model.matrix(~f-1))
}
