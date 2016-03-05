sample_drift = function(tr, te, n, replace=T, ...){

  # this returns a sample of the training data.frame
  # according to its weights
  s = function() tr[w > runif(length(w)), ]

  w = weights(tr, te, ...)

  # initialize sampled training set
  tr_sampled = s()

  # repeat sampling if necessary and asked for
  if(replace){
    while(nrow(tr_sampled) <= n){
      tr_sampled = rbind(tr_sampled, s())
    }
  }

  # if fewer rows than asked for
  # return the total number of rows
  n_limit = if(nrow(tr_sampled) < n){
    warning("Returning fewer rows than requested. Try replace=T")
    nrow(tr_sampled)
  } else n

  return(tr_sampled[1:n_limit, ])
}
