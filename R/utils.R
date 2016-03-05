check_data = function(tr, te){

  # find common variables
  var = intersect(colnames(tr), colnames(te))
  if(length(var) == 0) stop("No common variables found between testing and training")

  if(length(var) < ncol(tr)){
    missing = setdiff(var, colnames(tr))
    warning(paste(length(missing),
                  "variables were in the training set, but not the testing set"))
  }
  if(length(var) < ncol(tr)){
    missing = setdiff(var, colnames(tr))
    warning(paste(length(missing),
                  "variables were in the testing set, but not the training set"))
  }

  tr = lapply(tr, function(x) if(is.factor(x)) as.character((x)) else x)
  te = lapply(te, function(x) if(is.factor(x)) as.character((x)) else x)

  tr = as.data.frame(tr)
  te = as.data.frame(te)

  # look for type matching
  tr_classes = sapply(tr[var], class)
  te_classes = sapply(te[var], class)
  if(any(tr_classes != te_classes)){
    warning(paste("Data types do not match for",
                  var[tr_classes != te_classes],
                  "\n"))
  }

  # look for rare-categories

#   has_rare = lapply(df, function(x){
#     if(is.character(x)){
#       f = table(x) / length(x)
#       if(min(f) < 0.01) return(TRUE)
#     }
#     FALSE
#   })
#   if(any(has_rare)){
#     warning(paste(var[has_rare],
#                   "has rare (freq < 0.01) categories. Consider merging levels"))
#   }


  return(var)
}

combine_dfs = function(tr, te){
  var = check_data(tr, te)

  # create a balanced dataset between
  # training / testing
  n = min(nrow(tr), nrow(te))
  tr = tr[1:nrow(tr) %in% sample(1:nrow(tr), n), ]
  te = te[1:nrow(te) %in% sample(1:nrow(te), n), ]
  balanced = rbind(tr[var], te[var])

  # labels for balanced dataset
  label = factor(rep(c(0,1), each=n))
  list(balanced=balanced,
       label=label,
       tr=tr,
       te=te)
}
