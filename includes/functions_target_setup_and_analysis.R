
getcc.full <- function(df, target.rules, wtvar="s006") {
  # get constraint coefficients
  cc.base <- df
  for(i in 1:nrow(target.rules)){
    print(target.rules$cname[i])
    vname <- target.rules$cnum[i]
    if(target.rules$wtvar[i]==wtvar) mult <- rep(1, nrow(cc.base)) else
      mult <- cc.base[[target.rules$wtvar[i]]]
    cc.base[[vname]] <- with(cc.base, eval(rules[i]) * wt) * mult
  }
  cc.full <- cc.base %>% dplyr::select(one_of(target.rules$cnum))
  return(cc.full)
}

