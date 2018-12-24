

#****************************************************************************************************
#                Prep a synthetic file and its puf counterpart for reweighting ####
#****************************************************************************************************

prep_for_reweighting <- function(sfname) {
  # sfname is the base name of a csv file -- e.g., synthpop3
  
  syndf <- read_csv(paste0(globals$synd, sfname, ".csv"), 
                    col_types = cols(.default= col_double()), 
                    n_max=-1) %>%
    dplyr::select(intersect(names(.), str_to_upper(puf.vnames$vname))) %>% # don't get any non-PUF variables
    mutate(ftype=sfname)
  
  
  # we want a puf with exactly the same variables so that we can run it through Tax-Calculator
  puf.base <- get_puf.base(names(syndf))
  # identical(names(syndf), names(puf.base))
  
  # prepare a base file for tax calculator
  tc.base <- bind_rows(puf.base, syndf) %>%
    setNames(change_case(names(.))) %>% # Tax-Calculator expects mostly lower-case names
    do(impose_variable_rules(.)) %>% # not needed for synpuf5 and later
    do(prime_spouse_splits(.)) %>%
    mutate(RECID=row_number(), wt=s006 / 100)
  
  # write tcbase to a file, because the Tax-Calculator CLI reads a csv file
  # maybe use temp file?
  tc.fn <- "tcbase.csv"
  write_csv(tc.base, paste0(globals$tc.dir, tc.fn))
  
  cmd <- tc.wincmd(tc.fn, globals$tc.dir, globals$tc.cli)
  # cmd    # a good idea to look at the command
  
  a <- proc.time()
  system(cmd) # CAUTION: this will overwrite any existing output file that had same input filename!
  proc.time() - a # it can easily take 5-10 minutes depending on the size of the input file
  
  tc.outfn <- paste0(str_remove(basename(tc.fn), ".csv"), "-", 13, "-#-#-#", ".csv")
  tc.output <- read_csv(paste0(globals$tc.dir, tc.outfn), 
                        col_types = cols(.default= col_double()), 
                        n_max=-1)
  
  rwprep <- list(tc.base=tc.base, tc.output=tc.output)
  return(rwprep)
}


#****************************************************************************************************
#                target preparation functions ####
#****************************************************************************************************

add_rulenames <- function(target.rules){
  target.rules <- target.rules %>%
    mutate(constraint.name=paste0(wtvar, "_", str_remove_all(subgroup, " ")),
           constraint.shortname=paste0("con_", row_number()),
           target.num=row_number())
  return(target.rules)
}


#****************************************************************************************************
#                constraint coefficient functions ####
#****************************************************************************************************
get_constraint_coefficients_sparse <- function(synfile, target.rules){
  # get the nonzero constraint coefficients for all feasible targets, in sparse form, as a data frame
  # also create an enhanced targets data frame that includes a feasibility true-false indicator, a counter
  # for the feasible constraint number, and sum of the RHS for the constraint
  
  # target.num is the target number - there may be fewer feasible constraints than desired targets
  # i is the constraint number (there may be fewer feasible constraints than targets)
  # j is the variable number
  
  get_nzcc <- function(target.num, synfile, target.rules, rules, enhanced.targets, wtvar="wt"){
    # get constraint coefficients for a single target (a row in target.rules), as a data frame
    
    # determine whether the coefficient will be:
    #   the weight for each record, if we are targeting number of returns, or
    #   the weighted multiplied by a variable's value, for each record, if we are targeting weighted value of a variable
    # the vector multiplier has the weight, or the weighted value of the variable
    if(target.rules$wtvar[target.num]==wtvar) multiplier <- rep(1, nrow(synfile)) else # weighted number of records 
      multiplier <- synfile[[target.rules$wtvar[target.num]]] # weighted value
    
    # get all constraint coefficients for this target
    cc <- with(synfile, eval(rules[target.num]) * wt) * multiplier 
    
    inzcc <- which(cc!=0)  # get indexes of the nonzero constraint coefficients
    
    nzcc.row <- NULL
    if(length(inzcc) > 0){
      # only increase i, the counter of feasible constraints, if this constraint at least one nonzero constraint coefficient
      i <<- i + 1 # MUST use the << syntax to change the value of the global variable i
      nzcc.row <- tibble(i=i, j=inzcc, nzcc=cc[inzcc], constraint.shortname=target.rules$constraint.shortname[target.num])
      enhanced.targets$cnum[target.num] <<- i
      enhanced.targets$synfile.rhs[target.num] <<- sum(nzcc.row$nzcc)
    } else enhanced.targets$feasible[target.num] <<- FALSE  # note the << syntax to change global variable
    
    return(nzcc.row)
  }
  
  # Prepare to run get_nzcc on all targets
  i <- 0 # counter for the number of feasible targets
  enhanced.targets <- target.rules
  enhanced.targets$feasible <- TRUE # we will change this to false for any non-feasible targets
  enhanced.targets$cnum <- NA
  enhanced.targets$synfile.rhs <- 0
  rules <- parse(text=target.rules$subgroup) 
  nzcc <- ldply(1:nrow(target.rules), get_nzcc, synfile, target.rules, rules, enhanced.targets)
  
  return(list(nzcc=nzcc, enhanced.targets=enhanced.targets))
}


get_constraint_sums <- function(df, target.rules){
  get_consum <- function(target.num, df, target.rules, rules, wtvar="wt"){
    # get constraint coefficients for a single target (a row in target.rules), as a data frame
    
    # determine whether the coefficient will be:
    #   the weight for each record, if we are targeting number of returns, or
    #   the weighted multiplied by a variable's value, for each record, if we are targeting weighted value of a variable
    # the vector multiplier has the weight, or the weighted value of the variable
    if(target.rules$wtvar[target.num]==wtvar) multiplier <- rep(1, nrow(df)) else # weighted number of records 
      multiplier <- df[[target.rules$wtvar[target.num]]] # weighted value
    
    # get all constraint coefficients for this target
    cc <- with(df, eval(rules[target.num]) * wt) * multiplier 
    return(sum(cc))
  }
  
  rules <- parse(text=target.rules$subgroup) # R logical expression for rules used in eval below
  vsums <- laply(1:nrow(target.rules), get_consum, df, target.rules, rules)
  return(vsums)
}


make.sparse.structure.from.nzcc <- function(constraint.coefficients.sparse){
  dlply(constraint.coefficients.sparse, .(i), function(x) x$j)
}


get_constraint_coefficients_dense <- function(synfile, target.rules, wtvar="wt") {
  # get constraint coefficients -- how the value of a target changes when the weight changes
  
  rules <- parse(text=target.rules$subgroup) # R logical expression for rules used in eval below
  
  cc.base <- synfile
  for(i in 1:nrow(target.rules)){
    vname <- target.rules$constraint.shortname[i]
    if(target.rules$wtvar[i]==wtvar) mult <- rep(1, nrow(cc.base)) else
      mult <- cc.base[[target.rules$wtvar[i]]]
    cc.base[[vname]] <- with(cc.base, eval(rules[i]) * wt) * mult
  }
  cc.dense <- cc.base %>% dplyr::select(one_of(target.rules$constraint.shortname))
  return(cc.dense)
}


find_non_feasible_constraints <- function(synfile, target.rules){
  # find out if any of the constraints are not in the sparse matrix, meaning that the synthetic
  # file has one or more records that meet the logical criteria for that constraint (e.g., c00100==0)
  # but all values for the variable to be weighted are zero and thus it is impossible to reweight
  # and get a different value
  
  # There's got to be a better way to find these!
  
  constraint.coefficients.dense <- get_constraint_coefficients_dense(synfile, target.rules)
  constraint.shortname.vec <- names(constraint.coefficients.dense)
  
  # put the constraint coefficients into a sparse format that only 
  # includes non-zero coefficients, in a dataframe that has:
  #   i -- the constraint number
  #   constraint.shortname
  #   j -- the variable number (an index into the vector x)
  #   value -- the constraint coefficient
  constraint.coefficients.sparse.df <- constraint.coefficients.dense %>% 
    mutate(j=row_number()) %>%
    gather(constraint.shortname, value, -j) %>%
    mutate(constraint.shortname=
             factor(constraint.shortname, levels=constraint.shortname.vec), # factor so we can sort in order of appearance in cc.full
           i=match(constraint.shortname, constraint.shortname.vec)) %>%
    filter(value!=0) %>%
    dplyr::select(i, constraint.shortname, j, value) %>%
    arrange(i, j) # SORT ORDER IS CRITICAL
  
  # return a list of 3 items
  feasibility.list <- list()
  feasibility.list$nonfeasible.indexes <- which(!target.rules$constraint.shortname %in% 
                                                  unique(constraint.coefficients.sparse.df$constraint.shortname))
  feasibility.list$feasible.indexes <- setdiff(1:nrow(target.rules), feasibility.list$nonfeasible.indexes)
  
  feasibility.list$target.rules.feasible <- add_rulenames(target.rules[feasibility.list$feasible.indexes, ])
  
  return(feasibility.list)
}


get_constraint_coefficients_sparse_from_dense <- function(constraint.coefficients.dense){
  constraint.shortname.vec <- names(constraint.coefficients.dense)
  constraint.shortname.vec
  
  constraint.coefficients.sparse <- constraint.coefficients.dense %>% 
    mutate(j=row_number()) %>%
    gather(constraint.shortname, value, -j) %>%
    mutate(constraint.shortname=
             factor(constraint.shortname, levels=constraint.shortname.vec), # factor so we can sort in order of appearance in cc.full
           i=match(constraint.shortname, constraint.shortname.vec)) %>%
    filter(value!=0) %>%
    dplyr::select(i, constraint.shortname, j, value) %>%
    arrange(i, j) # SORT ORDER IS CRITICAL
}

