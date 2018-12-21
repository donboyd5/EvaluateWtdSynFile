

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
           constraint.shortname=paste0("con_", row_number()))
  return(target.rules)
}


#****************************************************************************************************
#                constraint coefficient functions ####
#****************************************************************************************************

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
  feasability.list <- list()
  feasability.list$nonfeasible.indexes <- which(!target.rules$constraint.shortname %in% 
                                                  unique(constraint.coefficients.sparse.df$constraint.shortname))
  feasability.list$feasible.indexes <- setdiff(1:nrow(target.rules), nonfeasible.indexes)
  
  feasability.list$target.rules.feasible <- add_rulenames(target.rules[feasible.indexes, ])
  
  return(feasability.list)
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

