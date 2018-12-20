

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
#                more functions ####
#****************************************************************************************************
get_constraint_coefficients_dense <- function(df, target.rules, wtvar="wt") {
  # get constraint coefficients -- how the value of a target changes when the weight changes
  # create an R expression for these rules so that we can use them in code
  rules <- parse(text=target.rules$subgroup)
  
  cc.base <- df
  for(i in 1:nrow(target.rules)){
    vname <- target.rules$constraint.shortname[i]
    if(target.rules$wtvar[i]==wtvar) mult <- rep(1, nrow(cc.base)) else
      mult <- cc.base[[target.rules$wtvar[i]]]
    cc.base[[vname]] <- with(cc.base, eval(rules[i]) * wt) * mult
  }
  cc.full <- cc.base %>% dplyr::select(one_of(target.rules$constraint.shortname))
  return(cc.full)
}

