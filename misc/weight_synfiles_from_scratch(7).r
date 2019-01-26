


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times.
library("lubridate") # lubridate, for date/times.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.
library("vctrs")
library("precis")

library("tibbletime") # https://business-science.github.io/tibbletime/

library("grDevices")
library("knitr")

library("zoo") # for rollapply

# devtools::install_github("donboyd5/btools")
library("btools") # library that I created (install from github)

library("ipoptr")


# library("synthpop") # note: masks select in dplyr


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************


#****************************************************************************************************
#                Includes ####
#****************************************************************************************************
source("./includes/globals_system_specific_boyd.r") # use a different version of this file if changing systems
source("./includes/globals_other.r")

source("./includes/functions.r")
source("./includes/functions_target_setup_and_analysis.r")
source("./includes/functions_ipopt.r")


# functions specific to the weighting from scratch approach:
source("./includes/functions_ipopt.r")
source("./includes/functions_weight_from_scratch.r")


#****************************************************************************************************
#                Get puf variable names ####
#****************************************************************************************************
puf.vnames <- get_puf_vnames()
addnames <- read_csv("vnum, vname, vdesc, category
                     0, c00100, Adjusted gross income (calculated), income
                     9000, e00200p, Wages-prime (calculated), income
                     9001, e00200s, Wages-spouse (calculated)
                     9010, taxbc, Tax before credit (calculated)")
puf.vnames <- bind_rows(addnames, puf.vnames) %>%
  arrange(vnum, vname)
ht(puf.vnames)



#******************************************************************************************************************
#  Get previously-prepared synfile-PUF and tax output, merge, and separate PUF and synfile ####
#******************************************************************************************************************
sfname <- "synthpop3"
# synprep <- readRDS(paste0(globals$tc.dir, sfname, "_rwprep.rds"))
synprep <- readRDS(paste0(globals$synd, sfname, "_rwprep.rds")) # this file is now in synpuf in Google Drive
names(synprep)

# now get the reforms and merge in taxbc

# merge and then split
tcvars <- c("c00100", "taxbc") # taxcalc vars
mrgdf <- left_join(synprep$tc.base, synprep$tc.output %>% dplyr::select(RECID, tcvars))
glimpse(mrgdf)
count(mrgdf, ftype)


#******************************************************************************************************************
#  pick a subset ####
#******************************************************************************************************************
mrgdf2 <- mrgdf %>%
  filter(MARS==2, c00100>=0, c00100<=25e3)

puf.full <- mrgdf2 %>% filter(ftype=="puf.full") # this has original puf weight
synfile <- mrgdf2 %>% filter(ftype==sfname) # this, too, has original puf weight


#******************************************************************************************************************
#  set up the "constraint" components of the objective function ####
#******************************************************************************************************************
# I refer to "constraint" components of the objective function as the components for which we want to minimize the squared difference of sum vs target
# for each component, we need:
#   the variable involved
#   the "constraint" type, i.e., one of:  n.all, sum.all, n.pos, n.neg, sum.pos, sum.neg
#   the "constraint" priority -- a multiplier of the squared diff -- the larger the mult, the more important this becomes
#   the target
#   the coefficient for each record determining how it enters into the sum that will be compared to the target
#     for value sums:
#       for the weight variable it will be 1 x the weight
#       for other variables it will be the variable times the weight
#     for numbers of returns it is simply the weight variable

# maybe make a list of objective function elements, or a data frame -- with an associated list of the coefficients
# the df would have:
#   elname, elvar, eltype, priority; link to a vector of coefficients based on elname, link to target based on elname

# we may want to see how far off we are on each constraint to help us determine priorities
# or even automate the priority setting process

# wt, c00100.val, taxbc.val, e00200.val
recipe <- read_csv("var, fn
                   wt, n.sum
                   c00100, val.sum
                   taxbc, val.sum
                   e00200, val.sum,
                   p23250, val.neg")


names(puf.full %>% select_if(is.numeric)) %>% sort
dropvars <- c("e00100", "DSI", "EIC", "f2441", "f6251", "fded", "MARS", "MIDR", "n24", "RECID", "s006", "wt",
              "e07400", "e11200", "e03300", "e00800", "e07600", "p08000", "e02100p", "e02100s", "e00900p", "e00900s")
puf.vnames %>% filter(vname %in% dropvars)

# vnum vname  vdesc                                             category   
# <dbl> <chr>  <chr>                                             <chr>      
#   1     7 e00800 Alimony received                                  income     
# 2    28 e03300 Payments to KEOGH accounts                        stat_adjust
# 3    31 e00100 Adjusted Gross Income (deficit)  (AGI)  (+/-)     agi        
# 4    50 e07400 General business credit                           credits    
# 5    51 e07600 Credit for prior year minimum tax                 credits    
# 6    52 p08000 Other Credits                                     credits    
# 7    72 e11200 Excess FICA/RRTA                                  payments   
# 8   169 s006   Decimal weight                                    misc_codes 
# 9  1005 fded   Form of Deduction Code                            manual     
# 10  1008 f2441  Form 2441, Child Care Credit Qualified Individual manual     
# 11  1010 f6251  Form 6251, Alternative Minimum Tax                manual     
# 12  1021 n24    Number of Children for Child Tax Credit           manual     

varlist <- names(puf.full %>% select_if(is.numeric)) %>% 
  sort %>%
  setdiff(., dropvars)

varlist
#varlist <- c("c00100", "taxbc", "e00200", "e00200p", "e01700", "e00300", "e00650", "p23250", "XTOT")
fnlist <- c("n.pos", "n.neg", "val.sum", "val.pos", "val.neg")

recipe <- expand.grid(var=varlist, fn=fnlist, stringsAsFactors = FALSE) %>% 
  as_tibble() %>%
  arrange(var, fn)
recipe <- bind_rows(tibble(var="wt", fn="n.sum"), recipe)
recipe
# for(i in 1:nrow(recipe)) recipe$target[i] <- do.call(recipe$fn[i], list(puf.full, recipe$var[i], puf.full$wt))

# start here to adjust a previously created recipe ----
tscale <- 1
recipe <- recipe %>%
  rowwise() %>%
  mutate(target=do.call(fn, list(puf.full, var, puf.full$wt))) %>%
  ungroup %>%
  mutate(scale=ifelse(target!=0, abs(target / tscale), 1/ tscale),
         obj.element=paste0(var, "_", fn)) %>%
  select(obj.element, var, fn, scale, target) %>%
  arrange(var, fn)
recipe

#..weed out unnecessary elements of the recipe ----
# if the target is 0 for negative values we can drop the neg versions AS LONG AS WE HAVE NO SYNTH NEG VERSIONS
# if the val.pos and val.sum versions are identical then we can drop the val.sum version
# can drop the "neg" and "pos" versions
recipe.flagged <- recipe %>%
  rowwise() %>%
  mutate(syn.unwtd=do.call(fn, list(synfile, var, rep(1, nrow(synfile))))) %>% # so we can check if negs!
  group_by(var) %>%
  mutate(flag.dropneg=ifelse(str_detect(fn, "neg") & target==0 & syn.unwtd==0, 1, 0),
         flag.dropdupsum=ifelse(target==target[match("val.sum", fn)] & (fn=="val.pos"), 1, 0),
         flag.dropdupn=ifelse(target==target[match("n.sum", fn)] & (fn=="n.pos"), 1, 0)) %>%
  mutate_at(vars(starts_with("flag")), funs(naz)) %>%
  ungroup %>%
  arrange(var, fn)
recipe.flagged

# remove recipe elements where the target is zero
recipe.use <- recipe.flagged %>%
  filter(!(flag.dropneg | flag.dropdupsum | flag.dropdupn)) %>%
  filter(target!=0) %>%
  select(obj.element, var, fn, scale, target)
recipe.use

# finally, add priority weights
recipe.use <- recipe.use %>%
  mutate(priority.weight=case_when(var %in% c("wt", "c00100", "e00200", "taxbc") ~ 100,
                                   fn %in% c("n.sum", "val.sum") ~ 10,
                                   TRUE ~ 1))  %>% 
  left_join(puf.vnames %>% select(var=vname, vdesc))
recipe.use

recipe.use %>% arrange(-priority.weight)

# What would our objective function be if each targeted variable was off by a given % (as a decimal)?
pct <- .01
sum(recipe.use$priority.weight * (pct^2))
# what if they were off by that same pct on average but with a random variation?
pctv <- rnorm(nrow(recipe.use), pct, sd=.05)
sum(recipe.use$priority.weight * (pctv^2))
(pctv *100) %>% round(., 1)


#******************************************************************************************************************
#  prepare the input list ####
#******************************************************************************************************************
inputs <- list()
inputs$recipe <- recipe.use
inputs$synsub <- synfile[, unique(inputs$recipe$var)] %>% mutate(wt=1)
synlong <- inputs$synsub %>%
  mutate(wtnum=row_number()) %>%
  gather(var, value, -wtnum)

# create a data frame with one row for each weight and obj.element combination
coeffs <- expand.grid(wtnum=1:nrow(inputs$synsub), obj.element=inputs$recipe$obj.element, stringsAsFactors = FALSE) %>%
  ungroup %>%
  left_join(inputs$recipe %>% select(obj.element, var, fn, scale, priority.weight, target)) %>%
  left_join(synlong) %>%
  mutate(coeff=case_when(fn=="val.sum" ~ value,
                         fn=="val.pos" ~ value*(value>0),
                         fn=="val.neg" ~ value*(value<0),
                         fn=="n.sum" ~ 1,
                         fn=="n.pos" ~ 1*(value>0),
                         fn=="n.neg" ~ 1*(value<0),
                         TRUE  ~ 0)) %>%
  select(obj.element, var, fn, wtnum, scale, priority.weight, value, coeff, target)
# glimpse(coeffs)
# ht(coeffs)
inputs$coeffs <- coeffs


#******************************************************************************************************************
#  run ipoptr ####
#******************************************************************************************************************
# inputs$recipe

# bounds on the weights
xlb <- rep(1, nrow(synfile))
#xub <- rep(1.5*max(puf.full$wt), nrow(synfile))
xub <- rep(max(puf.full$wt), nrow(synfile))

# starting point:
x0 <- (xlb + xub) / 2
x0 <- x0 * sum(puf.full$wt / sum(x0))

x0 <- rnorm(nrow(synfile), mean(puf.full$wt), sd(puf.full$wt))
x0 <- ifelse(x0<1, 1, x0)
x0 <- ifelse(x0>max(puf.full$wt), max(puf.full$wt), x0)
min(x0)
max(x0)


# PRE-CHECK: Take a look at the values at the starting point
start <- inputs$recipe %>%
  rowwise() %>%
  mutate(calc=do.call(fn, list(synfile, var, x0)),
         diff=calc - target,
         pdiff=diff / target * 100,
         apdiff=abs(pdiff),
         sdiffsq=(diff / scale)^2,
         objfn=sdiffsq * priority.weight) %>%
  ungroup
start %>% arrange(-apdiff)
start %>% arrange(-sdiffsq)
start %>% arrange(-objfn)
# END PRE-CHECK

opts <- list("print_level" = 5,
             "file_print_level" = 5, # integer
             "linear_solver" = "ma57", # mumps pardiso ma27 ma57 ma77 ma86 ma97
             "max_iter"=500,
             # "derivative_test"="first-order",
             # "derivative_test_print_all"="yes",
             "output_file" = "scratch.out")

a <- proc.time()
result <- ipoptr(x0 = x0,
                 lb = xlb,
                 ub = xub,
                 eval_f = eval_f_full_scaled, 
                 eval_grad_f = eval_grad_f_full_scaled,
                 opts = opts,
                 inputs = inputs)
b <- proc.time()
b - a

# optim_example <- list()
# optim_example$result <- result
# optim_example$puf.full <- puf.full
# optim_example$synfile <- synfile
# optim_example$inputs <- inputs
# optim_example$recipe.flagged <- recipe.flagged
# saveRDS(optim_example, "./results/optim_example.rds")


#******************************************************************************************************************
#  Examine results ####
#******************************************************************************************************************
# retrieve a previous run or else use the results from above
optim_example <- readRDS("./results/optim_example.rds")
result <- optim_example$result
puf.full <- optim_example$puf.full
synfile <- optim_example$synfile
inputs <- optim_example$inputs
recipe.flagged <- optim_example$recipe.flagged


names(result)

# ------------------
w.sol <- result$solution
# w.sol <- val$solution

comp <- inputs$recipe %>%
  rowwise() %>%
  mutate(calc=do.call(fn, list(synfile, var, w.sol)),
         diff=calc - target,
         pdiff=diff / target * 100,
         apdiff=abs(pdiff),
         sdiffsq=(diff / scale)^2, # scaled diff sq
         objfn=sdiffsq * priority.weight) %>% # weighted sdiffsq -- the element in the objective function
  select(obj.element, var, fn, scale, priority.weight, target, calc, diff, pdiff, apdiff, sdiffsq, objfn, vdesc)

sum(comp$objfn)
result$objective 

comp %>%
  arrange(-sdiffsq)

comp %>%
  arrange(-objfn)

comp %>%
  arrange(-apdiff)

comp %>%
  arrange(apdiff)

comp %>% filter(var %in% c("wt", 'c00100', "e00200", "taxbc"))

quantile(w.sol, probs=0:10/10)
quantile(synfile$wt, probs=0:10/10)
quantile(puf.full$wt, probs=0:10/10)

p <- bind_rows(tibble(w=puf.full$wt, type="1_puf"),
          tibble(w=result$solution, type="2_weights_from_scratch"),
          tibble(w=synfile$wt, type="3_synthesized")) %>%
  ggplot(aes(w)) +
  geom_histogram(binwidth=25, fill="blue") +
  geom_vline(aes(xintercept = median(w))) +
  scale_x_continuous(breaks=seq(0, 5000, 250)) +
  theme(axis.text.x=element_text(size=8, angle=30)) +
  facet_wrap(~type, nrow=3) +
  ggtitle("Distribution of weights")
p

# ggsave("./results/optim_example_hist.png", plot=p)


#******************************************************************************************************************
# Solve the problem for the full file, in pieces ####
#******************************************************************************************************************
# get full versions of the two files
puf <- mrgdf %>% filter(ftype=="puf.full")
syn <- mrgdf %>% filter(ftype==sfname)
# note that RECID is sequential from 1 on puf to the highest value on syn and is unique


#.. examine the data to estimate where we should split it ----
count(puf, MARS)
ycuts <- cut(puf$c00100, 10)
levels(ycuts)

# determine uniform cut points
puf %>% 
  mutate(igroup=ntile(c00100, 15)) %>% 
  group_by(igroup) %>%
  summarise(ymin=min(c00100), ymax=max(c00100), n=n(), wt.sum=sum(wt), agib=sum(wt * c00100) / 1e9)
ibreaks <- c(-Inf, 0, 5e3, 10e3, 15e3, 20e3, 25e3, 40e3, 50e3, 75e3, 100e3,
             150e3, 200e3, 300e3, 400e3, 500e3,
             750e3, 1e6, 1.5e6, 2e6, 5e6, 10e6, Inf)

tmp <- puf %>% 
  select(MARS, wt, c00100) %>%
  mutate(mgroup=ifelse(MARS %in% 1:2, MARS, 3),
         igroup=cut(c00100, ibreaks, include.lowest = TRUE)) %>%
  group_by(mgroup, igroup)
count(tmp, mgroup, igroup) %>% spread(mgroup, n)


tmp <- puf %>% 
  select(MARS, wt, c00100) %>%
  mutate(mgroup=ifelse(MARS %in% 1:2, MARS, 3),
         inum=case_when(mgroup==1 ~ 12,
                        mgroup==2 ~ 20,
                        mgroup==3 ~ 10,
                        TRUE ~ 0)) %>%
  group_by(mgroup) %>%
  mutate(igroup=ntile(c00100, inum))

gbreaks <- tmp %>%
  group_by(mgroup, igroup) %>%
  summarise(imin=min(c00100), imax=max(c00100), n=n(), wt.sum=sum(wt), agib=sum(wt * c00100) / 1e9)
gbreaks

# how does synfile look if we choose these exact breaks?
i1 <- gbreaks %>% filter(mgroup==1) %>% .[["imin"]]
i1 <- c(-Inf, i1[-1], Inf)
i1

i2 <- gbreaks %>% filter(mgroup==2) %>% .[["imin"]]
i2 <- c(-Inf, i2[-1], Inf)
i2

i3 <- gbreaks %>% filter(mgroup==3) %>% .[["imin"]]
i3 <- c(-Inf, i3[-1], Inf)
i3

# adjust the breaks so that there is always a zero
adjb <- function(b) c(b[1], 0, b[2:length(b)])
adjb(i3)

i1a <- adjb(i1)
i2a <- adjb(i2)
i3a <- adjb(i3)

f <- function(c00100, mgroup){
  brks <- if(mgroup[1]==1) i1a else
    if(mgroup[1]==2) i2a else
      if(mgroup[1]==3) i3a
  # igroup <- cut(c00100, brks, include.lowest = TRUE) %>% as.factor
  igroup.element <- function(c00100) min(which(c00100 < brks)) - 1
  igroup <- sapply(c00100, igroup.element)
  return(igroup)
}

# look at two files together
btmp <- bind_rows(puf %>% mutate(type="puf"),
                  syn %>% mutate(type="syn")) %>%
  select(type, MARS, wt, c00100) %>%
  mutate(mgroup=ifelse(MARS %in% 1:2, MARS, 3)) %>%
  group_by(type, mgroup) %>%
  mutate(igroup=f(c00100, mgroup))
btmp %>% ungroup %>% filter(mgroup==1) %>% count(igroup)

btmp %>%
  group_by(type, mgroup, igroup) %>%
  summarise(n=n()) %>%
  spread(type, n) %>%
  mutate(diff=syn - puf) %>%
  ungroup %>%
  arrange(mgroup, igroup)

#.. create split rules ----
m1 <- tibble(imin=i1a[-length(i1a)], imax=i1a[-1], mgroup=1)
m2 <- tibble(imin=i2a[-length(i2a)], imax=i2a[-1], mgroup=2)
m3 <- tibble(imin=i3a[-length(i3a)], imax=i3a[-1], mgroup=3)
split.rules <- bind_rows(m1, m2, m3) %>% mutate(group=row_number()) %>% select(group, mgroup, imin, imax)
split.rules

# prepare the files for splitting
getgroup <- function(mgroup.in, c00100){
  split <- split.rules %>% filter(mgroup==mgroup.in[1])
  igroup.element <- function(c00100) min(which(c00100 < split$imax))
  group <- split$group[sapply(c00100, igroup.element)]
  # split$group[min(which(c00100 < split$imax))]
  return(group)
}
getgroup(1, c(-100, -1, 0, 1))

idfile <- mrgdf %>%
  mutate(mgroup=ifelse(MARS %in% 1:2, MARS, 3)) %>%
  group_by(ftype, mgroup) %>%
  mutate(group=getgroup(mgroup, c00100)) %>%
  ungroup %>%
  select(ftype, RECID, mgroup, group) %>%
  arrange(RECID)
ht(idfile)
count(idfile, mgroup, ftype, group) %>% spread(ftype, n) %>% mutate(diff=synthpop3 - puf.full, sum=puf.full + synthpop3)

# now we are ready to run the file in pieces
getrec <- function(puf.full, synfile){
  dropvars <- c("e00100", "DSI", "EIC", "f2441", "f6251", "fded", "MARS", "MIDR", "n24", "RECID", "s006", "wt",
                "e07400", "e11200", "e03300", "e00800", "e07600", "p08000", "e02100p", "e02100s", "e00900p", "e00900s")
  varlist <- names(puf.full %>% select_if(is.numeric)) %>% 
    sort %>%
    setdiff(., dropvars)
  fnlist <- c("n.pos", "n.neg", "val.sum", "val.pos", "val.neg")
  
  recipe <- expand.grid(var=varlist, fn=fnlist, stringsAsFactors = FALSE) %>% 
    as_tibble() %>%
    arrange(var, fn)
  recipe <- bind_rows(tibble(var="wt", fn="n.sum"), recipe)
  
  tscale <- 1
  recipe <- recipe %>%
    rowwise() %>%
    mutate(target=do.call(fn, list(puf.full, var, puf.full$wt))) %>%
    ungroup %>%
    mutate(scale=ifelse(target!=0, abs(target / tscale), 1/ tscale),
           obj.element=paste0(var, "_", fn)) %>%
    select(obj.element, var, fn, scale, target) %>%
    arrange(var, fn)
  
  recipe.flagged <- recipe %>%
    rowwise() %>%
    mutate(syn.unwtd=do.call(fn, list(synfile, var, rep(1, nrow(synfile))))) %>% # so we can check if negs!
    group_by(var) %>%
    mutate(flag.dropneg=ifelse(str_detect(fn, "neg") & target==0 & syn.unwtd==0, 1, 0),
           flag.dropdupsum=ifelse(target==target[match("val.sum", fn)] & (fn=="val.pos"), 1, 0),
           flag.dropdupn=ifelse(target==target[match("n.sum", fn)] & (fn=="n.pos"), 1, 0)) %>%
    mutate_at(vars(starts_with("flag")), funs(naz)) %>%
    ungroup %>%
    arrange(var, fn)
  
  # remove recipe elements where the target is zero
  recipe.use <- recipe.flagged %>%
    filter(!(flag.dropneg | flag.dropdupsum | flag.dropdupn)) %>%
    filter(target!=0) %>%
    select(obj.element, var, fn, scale, target)
  
  # finally, add priority weights
  recipe.use <- recipe.use %>%
    mutate(priority.weight=case_when(var %in% c("wt", "c00100", "e00200", "taxbc") ~ 100,
                                     fn %in% c("n.sum", "val.sum") ~ 10,
                                     TRUE ~ 1))  %>% 
    left_join(puf.vnames %>% select(var=vname, vdesc))
  return(list(recipe.use=recipe.use, recipe.flagged=recipe.flagged))
}

getinplist <- function(synfile, recipe.use){
  inputs <- list()
  inputs$recipe <- recipe.use
  inputs$synsub <- synfile[, unique(inputs$recipe$var)] %>% mutate(wt=1)
  synlong <- inputs$synsub %>%
    mutate(wtnum=row_number()) %>%
    gather(var, value, -wtnum)
  
  # create a data frame with one row for each weight and obj.element combination
  coeffs <- expand.grid(wtnum=1:nrow(inputs$synsub), obj.element=inputs$recipe$obj.element, stringsAsFactors = FALSE) %>%
    ungroup %>%
    left_join(inputs$recipe %>% select(obj.element, var, fn, scale, priority.weight, target)) %>%
    left_join(synlong) %>%
    mutate(coeff=case_when(fn=="val.sum" ~ value,
                           fn=="val.pos" ~ value*(value>0),
                           fn=="val.neg" ~ value*(value<0),
                           fn=="n.sum" ~ 1,
                           fn=="n.pos" ~ 1*(value>0),
                           fn=="n.neg" ~ 1*(value<0),
                           TRUE  ~ 0)) %>%
    select(obj.element, var, fn, wtnum, scale, priority.weight, value, coeff, target)

  inputs$coeffs <- coeffs
  return(inputs)
}

# names(inputs)
# 
# group.ind <- 1

for(group.ind in 1:max(idfile$group)){
  base <- left_join(idfile %>% filter(group==group.ind), mrgdf)
  puf.full <- base %>% filter(ftype=="puf.full")
  synfile <- base %>% filter(ftype==sfname)
  recipes <- getrec(puf.full, synfile)
  recipe.use <- recipes$recipe.use
  inputs <- getinplist(synfile, recipe.use)
  
  # bounds on the weights
  xlb <- rep(1, nrow(synfile))
  xub <- rep(1.5*max(puf.full$wt), nrow(synfile))
  
  # starting point:
  x0 <- (xlb + xub) / 2
  x0 <- x0 * sum(puf.full$wt / sum(x0))
  
  opts <- list("print_level" = 5,
               "file_print_level" = 5, # integer
               "linear_solver" = "ma57", # mumps pardiso ma27 ma57 ma77 ma86 ma97
               "max_iter"=300,
               "output_file" = paste0(globals$tc.dir, "weight_pieces/output_group_", group.ind, ".out"))
  
  result <- ipoptr(x0 = x0,
                   lb = xlb,
                   ub = xub,
                   eval_f = eval_f_full_scaled, 
                   eval_grad_f = eval_grad_f_full_scaled,
                   opts = opts,
                   inputs = inputs)
  
  optim <- list()
  optim$result <- result
  optim$puf.full <- puf.full
  optim$synfile <- synfile
  optim$inputs <- inputs
  optim$recipe.flagged <- recipes$recipe.flagged
  saveRDS(optim, paste0(globals$tc.dir, "weight_pieces/optim_group_", group.ind, ".rds"))
}


#******************************************************************************************************************
# Temporary clunky approach to getting all 3 weights for the synfile ####
#******************************************************************************************************************

tmp <- readRDS(paste0(globals$tc.dir, sfname, "_reweighted_stackedfiles.rds"))
weights <- tibble(rownum=tmp$RECID[tmp$ftype=="puf.full"],
                  puf.RECID=tmp$RECID[tmp$ftype=="puf.full"],
                  puf.wt=tmp$wt[tmp$ftype=="puf.full"],
                  syn.RECID=tmp$RECID[tmp$ftype=="synthpop3"],
                  syn.wt=tmp$wt[tmp$ftype=="synthpop3"],
                  syn.rwt=tmp$wt[tmp$ftype=="synthpop3.rwt"])
ht(weights)


#******************************************************************************************************************
# Examine results for full file ####
#******************************************************************************************************************


getpiece <- function(group.ind){
  optim <- readRDS(paste0(globals$tc.dir, "weight_pieces/optim_group_", group.ind, ".rds"))
}

n <- 45
optlist <- llply(1:n, getpiece, .progress="text")
memory()

# group.ind <- 2

length(optlist)
names(optlist[[1]])
names(optlist[[1]]$inputs)
optlist[[1]]$inputs$recipe
optlist[[1]]$recipe.flagged
unique(optlist[[1]]$recipe.flagged$var)
names(optlist[[1]]$result)
names(optlist[[1]]$synfile)

# analyze summary result
obj.vals <- laply(1:n, function(i) optlist[[i]]$result$objective)
quantile(obj.vals) %>% round(2)

message <- laply(1:n, function(i) optlist[[i]]$result$message)
count(tibble(message), message)

# aggregate the pieces of the puf and synthetic files, and attach new weights
# first puf
puf.agg <- ldply(1:n, function(i) optlist[[i]]$puf.full)
names(puf.agg)
ht(puf.agg[, c(1:5, (ncol(puf.agg)-5):ncol(puf.agg))]) # NOTE that RECIDs are not in order
min(puf.agg$RECID); max(puf.agg$RECID)
puf.agg <- puf.agg %>% arrange(RECID)


# now synthetic
syn.agg <- ldply(1:n, function(i) {optlist[[i]]$synfile %>% mutate(syn.wtfs=optlist[[i]]$result$solution)})
names(syn.agg)
ht(syn.agg[, c(1:5, ncol(syn.agg))]) # RECIDs not in order here, either
# add the other synthetic weights
syn.agg <- syn.agg %>%
  arrange(RECID) %>%
  left_join(weights %>% select(RECID=syn.RECID, syn.wt, syn.rwt)) %>%
  mutate(wt=syn.wtfs)
ht(syn.agg[, c(1:5, (ncol(syn.agg)-5):ncol(syn.agg))])
write_csv(syn.agg, paste0(globals$synd, "synthpop3_allwts.csv"))

cor(syn.agg %>% select(syn.wt, syn.rwt, syn.wtfs))


stack <- bind_rows(puf.agg %>% mutate(ftype="puf"),
                syn.agg %>% mutate(ftype="syn.raw", wt=syn.wt),
                syn.agg %>% mutate(ftype="syn.rwt", wt=syn.rwt),
                syn.agg %>% mutate(ftype="syn.wtfs", wt=syn.wtfs))

p <- stack %>%
  ggplot(aes(wt)) +
  geom_histogram(binwidth=25, fill="blue") +
  geom_vline(aes(xintercept = median(wt))) +
  scale_x_continuous(breaks=seq(0, 5000, 250), limits=c(0, 3000)) +
  theme(axis.text.x=element_text(size=8, angle=30)) +
  facet_wrap(~ftype, nrow=2) +
  ggtitle("Distribution of weights")
p
ggsave(filename="./results/weights_hist.png", plot=p, width=10, height=6, units="in")

#.. summary statistics on the results ----
agiranges <- c(-Inf, 0, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, 10e6, Inf)
f <- function(var, wt=wt) {sum(var * wt / 1e9)}

var <- "e00200"

vlist <- c("c00100", "e00200", "e00300", "e00600", "e01700", "p23250", "taxbc")
dfsums <- stack %>%
  mutate(agirange=cut(c00100, agiranges, right=FALSE),
         ftype=str_remove(ftype, "syn."),
         wtone=1e9) %>%
  select(ftype, agirange, wt, wtone, vlist) %>%
  gather(vname, value, -ftype, -agirange, -wt) %>%
  group_by(ftype, agirange, vname) %>%
  summarise(n=n(), wtsum.m=sum(wt) / 1e6, valsum.b=sum(wt * value) / 1e9) %>%
  left_join(puf.vnames %>% select(vname, vdesc))
dfsums
totrows <- dfsums %>%
  group_by(ftype, vname, vdesc) %>%
  summarise_at(vars(n, wtsum.m, valsum.b), funs(sum)) %>%
  mutate(agirange="Total")
dfall <-bind_rows(dfsums, totrows) %>%
  ungroup %>%
  mutate(agirange=factor(agirange, levels=c(levels(dfsums$agirange), "Total")),
         avgval=valsum.b * 1e3 / wtsum.m) %>%
  arrange(ftype, vname, vdesc, agirange)

tab <- function(vname.in, ttype="sum"){
  digits.id <- rep(0, 3)
  digits.vals <- rep(1, 7)
  digits.pdiff <- rep(1, 3)
  digits.other <- 0
  if(ttype=="sum") {
    selvar <- "valsum.b"
    if(vname.in=="wtone") digits.vals <- rep(0, 7)
    } else
    if(ttype=="avg") {
      selvar <- "avgval"
      digits.vals <- rep(0, 7)
    }
  digits.tab <- c(digits.id, digits.vals, digits.pdiff, digits.other)
  df <- dfall %>%
    filter(vname==vname.in) %>%
    select(ftype, agirange, vname, vdesc, var=selvar) %>%
    mutate(ttype=ttype,
           vdesc=str_sub(vdesc, 1, 20)) %>%
    spread(ftype, var) %>%
    mutate_at(vars(raw, rwt, wtfs), funs(diff=. - puf, pdiff=(. - puf) / puf * 100)) %>%
    select(-vdesc, everything(), vdesc)
  dfk <- df %>%
    kable(digits=digits.tab, format.args=list(big.mark = ','))
  return(dfk)
}

tab("wtone", "sum")

tab("c00100", "sum"); tab("c00100", "avg")
var <- "e00200"; suppressWarnings(tab(var, "sum")); suppressWarnings(tab(var, "avg"))
var <- "e00300"; suppressWarnings(tab(var, "sum")); suppressWarnings(tab(var, "avg"))
var <- "e00600"; suppressWarnings(tab(var, "sum")); suppressWarnings(tab(var, "avg"))
var <- "p23250"; suppressWarnings(tab(var, "sum")); suppressWarnings(tab(var, "avg"))
var <- "taxbc"; suppressWarnings(tab(var, "sum")); suppressWarnings(tab(var, "avg"))


#.. examine tax reforms ----
reform.fn <- "rate_cut.json"
reform.fn <- "toprate.json"
# reform.fn <- "EITC.json"
reform <- readRDS(paste0(altruns.dir, str_remove(basename(reform.fn), ".json"), ".rds"))

min(reform$RECID); max(reform$RECID)
min(df$RECID); max(df$RECID)

stack.mrg <- left_join(stack, reform %>% select(RECID, taxbc.reform=taxbc))
glimpse(stack.mrg)
count(stack.mrg, ftype)

f <- function(var, wt=wt) {sum(var * wt / 1e9)}
stack.mrg %>%
  group_by(ftype) %>%
  summarise(n=n(), wtm=sum(wt / 1e6), 
            agib=f(c00100, wt), 
            wageb=f(e00200, wt), 
            taxbcb=f(taxbc, wt),
            taxbc.reform=f(taxbc.reform, wt)) %>%
  gather(var, value, -ftype) %>%
  spread(ftype, value) %>%
  mutate_at(vars(syn.raw, syn.rwt, syn.wtfs), funs(diff=. - puf, pdiff=(. - puf) / puf * 100))


# get the % change in tax by file
dtot <- function(df){
  dsums <- df %>% 
    summarise_at(vars(-ftype, -agirange), funs(sum)) %>%
    mutate(ftype=df$ftype[1], agirange="Total")
  dfout <- bind_rows(df, dsums)
  return(dfout)
}

diffs <- stack.mrg %>%
  mutate(agirange=cut(c00100, agiranges, right=FALSE)) %>%
  group_by(ftype, agirange) %>%
  summarise_at(vars(taxbc, taxbc.reform), funs(sum(. * wt) / 1e9)) %>%
  do(dtot(.)) %>%
  mutate(agirange=factor(agirange, levels=unique(agirange), ordered=TRUE),
         diff=taxbc.reform - taxbc,
         pdiff=diff / taxbc * 100)

diffs %>%
  select(ftype, agirange, taxbc) %>%
  spread(ftype, taxbc) %>%
  mutate(var="taxbc$b", reform=reform.fn) %>%
  kable(digits=1)

diffs %>%
  select(ftype, agirange, taxbc.reform) %>%
  spread(ftype, taxbc.reform) %>%
  mutate(var="taxbc.reform$b", reform=reform.fn) %>%
  kable(digits=1)

diffs %>%
  select(ftype, agirange, diff) %>%
  spread(ftype, diff) %>%
  mutate(var="change.v.baseline$b", reform=reform.fn) %>%
  kable(digits=1)

diffs %>%
  select(ftype, agirange, pdiff) %>%
  spread(ftype, pdiff) %>%
  mutate(var="pct.change.v.baseline", reform=reform.fn) %>%
  kable(digits=1)


#******************************************************************************************************************
# DEFUNCT -- look at other nonlinear solvers ####
#******************************************************************************************************************

#******************************************************************************************************************
# trustOptim ####
#******************************************************************************************************************
# NO GOOD - cannot set bounds, gives negative weights
install.packages("trustOptim")
library("trustOptim")

val <- trust.optim(x0, fn=eval_f_full_scaled, gr=eval_grad_f_full_scaled, hs=NULL,
                   method = "SR1", control = list(report.precision=1L, function.scale.factor=-1),
                   inputs=inputs)


#******************************************************************************************************************
# optimx ####
#******************************************************************************************************************
# install.packages("optimx")
# install.packages("numDeriv")
# numDeriv
library("optimx")
# c("Nelder-Mead","BFGS")
# methods that allow box constraints
# Rcgmin bobyqa L-BFGS-B Rvmmmin maybe spg

grad.nd <- function(x, inputs) {
  require(numDeriv)
  grad.nd <- grad(eval_f_full_scaled, x, inputs=inputs)
  return(grad.nd)
}

opx <- optimx(x0, fn=eval_f_full_scaled, gr=grad.nd, hess=NULL,
              lower=xlb, upper=xub,
              method="bobyqa", itnmax=100, hessian=FALSE,
              control=list(trace=3),
              inputs=inputs)

opx <- optimx(x0, fn=eval_f_full_scaled, gr=eval_grad_f_full_scaled, hess=NULL,
              lower=xlb, upper=xub,
              method="bobyqa", itnmax=100, hessian=FALSE,
              control=list(trace=3),
              inputs=inputs)





