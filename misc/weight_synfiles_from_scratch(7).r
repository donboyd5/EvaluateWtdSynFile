



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

# merge and then split
tcvars <- c("c00100", "taxbc") # taxcalc vars
mrgdf <- left_join(synprep$tc.base, synprep$tc.output %>% dplyr::select(RECID, tcvars))
glimpse(mrgdf)


#******************************************************************************************************************
#  pick a subset ####
#******************************************************************************************************************
mrgdf2 <- mrgdf %>%
  filter(MARS==2, c00100>=0, c00100<=25e3)

puf.full <- mrgdf2 %>% filter(ftype=="puf.full")
synfile <- mrgdf2 %>% filter(ftype==sfname)


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
xub <- rep(1.5*max(puf.full$wt), nrow(synfile))

# starting point:
x0 <- (xlb + xub) / 2
x0 <- x0 * sum(puf.full$wt / sum(x0))

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
             "output_file" = "scratch_new.out")

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

optim_example <- list()
optim_example$result <- result
optim_example$puf.full <- puf.full
optim_example$synfile <- synfile
optim_example$inputs <- inputs
optim_example$recipe.flagged <- recipe.flagged
saveRDS(optim_example, "./results/optim_example.rds")


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

comp %>% filter(var %in% c('c00100', "e00200", "taxbc"))

quantile(w.sol, probs=0:10/10)
quantile(synfile$wt, probs=0:10/10)
quantile(puf.full$wt, probs=0:10/10)

p <- bind_rows(tibble(w=puf.full$wt, type="1_puf"),
          tibble(w=result$solution, type="2_weights_from_scratch"),
          tibble(w=synfile$wt, type="3_reweighted_with_constraints")) %>%
  ggplot(aes(w)) +
  geom_histogram(binwidth=25, fill="blue") +
  geom_vline(aes(xintercept = median(w))) +
  scale_x_continuous(breaks=seq(0, 5000, 250)) +
  theme(axis.text.x=element_text(size=8, angle=30)) +
  facet_wrap(~type, nrow=3) +
  ggtitle("Distribution of weights")
p

ggsave("./results/optim_example_hist.png", plot=p)


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





