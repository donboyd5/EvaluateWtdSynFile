



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



#****************************************************************************************************
#                Initialization ####
#****************************************************************************************************
puf.vnames <- get_puf_vnames()


#******************************************************************************************************************
#  Get previously-prepared synfile-PUF and tax output, merge, and separate PUF and synfile ####
#******************************************************************************************************************
sfname <- "synthpop3"
synprep <- readRDS(paste0(globals$tc.dir, sfname, "_rwprep.rds"))

# merge and then split
tcvars <- c("c00100", "taxbc")
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
#  helper functions ####
#******************************************************************************************************************

n.sum <- function(df, var, weight, condition=TRUE){
  # get the weighted number of records for which a logical condition related to the variable is met
  # var is a numeric column in df
  # weight is a vector of weights (could be a column in df, or could be external)
  # condition: boolean expression as text
  # returns a scalar which is the weighted number of records
  condition <- parse(text=condition)
  df %>%
    select(variable=!!var) %>%
    summarise(value=sum(weight * eval(condition))) %>%
    .[[1]]
}


val.sum <- function(df, var, weight, condition=TRUE){
  # get the weighted value of a variable for which a logical condition related to the variable is met
  # var is a numeric column in df
  # weight is a vector of weights (could be a column in df, or could be external)
  # condition: boolean expression as text
  # returns a scalar which is the weighted value of the variable
  condition <- parse(text=condition)
  df %>% 
    select(variable=!!var) %>%
    summarise(value=sum(variable * weight * eval(condition))) %>%
    .[[1]]
}


n.pos <- function(df, var, weight){
  n.sum(df, var, weight, condition="variable>0")
}

n.neg <- function(df, var, weight){
  n.sum(df, var, weight, condition="variable<0")
}


val.pos <- function(df, var, weight){
  val.sum(df, var, weight, condition="variable>0")
}

val.neg <- function(df, var, weight){
  val.sum(df, var, weight, condition="variable<0")
}

# val.pos(puf.full, vlist[6], puf.full$wt)
# val.neg(puf.full, vlist[6], puf.full$wt)


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
varlist <- names(puf.full %>% select_if(is.numeric)) %>% 
  sort %>%
  setdiff(., c("DSI", "f2441", "f6251", "fded", "MARS", "MIDR", "n24", "RECID", "s006", "wt",
               "e07400", "e11200", "e03300", "e00800", "e07600", "p08000"))
# e00800 is alimony received -- dropped it based on this
# e07600 is Credit for prior year minimum tax, also dropped
# p08000 is other credits, also dropped
varlist
#varlist <- c("c00100", "taxbc", "e00200", "e00200p", "e01700", "e00300", "e00650", "p23250", "XTOT")
fnlist <- c("n.pos", "n.neg", "val.sum", "val.pos", "val.neg")

recipe <- expand.grid(var=varlist, fn=fnlist, stringsAsFactors = FALSE) %>% 
  as_tibble() %>%
  arrange(var, fn)
recipe <- bind_rows(tibble(var="wt", fn="n.sum"), recipe)
recipe
# for(i in 1:nrow(recipe)) recipe$target[i] <- do.call(recipe$fn[i], list(puf.full, recipe$var[i], puf.full$wt))

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
                                   TRUE ~ 1))
recipe.use


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


# callit <- function(FUN, df, var){FUN(df, var)}
# callit(val.sum, puf.full, "p23250")


#******************************************************************************************************************
#  run ipoptr ####
#******************************************************************************************************************

# inputs$recipe

xlb <- rep(1, nrow(synfile))
xub <- rep(1.5*max(puf.full$wt), nrow(synfile))
x0 <- (xlb + xub) / 2

# DIAGNOSTIC: Take a look at the values at the starting point
start <- inputs$recipe %>%
  rowwise() %>%
  mutate(calc=do.call(fn, list(synfile, var, x0)),
         diff=calc - target,
         pdiff=diff / target * 100,
         apdiff=abs(pdiff),
         diffsq=(diff / scale)^2) %>%
  ungroup
start %>% arrange(-apdiff)
start %>% arrange(-diffsq)
# e00800 is alimony received -- dropped it based on this
# e07600 is Credit for prior year minimum tax, also dropped
# p08000 is other credits, also dropped
# END DIAGNOSTIC

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


#******************************************************************************************************************
#  Examine results ####
#******************************************************************************************************************
names(result)

w.sol <- result$solution
# w.sol <- val$solution

quantile(w.sol, probs=0:10/10)
quantile(synfile$wt, probs=0:10/10)
quantile(puf.full$wt, probs=0:10/10)

comp <- inputs$recipe %>%
  rowwise() %>%
  mutate(calc=do.call(fn, list(synfile, var, w.sol)),
         diff=calc - target,
         pdiff=diff / target * 100,
         apdiff=abs(pdiff),
         diffsq=(diff / scale)^2)

sum(comp$diffsq)
result$objective

comp %>%
  arrange(-diffsq)

comp %>%
  arrange(-apdiff)

comp %>%
  arrange(apdiff)

comp %>% filter(var %in% c('c00100', "e00200", "taxbc"))

wt <- result$solution
wt <- puf.full$wt
wt <- synfile$wt
tibble(w=wt) %>%
  ggplot(aes(w)) +
  geom_histogram(binwidth=25, fill="blue") +
  geom_vline(aes(xintercept = median(w))) +
  # scale_x_continuous(breaks=seq(0, max(w), .05)) +
  theme(axis.text.x=element_text(size=8, angle=30)) +
  ggtitle("Distribution of weights")





#******************************************************************************************************************
#  define functions -- AUTOMATED ####
#******************************************************************************************************************
eval_f_full_scaled <- function(w, inputs) {
  # objective function - evaluates to a single number
  
  # w is the vector of weights computed by ipopt in this iteration
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  
  # inputs$recipe is a data frame where each row defines an element of the objective function, with columns:
  #   obj.element -- a short name for the element, giving the variable involved and the function to be applied to it
  #   var -- the variable involved -- e.g., c00100 or e00200
  #   fn -- the function to be applied -- one of n.sum, val.sum, n.pos, val.pos, n.neg, val.neg
  #   multiplier -- a weighting factor that can make this element more important than other elements
  #     default is 1; larger numbers make an element more important
  #   target -- the value for this calculation as applied to the target puf file
  
  # inputs$synsub is a dataframe that is a subset of the synfile that only includes the columns needed to
  #   calculate the objective function. 
  #     The wt column is special in that its value is set to 1, to make
  #     calculations involving a weight variable easy. (Multiplying this iteration's weight vector, w, by
  #     the synsub$wt weight variable (i.e., 1), gives the tentative weight for the file. The sum of this is
  #     of course the sum of weights. THis allows us to treat the weight like it is any other variable.)
  
  # obj -- calculated below is the objective function value; it is the sum across all elements of the:
  #   priority-weighted 
  #     squared differences of the 
  #       sum of values calculated for an element using the weight vector from this iteration, minus the
  #         corresponding target value in the puf calculated  using its weights
  # ratio <- function(target.calc, target) ifelse(target==0, 1, target.calc / target)
  
  temp <- inputs$recipe %>%
    rowwise() %>%
    # the mutate step applies this element's function to this element's variable, using this iteration's weights:
    mutate(target.calc=do.call(fn, list(inputs$synsub, var, w))) %>% 
    ungroup %>%
    mutate(obj=priority.weight * {(target.calc / scale - target / scale)^2})
  
  obj <- sum(temp$obj)
  
  return(obj)
}
# w <- x0


* grad_f[         11] = 8.9000438541168797e+008    ~ 8.8990305062740314e+008  [1.139e-004]

w1 <- x0; w2 <- w1
n <- 11
w2[n] <- w1[n] + 1
a <- eval_f_full_scaled(w2, inputs) - eval_f_full_scaled(w1, inputs)
b <- eval_grad_f_full_scaled(w, inputs)[n]
cbind(a, b, b - a)

# * grad_f[         11] = 8.9000438541168797e+008    ~ 8.8990305062740314e+008  [1.139e-004]
# * grad_f[         11] = 8.9000438541168797e+008    ~ 8.8990305062740314e+008  [1.139e-004]



eval_grad_f_full_scaled <- function(w, inputs){
  # gradient of objective function - a vector length w
  # giving the partial derivatives of obj wrt each w[i]
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  
  # inputs will include:
  #   for each target element in the objective function:
  #     the sum -- s1 for first target element, and so on, and
  #     the multiplier vector (e.g., wages, cap gains, etc.)
  
  # http://www.derivative-calculator.net/
  # https://www.symbolab.com/solver/partial-derivative-calculator
  # Example where we only have one target element in the objective function, and only have
  #   3 records and therefore 3 weights, and need to return a vector of 3 partial derivatives
  # Notation: w1, w2, and w3 are the first 3 weights in the vector of weights
  #           a1, a2, and a3 are the 3 constants they are multiplied by (e.g., wages1, wages2, wages3)
  #           t is the sum or target we are comparing to
  #           p is the priority weight of this element of the objective function
  #           calc is the calculated value of the target with the new weights  
  # The objective function to minimize is p*(w1*a1 + w2*a2 + w3*a3 - s)^2
  # The first partial derivatives of the objective function are:
  #   wrt w1:          2*a1*p*(w1*a1 + w2*a2 + w3*a3 - t) = 2*a1*p*(calc - t)
  #   wrt w2:          2*a2*p*(w1*a1 + w2*a2 + w3*a3 - t) = 2*a2*p*(calc - t)
  #   wrt w3:          2*a3*p*(w1*a1 + w2*a2 + w3*a3 - t) = 2*a3*p*(calc - t)
  # If we have multiple target elements, each vector element will have a more complex sum
  
  wdf <- tibble(wt.iter=w, wtnum=1:length(w))
  
  grad <- inputs$coeffs %>% 
    left_join(wdf, by="wtnum") %>%
    group_by(obj.element) %>%
    # mutate(gsum=sum(wt.iter*coeff), diff=gsum - first(target)) %>%
    # mutate(g2=(gsum + 1*coeff - target)^2 - (gsum - target)^2) %>%
    # mutate(cdiff=coeff*(sum(wt.iter*coeff) - first(target))) %>%
    # mutate(cscaled=cdiff /(first(scale)^2)) %>%
    # mutate(g3=2 * priority.weight * cscaled) %>%
    mutate(calc=sum(wt.iter*coeff)) %>%
    mutate(grad={2 * coeff * priority.weight * (calc - target)} / {scale^2}) %>%
    group_by(wtnum) %>%
    summarise(grad=sum(grad)) %>% # sum across all of the object elements for this particular weight
    ungroup
  
  return(grad$grad)
}


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





