



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
#  2. Get previously-prepared synfile-PUF and tax output, merge, and separate PUF and synfile ####
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
#  set targets ####
#******************************************************************************************************************
glimpse(puf.full)
targs <- puf.full %>%
  mutate(one=1) %>%
  summarise_at(vars(one, c00100, taxbc, e00200, e01700), funs(sum(. * wt)))
targs



#******************************************************************************************************************
#  helper functions ####
#******************************************************************************************************************
names(puf.full) %>% sort
vlist <- c("c00100", "taxbc", "e00200", "e01700", "e00300", "p23250")
df <- puf.full
var <- "c00100"
weight="wt"

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

n.sum(puf.full, vlist[6], puf.full$wt, "variable>0")
n.sum(synfile, vlist[6], synfile$wt, "variable>0")

n.sum(puf.full, vlist[6], puf.full$wt)
n.sum(puf.full, vlist[6], puf.full$wt, condition="variable>0")
n.sum(puf.full, vlist[6], puf.full$wt, condition="variable<0")
n.sum(puf.full, vlist[6], puf.full$wt, condition="variable==0")

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

val.sum(puf.full, vlist[6], puf.full$wt, "variable>0")
val.sum(synfile, vlist[6], synfile$wt, "variable>0")

val.sum(puf.full, vlist[6], puf.full$wt)
val.sum(puf.full, vlist[6], puf.full$wt, condition="variable>0")
val.sum(puf.full, vlist[6], puf.full$wt, condition="variable<0")
val.sum(puf.full, vlist[6], puf.full$wt, condition="variable==0")


n.pos <- function(df, var, weight){
  n.sum(df, var, weight, condition="variable>0")
}

n.neg <- function(df, var, weight){
  n.sum(df, var, weight, condition="variable<0")
}

n.pos(puf.full, vlist[6], puf.full$wt)
n.neg(puf.full, vlist[6], puf.full$wt)

val.pos <- function(df, var, weight){
  val.sum(df, var, weight, condition="variable>0")
}

val.neg <- function(df, var, weight){
  val.sum(df, var, weight, condition="variable<0")
}

val.pos(puf.full, vlist[6], puf.full$wt)
val.neg(puf.full, vlist[6], puf.full$wt)


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
                   e00200, val.sum")


names(puf.full) %>% sort
varlist <- c("c00100", "taxbc", "e00200", "e01700", "e00300", "p23250")
fnlist <- c("n.pos", "n.neg", "val.sum", "val.pos", "val.neg")

recipe <- expand.grid(var=varlist, fn=fnlist) %>% 
  mutate_all(as.character) %>% 
  as_tibble() %>%
  arrange(var, fn)
recipe <- bind_rows(tibble(var="wt", fn="n.sum"), recipe)
recipe
# for(i in 1:nrow(recipe)) recipe$target[i] <- do.call(recipe$fn[i], list(puf.full, recipe$var[i], puf.full$wt))


recipe <- recipe %>%
  rowwise() %>%
  mutate(target=do.call(fn, list(puf.full, var, puf.full$wt))) %>%
  ungroup %>%
  mutate(priority.weight=case_when(fn %in% c("n.sum", "val.sum") ~ 1,
                                   var %in% c("wt", "c00100", "e00200") ~ 1,
                                   TRUE ~ 1)) %>%
  mutate(element=paste0(var, "_", fn)) %>%
  select(element, var, fn, priority.weight, target)
recipe

w <- synfile$wt
w <- rep(2*max(puf.full$wt), nrow(synfile))
w <- w.opt
inputs <- list()
inputs$recipe <- recipe
inputs$synsub <- synfile[, unique(recipe$var)] %>% mutate(wt=1)

glimpse(inputs$synsub)

n.pos(synfile, "c00100", w)


t2 <- recipe %>%
  rowwise() %>%
  mutate(calc=do.call(fn, list(synfile, var, w))) %>%
  ungroup %>%
  mutate(obj=priority.weight * (calc - target)^2)
t2
sum(t2$obj)
eval_f_full_2(w, inputs)
eval_f_full(w, inputs2)

eval_f_full_2 <- function(w, inputs) {
  # objective function - evaluates to a single number
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  # here are the objective function, the 1st deriv, and the 2nd deriv
  # http://www.derivative-calculator.net/
  #                   objective function
  #                   first deriv
  #                   second deriv
  temp <- inputs$recipe %>%
    rowwise() %>%
    mutate(calc=do.call(fn, list(inputs$synsub, var, w))) %>%
    ungroup %>%
    mutate(obj=priority.weight * (calc - target)^2)
  
  obj <- sum(temp$obj)
  
  return(obj)
}



# callit <- function(FUN, df, var){FUN(df, var)}
# callit(val.sum, puf.full, "p23250")




#******************************************************************************************************************
#  define functions ####
#******************************************************************************************************************

eval_f_full <- function(w, inputs) {
  # objective function - evaluates to a single number
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  # here are the objective function, the 1st deriv, and the 2nd deriv
  # http://www.derivative-calculator.net/
  #                   objective function
  #                   first deriv
  #                   second deriv
  # diffsq <- function(w, target.num, inputs) {
  #   return(NULL)
  # }
  
  obj <- 
    (sum(w * inputs$a) - inputs$a.target)^2 + 
    (sum(w * inputs$b) - inputs$b.target)^2 + 
    (sum(w * inputs$c) - inputs$c.target)^2 +
    (sum(w * inputs$d) - inputs$d.target)^2
  
  return(obj)
}

eval_grad_f_full <- function(w, inputs){
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
  #           s is the sum or target we are comparing to
  # The objective function to minimize is (w1*a1 + w2*a2 + w3*a3 - s)^2
  # The first partial derivatives of the objective function are:
  #   wrt w1:          2*a1*(w1*a1 + w2*a2 + w3*a3 - s)
  #   wrt w2:          2*a2*(w1*a1 + w2*a2 + w3*a3 - s)
  #   wrt w3:          2*a3*(w1*a1 + w2*a2 + w3*a3 - s)
  # If we have multiple target elements, each vector element will have a more complex sum
  
  # make it easier to read:
  gradf <- double(length(w))
  gradf <- 2*inputs$a*(sum(w*inputs$a) - inputs$a.target) + 2*inputs$b*(sum(w*inputs$b) - inputs$b.target) +
    2*inputs$c*(sum(w*inputs$c) - inputs$c.target) + 2*inputs$d*(sum(w*inputs$d) - inputs$d.target)
  
  return(gradf)
}


#******************************************************************************************************************
#  6. Run ipoptr to get optimal x values ####
#******************************************************************************************************************
inputs2 <- list()
inputs2$a <- rep(1, nrow(synfile))
inputs2$a.target <- targs$one
inputs2$b <- synfile$c00100
inputs2$b.target <- targs$c00100
inputs2$c <- synfile$taxbc
inputs2$c.target <- targs$taxbc
inputs2$d <- synfile$e00200
inputs2$d.target <- targs$e00200


xlb <- rep(0, nrow(synfile))
xub <- rep(2*max(puf.full$wt), nrow(synfile))
x0 <- (xlb + xub) / 2

tmp <- eval_grad_f_full(x0, inputs)

opts <- list("print_level" = 5,
             "file_print_level" = 5, # integer
             "linear_solver" = "ma57", # mumps pardiso ma27 ma57 ma77 ma86 ma97
             "max_iter"=500,
             "output_file" = "scratch.out")

a <- proc.time()
result <- ipoptr(x0 = x0,
                 lb = xlb,
                 ub = xub,
                 eval_f = eval_f_full, 
                 eval_grad_f = eval_grad_f_full, 
                 opts = opts,
                 inputs = inputs)
b <- proc.time()
b - a


names(result)

w.opt <- result$solution
targs
sum(w * inputs$a)
sum(w * inputs$b)
sum(w * inputs$c)
sum(w * inputs$d)

quantile(w, probs=0:10/10)
quantile(synfile$wt, probs=0:10/10)
quantile(puf.full$wt, probs=0:10/10)

tibble(w=result$solution) %>%
  ggplot(aes(w)) +
  geom_histogram(binwidth=25, fill="blue") +
  geom_vline(xintercept = median(w)) +
  # scale_x_continuous(breaks=seq(0, max(w), .05)) +
  theme(axis.text.x=element_text(size=8, angle=30)) +
  ggtitle("Distribution of weights")



