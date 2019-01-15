



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
inputs <- list()
inputs$a <- rep(1, nrow(synfile))
inputs$a.target <- targs$one
inputs$b <- synfile$c00100
inputs$b.target <- targs$c00100
inputs$c <- synfile$taxbc
inputs$c.target <- targs$taxbc
inputs$d <- synfile$e00200
inputs$d.target <- targs$e00200


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

w <- result$solution
targs
sum(w * inputs$a)
sum(w * inputs$b)
sum(w * inputs$c)
sum(w * inputs$d)

quantile(w)




