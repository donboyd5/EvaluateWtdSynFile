# 12/20/2018

# This program reweights a synthetic file so that it will hit targets in a base file, typically the PUF.

# CAUTION: Currently this is only set to work on the full PUF and a synfile of the full PUF

# The main things it does are:
# 1. Optionally prepare a file and its puf counterpart, if not already prepared.
#    - Get a synfile, stack it with PUF using only the variables that are in both files
#    - Run it through Tax-Calculator so that we can get desired calculated variables
#    - Save the prepared file and Tax-Calculator output as a list so that step 1 need not be done in the future on this file
# 2. Get the previously prepared list for a file
# 3. Define and construct a set of targets (weighted values) from the base file
# 4. Compare weighted values on the existing file to the targets to see which targets may be especially hard to hit
# 5. Construct a new reweighted file, synfile.rwt, that hits the targets
#    - Obtain adjustment factors for synfile weights that minimize a distortion function based on size of the adjustment,
#      while satisfying constraints that ensure that the targets are hit (or that results are within defined tolerances)
#    - Construct synfile.rwt by adjusting the wt variable (and save the raw synthesized weight as wt.rawsyn)
# 6. Do simple comparisons of base, synfile, and synfile.rwt
# 7. Save the resulting synfile.rwt so that it can be analyzed further



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
#                Additional functions ####
#****************************************************************************************************


#****************************************************************************************************
#                Initialization ####
#****************************************************************************************************
puf.vnames <- get_puf_vnames()



#****************************************************************************************************
#                1. Optionally prepare a file and its PUF counterpart ####
#****************************************************************************************************
#.. ONLY RUN THIS IF NOT PREVIOUSLY DONE FOR A PARTICULAR SYNTHETIC FILE!! ----

sfname <- "synthpop3"

system.time(synprep <- prep_for_reweighting(sfname))

# take a quick look at the results
names(synprep)
glimpse(synprep$tc.base)
glimpse(synprep$tc.output)
count(synprep$tc.base, ftype)

saveRDS(synprep, paste0(globals$tc.dir, sfname, "_rwprep.rds"))


#****************************************************************************************************
#                2. Get a previously prepared list for a file, split out the merged files ####
#****************************************************************************************************
sfname <- "synthpop3"
synprep <- readRDS(paste0(globals$tc.dir, sfname, "_rwprep.rds"))

# merge and then split
tcvars <- c("c00100", "taxbc")
mrgdf <- left_join(synprep$tc.base, synprep$tc.output %>% dplyr::select(RECID, tcvars))
glimpse(mrgdf)

puf.full <- mrgdf %>% filter(ftype=="puf.full")
synfile <- mrgdf %>% filter(ftype==sfname)


#****************************************************************************************************
#                3. Define and construct a set of targets (weighted values) from the base file ####
#****************************************************************************************************
# create a set of logical rules that will define subsets of the data for which we will define targets
# and variables that we will target in those subsets, and whether we are targeting
# their weighted sums or their weighted number of records (can do both)

#..Define targets (constraints) ----
# create strings that define logical rules we will apply
agi.ranges <- c(
  "c00100 < 0",
  "c00100 == 0",
  "c00100 > 0 & c00100 <= 25e3",
  "c00100 > 25e3 & c00100 <= 50e3",
  "c00100 > 50e3 & c00100 <= 75e3",
  "c00100 > 75e3 & c00100 <= 100e3",
  "c00100 > 100e3 & c00100 <= 200e3",
  "c00100 > 200e3 & c00100 <= 500e3",
  "c00100 > 500e3 & c00100 <= 1e6",
  "c00100 > 1e6 & c00100 <= 10e6",
  "c00100 > 10e6 & c00100 <= Inf")
agi.ranges

names(synfile) %>% sort
vars.to.target <- c("wt", "c00100", "e00200", "e00300", "e00650", "p23250")
# vtt <- c("wt", "c00100", "e00200", "e00200p")
# vtt <- c("wt", "c00100", "e00200", "e00200p", "e00600", "e00650")
# vtt <- c("wt", "c00100", "e00200", "e00200p", "e00600", "e00650", "p23250")
# vtt <- c("wt", "c00100", "e00200", "e00200p", "e00600", "e00650", "p23250", "e26270")
vars.to.target

# get the cartesian product of income ranges and variables, as data frame
target.rules.s <- expand.grid(wtvar=vars.to.target, subgroup=agi.ranges, stringsAsFactors = FALSE)
target.rules.s

# add constraint name (a compressed text version of the rules) and constraint number
target.rules <- target.rules.s %>%
  mutate(constraint.name=paste0(wtvar, "_", str_remove_all(subgroup, " ")),
         constraint.shortname=paste0("con_", row_number()))
target.rules


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
  
  missing.indexes <- which(!target.rules$constraint.shortname %in% 
                             unique(constraint.coefficients.sparse.df$constraint.shortname))
  return(missing.indexes)
}


missing.indexes <- find_non_feasible_constraints(synfile, target.rules)
nonmissing.indexes <- setdiff(1:nrow(target.rules), missing.indexes)
target.rules[missing.indexes, ]
target.rules.feasible <- target.rules[nonmissing.indexes, ]

# now get constraint coefficients in the synthetic file
constraint.coefficients.dense <- get_constraint_coefficients_dense(synfile, target.rules.feasible)

# create the full constraint coeff matrix
constraint.shortname.vec <- names(constraint.coefficients.dense)
constraint.shortname.vec

# get the targets - i.e., the constraint righthand side, from the puf
synfile.rhs <- colSums(constraint.coefficients.dense)
constraint.rhs <- get_constraint_coefficients_dense(puf.full, target.rules.feasible) %>% colSums

constraint.coefficients.sparse <- constraint.coefficients.dense %>% 
  mutate(j=row_number()) %>%
  gather(constraint.shortname, value, -j) %>%
  mutate(constraint.shortname=
           factor(constraint.shortname, levels=constraint.shortname.vec), # factor so we can sort in order of appearance in cc.full
         i=match(constraint.shortname, constraint.shortname.vec)) %>%
  filter(value!=0) %>%
  dplyr::select(i, constraint.shortname, j, value) %>%
  arrange(i, j) # SORT ORDER IS CRITICAL
# ht(constraint.coefficients.sparse.df)
length(unique(constraint.coefficients.sparse$constraint.shortname))


#****************************************************************************************************
#                # 4. Compare weighted values on synthetic file to target values ####
#****************************************************************************************************
synfile.vs.targets <- target.rules.feasible %>%
  dplyr::select(constraint.name, constraint.shortname) %>%
  mutate(cnum=row_number(),
         target.value=constraint.rhs / 1e6,
         syn.value=synfile.rhs / 1e6,
         pdiff=syn.value / target.value * 100 - 100)

synfile.vs.targets %>%
  kable(caption="Target and synthetic weighted sums in $ millions, plus syn % diff from target",
        digits=c(0, 0, 0, 1, 1, 3), 
        format.args=list(big.mark = ','))

synfile.vs.targets %>%
  arrange(-abs(pdiff)) %>%
  filter(row_number() <= 25) %>%
  kable(caption="Target and synthetic weighted sums in $ millions, plus syn % diff from target,
        top up-to-25 worst differences",
        digits=c(0, 0, 0, 1, 1, 3), 
        format.args=list(big.mark = ','))

#.. Establish tentative tolerances around targets based on the above
# will need to automate this when we hae a lot of targets
tol.tentative <- rep(.001, nrow(target.rules.feasible))
tol.tentative[c(9, 8, 12)] <- Inf
tol.tentative[c(3, 6, 2, 5)] <- .5
tol.tentative[c(1, 49, 51, 55, 53, 43, 7, 10, 61)] <- .1
tol.tentative


#****************************************************************************************************
#                # 5. Prepare inputs for optimization ####
#****************************************************************************************************

# make the sparseness structure for the constraint coefficients: 
#   a list of vectors, where each vector contains the INDICES of the non-zero elements of one row
# cc.sparse.stru <- make.sparse(cc.full %>% as.matrix) # SLOW
cc.sparse.struc <- make.sparse.struc(constraint.coefficients.dense %>% as.matrix %>% t)
# cc.sparse.struc.nonmissing <- make.sparse.struc.nonempty(constraint.coefficients %>% as.matrix %>% t)

length(cc.sparse.struc) - nrow(target.rules.nonmissing) # must equal number of constraints

# now prepare the inputs for ipopt
inputs <- list()
inputs$p <- 2
inputs$wt <- synfile$wt
inputs$constraint.coefficients.sparse <- constraint.coefficients.sparse
inputs$eval_jac_g_structure <- cc.sparse.struc
inputs$eval_h_structure <- lapply(1:length(inputs$wt), function(x) x) # diagonal elements of our Hessian

#.. Establish tolerances around the targets, if desired ----
# establish constraint lower bounds (clb) and upper bounds (cub) 
# based on tolerances if any around the constraints
# choose tolerances based on our earlier looks at the data
# tol <- rep(.001, length(nonmissing.indexes)) # an alternative

tol <- tol.tentative
clb <- constraint.rhs - abs(constraint.rhs) * tol
cub <- constraint.rhs + abs(constraint.rhs) * tol

# what do the bounds look like vs. targets and data?
synfile.vs.targets %>%
  mutate(clb=clb / 1e6, cub=cub / 1e6) %>%
  kable(caption="Ts",
        digits=c(0, 0, 1, 1, 3, 1, 1, 1), 
        format.args=list(big.mark = ','))

# set starting x values (adjustment factor), plus bounds on the x values
x0 <- rep(1, nrow(synfile))
xlb <- rep(0, nrow(synfile))
xub <- rep(2, nrow(synfile))


#****************************************************************************************************
#                6. Run ipoptr to get optimal x values ####
#****************************************************************************************************
opts <- list("print_level" = 5,
             "file_print_level" = 5, # integer
             "linear_solver" = "ma57", # mumps pardiso ma27 ma57 ma77 ma86 ma97
             "max_iter"=100,
             "output_file" = "syntarget.out")

result <- ipoptr(x0 = x0,
              lb = xlb,
              ub = xub,
              eval_f = eval_f_xtop, 
              eval_grad_f = eval_grad_f_xtop, 
              eval_g = eval_g, 
              eval_jac_g = eval_jac_g,
              eval_jac_g_structure = inputs$eval_jac_g_structure,
              eval_h = eval_h_xtop, # the hessian is essential for this problem
              eval_h_structure = inputs$eval_h_structure,
              constraint_lb = clb,
              constraint_ub = cub,
              opts = opts,
              inputs = inputs)


# take a quick look at the resulting x values
# str(result)
# result$solution
quantile(result$solution, probs=c(0, .01, .05, .10, .25, .5, .75, .9, .95, .99, 1))

tibble(xopt=result$solution) %>%
  ggplot(aes(xopt)) +
  geom_histogram(binwidth=.001, fill="blue") +
  geom_vline(xintercept = 1) +
  scale_x_continuous(breaks=seq(0, 2, .05)) +
  theme(axis.text.x=element_text(size=8, angle=30)) +
  ggtitle("Distribution of x values (ratio of new weight to old weight)")


# 5. Construct a new reweighted file, synfile.rwt, that hits the targets

#****************************************************************************************************
#                7. Construct a new reweighted file, synfile.rwt, that hits the targets ####
#****************************************************************************************************
synfile.rwt <- synfile %>%
  mutate(ftype=paste0(sfname, ".rwt"),
         wt.rawsyn=wt,
         wt=wt.rawsyn * result$solution,
         RECID=nrow(synfile) * 2 + row_number())

# do some quick comparisons
stack <- bind_rows(puf.full, synfile, synfile.rwt)
glimpse(stack)

# make sure RECIDs make sense
stack %>%
  group_by(ftype) %>%
  summarise(RECID.min=min(RECID), RECID.max=max(RECID))

agiranges <- c(-Inf, 0, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, 10e6, Inf)
comps <- stack %>%
  mutate(agirange=cut(c00100, agiranges, right=FALSE), wtvar=1e9) %>%
  group_by(ftype, agirange) %>%
  summarise_at(vars(wtvar, c00100, taxbc, e00200), funs(sum(. * wt) / 1e9))
comps

f <- function(var) {
  comps %>%
    dplyr::select(ftype, agirange, value=var) %>%
    spread(ftype, value) %>%
    janitor:: adorn_totals(where="row") %>%
    mutate(syn.diff=.[[3]] - puf.full,
           rwt.diff=.[[4]] - puf.full,
           syn.pdiff=syn.diff / puf.full * 100,
           rwt.pdiff=rwt.diff / puf.full * 100,
           var=var) %>%
    kable(caption="Comparison of puf, synfile, and reweighted synfile",
          digits=c(rep(1, 8), 0), 
          format.args=list(big.mark = ','))
}
f("wtvar")
f("c00100")
f("taxbc")
f("e00200")


#****************************************************************************************************
#                8. Save all 3 files as a list, and also as csv to synpuf ####
#****************************************************************************************************
glimpse(stack)



