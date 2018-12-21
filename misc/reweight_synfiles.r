# 12/20/2018

# This program reweights a synthetic file so that it will hit targets in a base file, typically the PUF.

# CAUTION: Currently this is only set to work on the full PUF and a synfile of the full PUF

# Steps in this program ----
# The main things it does are:
# 1. ONETIME PER SYNFILE - Optionally prepare a file and its puf counterpart, if not already prepared.
#    - Get a synfile, stack it with PUF using only the variables that are in both files
#    - Run it through Tax-Calculator so that we can get desired calculated variables
#    - Save the prepared file and Tax-Calculator output as a list so that step 1 need not be done in the future on this synfile
# 2. Get previously-prepared synfile-PUF and tax output, merge, and separate PUF and synfile
# 3. Define and construct a set of targets (weighted values) from the base file
# 4. Compare weighted values on synthetic file to target values
# 5. Prepare inputs for optimization
# 6. Run ipoptr to get optimal x values
# 7. Construct a new reweighted file, synfile.rwt, that hits the targets
#    - Obtain adjustment factors for synfile weights that minimize a distortion function based on size of the adjustment,
#      while satisfying constraints that ensure that the targets are hit (or that results are within defined tolerances)
#    - Construct synfile.rwt by adjusting the wt variable (and save the raw synthesized weight as wt.rawsyn)
# 8. Save all 3 files as a list, and also as csv to synpuf

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
#  1. ONETIME PER SYNFILE - Optionally prepare a file and its PUF counterpart ####
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


#******************************************************************************************************************
#  2. Get previously-prepared synfile-PUF and tax output, merge, and separate PUF and synfile ####
#******************************************************************************************************************
sfname <- "synthpop3"
synprep <- readRDS(paste0(globals$tc.dir, sfname, "_rwprep.rds"))

# merge and then split
tcvars <- c("c00100", "taxbc")
mrgdf <- left_join(synprep$tc.base, synprep$tc.output %>% dplyr::select(RECID, tcvars))
glimpse(mrgdf)

puf.full <- mrgdf %>% filter(ftype=="puf.full")
synfile <- mrgdf %>% filter(ftype==sfname)


#******************************************************************************************************************
#  3. Define and construct a set of targets (weighted values) from the base file ####
#******************************************************************************************************************
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
  "c00100 > 10e6")
agi.ranges <- parens(agi.ranges)
agi.ranges

mars.groups <- c("MARS==1", "MARS==2", "MARS %in% c(3, 4)")
mars.groups <- parens(mars.groups)
mars.groups

# cross these two groups
agi.mars.cross <- expand.grid(a=agi.ranges, b=mars.groups, stringsAsFactors = FALSE) %>%
  mutate(amcross=paste0(a, " & ", b),
         amcross=parens(amcross)) %>%
  .[["amcross"]]
agi.mars.cross   


names(synfile) %>% sort
vars.to.target <- c("wt", "c00100", "e00200", "e00300", "e00650", "e00700", "e00900",
                    "e01100", "e01200", "e01400", "e01700", "e02400",
                    "p23250", "taxbc")
vars.to.target

# define target rules in several steps or optionally use the crossed values
# a) define targets for variables by income range, getting cartesian product
agi.target.rules <- expand.grid(wtvar=vars.to.target, subgroup=agi.ranges, stringsAsFactors = FALSE)
agi.target.rules

# b) define targets for variables by income range, getting cartesian product
mars.target.rules <- expand.grid(wtvar=vars.to.target, subgroup=mars.groups, stringsAsFactors = FALSE)
mars.target.rules

# concatenate the target rules
target.rules <- bind_rows(agi.target.rules, mars.target.rules)
target.rules

# alternatively use the crossed values
target.rules <- expand.grid(wtvar=vars.to.target, subgroup=agi.mars.cross, stringsAsFactors = FALSE)

# add constraint name (a compressed text version of the rules) and constraint number
target.rules <- add_rulenames(target.rules)
target.rules

# some targets may be nonfeasible - we need to remove them
feasability.list <- find_non_feasible_constraints(synfile, target.rules) # make this faster
names(feasability.list)

length(feasability.list$nonfeasible.indexes)
target.rules[feasability.list$nonfeasible.indexes, ]

target.rules.feasible <- feasability.list$target.rules.feasible

# now get constraint coefficients in the synthetic file
constraint.coefficients.dense <- get_constraint_coefficients_dense(synfile, target.rules.feasible)
constraint.coefficients.sparse <- get_constraint_coefficients_sparse_from_dense(constraint.coefficients.dense)

# get the targets - i.e., the constraint righthand side, from the puf
synfile.rhs <- colSums(constraint.coefficients.dense)
constraint.rhs <- get_constraint_coefficients_dense(puf.full, target.rules.feasible) %>% colSums


#******************************************************************************************************************
#  4. Compare weighted values on synthetic file to target values ####
#******************************************************************************************************************
synfile.vs.targets <- target.rules.feasible %>%
  dplyr::select(constraint.name, constraint.shortname) %>%
  mutate(cnum=row_number(),
         target.value=constraint.rhs / 1e6,
         syn.value=synfile.rhs / 1e6,
         pdiff=syn.value / target.value * 100 - 100,
         apdiff=abs(pdiff))

synfile.vs.targets %>%
  kable(caption="Target and synthetic weighted sums in $ millions, plus syn % diff from target",
        digits=c(0, 0, 0, 1, 1, 3, 1), 
        format.args=list(big.mark = ','))

synfile.vs.targets %>%
  arrange(-apdiff) %>%
  filter(row_number() <= 25) %>%
  kable(caption="Target and synthetic weighted sums in $ millions, plus syn % diff from target,
        top up-to-25 worst differences",
        digits=c(0, 0, 0, 1, 1, 3, 1), 
        format.args=list(big.mark = ','))

#..Automate the setting of tolerances around constraints based upon rules ----
# These can be overriden if desired
synfile.vs.targets <- synfile.vs.targets %>%
  mutate(tol.default=case_when(apdiff >= 200 ~ Inf,
                               apdiff >= 100 & apdiff < 200  ~ .8,
                               apdiff >= 50 & apdiff < 100 ~ .25,
                               apdiff >= 25 & apdiff < 50 ~ .10,
                               apdiff >= 10 & apdiff < 25 ~ .02,
                               TRUE ~ .001),
         tol=tol.default)
synfile.vs.targets %>% arrange(-apdiff)
count(synfile.vs.targets, tol.default)

# override the tolerance defaults based upon possibly subequent analysis of violations
synfile.vs.targets$tol[c(110, 378, 392, 406)] <- .75



#******************************************************************************************************************
#  5. Prepare inputs for optimization ####
#******************************************************************************************************************

# make the sparseness structure for the constraint coefficients: 
#   a list of vectors, where each vector contains the INDICES of the non-zero elements of one row
# cc.sparse.stru <- make.sparse(cc.full %>% as.matrix) # SLOW
cc.sparse.structure <- make.sparse.structure(constraint.coefficients.dense %>% as.matrix %>% t)
length(cc.sparse.structure) - nrow(target.rules.feasible) # must equal number of constraints

# now prepare the inputs for ipopt
inputs <- list()
inputs$p <- 2
inputs$wt <- synfile$wt
inputs$constraint.coefficients.sparse <- constraint.coefficients.sparse
inputs$eval_jac_g_structure <- cc.sparse.structure
inputs$eval_h_structure <- lapply(1:length(inputs$wt), function(x) x) # diagonal elements of our Hessian

#.. Establish tolerances around the targets, if desired ----
# establish constraint lower bounds (clb) and upper bounds (cub) 
# based on tolerances if any around the constraints
# choose tolerances based on our earlier looks at the data
# tol <- rep(.001, length(nonmissing.indexes)) # an alternative

tol <- synfile.vs.targets$tol
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
xlb <- rep(0.1, nrow(synfile))
xub <- rep(3, nrow(synfile))


#******************************************************************************************************************
#  6. Run ipoptr to get optimal x values ####
#******************************************************************************************************************
opts <- list("print_level" = 5,
             "file_print_level" = 5, # integer
             "linear_solver" = "ma57", # mumps pardiso ma27 ma57 ma77 ma86 ma97
             "max_iter"=500,
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

names(result)

# IF the result is infeasible, compare constraints to bounds ----
check <- tibble(cnum=synfile.vs.targets$cnum,
                     name=synfile.vs.targets$constraint.name,
                     target=constraint.rhs,
                     lb=clb,
                     result=result$constraints,
                     ub=cub) %>%
  mutate(violation=case_when(result < lb ~ lb - result,
                             result > ub ~ ub - result,
                             TRUE ~ 0),
         vpct=violation / target * 100)
violations <-  check %>%
  filter(violation!=0)
violations %>% left_join(synfile.vs.targets %>% select(name=constraint.name, pdiff, tol.default, tol))


# If optimal take a quick look at the resulting x values ----
# str(result)
# result$solution
quantile(result$solution, probs=c(0, .01, .05, .10, .25, .5, .75, .9, .95, .99, 1))

tibble(xopt=result$solution) %>%
  ggplot(aes(xopt)) +
  geom_histogram(binwidth=.001, fill="blue") +
  geom_vline(xintercept = 1) +
  scale_x_continuous(breaks=seq(0, 20, .05)) +
  theme(axis.text.x=element_text(size=8, angle=30)) +
  ggtitle("Distribution of x values (ratio of new weight to old weight)")


#******************************************************************************************************************
#  7. Construct a new reweighted file, synfile.rwt, that hits the targets ####
#******************************************************************************************************************
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
  summarise_at(vars(wtvar, c00100, taxbc, e00200, e01700), funs(sum(. * wt) / 1e9))
comps

f.agi <- function(var) {
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
f.agi("wtvar")
f.agi("c00100")
f.agi("e00200")
f.agi("taxbc")


comps.m <- stack %>%
  mutate(mstat=factor(MARS, levels=c(1:4), labels=c("single", "married", "other", "other")),
         wtvar=1e9) %>%
  group_by(ftype, mstat) %>%
  summarise_at(vars(wtvar, c00100, taxbc, e00200, e01700), funs(sum(. * wt) / 1e9))
comps.m

f.mstat <- function(var) {
  comps.m %>%
    dplyr::select(ftype, mstat, value=var) %>%
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
f.mstat("wtvar")
f.mstat("c00100")
f.mstat("e00200")
f.mstat("taxbc")


comps.agim <- stack %>%
  mutate(agirange=cut(c00100, agiranges, right=FALSE),
         mstat=factor(MARS, levels=c(1:4), labels=c("single", "married", "other", "other")),
         wtvar=1e9) %>%
  group_by(ftype, agirange, mstat) %>%
  summarise_at(vars(wtvar, c00100, taxbc, e00200, e01700), funs(sum(. * wt) / 1e9)) %>%
  ungroup
comps.agim

f.agim <- function(var, mstat.use) {
  comps.agim %>%
    dplyr::select(ftype, agirange, mstat, value=var) %>%
    filter(mstat==mstat.use) %>%
    spread(ftype, value) %>%
    janitor:: adorn_totals(where="row") %>%
    mutate(syn.diff=.[[4]] - puf.full,
           rwt.diff=.[[5]] - puf.full,
           syn.pdiff=syn.diff / puf.full * 100,
           rwt.pdiff=rwt.diff / puf.full * 100,
           var=var) %>%
    kable(caption="Comparison of puf, synfile, and reweighted synfile",
          digits=c(0, 0, rep(1, 7), 0), 
          format.args=list(big.mark = ','))
}
f.agim("wtvar", "single")
f.agim("wtvar", "married")
f.agim("wtvar", "other")

f.agim("c00100", "single")
f.agim("c00100", "married")
f.agim("c00100", "other")

f.agim("e00200", "single")
f.agim("e00200", "married")
f.agim("e00200", "other")

f.agim("taxbc", "single")
f.agim("taxbc", "married")
f.agim("taxbc", "other")

glimpse(target.rules.feasible)
target.rules.feasible$subgroup

target.rules.feasible %>% filter(subgroup=="((c00100 > 500e3 & c00100 <= 1e6) & (MARS==1))")
synfile.vs.targets[103, ]
check[103, ]

#******************************************************************************************************************
#  8. Save all 3 files as a list, and also as csv to synpuf ####
#******************************************************************************************************************
glimpse(stack)
count(stack, ftype)
saveRDS(stack, paste0(globals$tc.dir, sfname, "_reweighted_stackedfiles.rds"))

fnout <- function(fileid) paste0(globals$synd, sfname, "_", fileid, ".csv")
fnout("puf")
write_csv(stack %>% filter(ftype=="puf.full"), fnout("puf"))
write_csv(stack %>% filter(ftype==sfname), fnout("syn"))
write_csv(stack %>% filter(ftype==paste0(sfname, ".rwt")), fnout("rwt"))



