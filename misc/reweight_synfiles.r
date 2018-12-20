# 12/20/2018

# This program reweights a synthetic file so that it will hit targets in a base file, typically the PUF.

# The main things it does are:
# 1. Optionally prepare files if they are not already prepared.
#    - Get a file, which may be the base file (typically PUF) whose targets we want to hit or may be a synfile
#    - Run it through Tax-Calculator to get desired variables such as c00100 and taxbc, and possibly others.
#    - Save the prepared file so that step 1 need not be done in the future on this file
# 2. Get 2 previously prepared files, one of which will serve as the base file (typically PUF) whose targets we want to hit
#    and one of which is the synfile
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


# library("synthpop") # note: masks select in dplyr

# vignette(package = "synthpop")


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
agibrks <- c(-Inf, 0, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, Inf)



#****************************************************************************************************
#                1. Optionally prepare files ####
#****************************************************************************************************


#****************************************************************************************************
#                Functions ####
#****************************************************************************************************

cordf <- function(df){
  # compute correlations and return a data frame with unique combinations
  cordf <- cor(df, use="complete.obs") %>%
    as_tibble(rownames = "var1") %>%
    gather(var2, cor, -var1) %>%
    filter(var1!=var2) %>%
    arrange(var1, var2) %>%
    mutate(combo=ifelse(var1 < var2, paste0(var1, "-", var2), paste0(var2, "-", var1))) %>%
    arrange(combo, var1) %>%
    group_by(combo) %>%
    slice(1) # select first row in each group
  return(cordf)
}


comp2 <- function(var, df){
  # utility function for comparing results
  if(var=="wt2") weight <- rep(1, nrow(df)) else weight <- df$wt2
  newigroup <- c(levels(df$igroup), "TOTAL")
  dfsum <- df %>%
    dplyr::select(file, igroup, variable=!!var) %>%
    mutate(wtdvar=weight * variable,
           igroup=as.character(igroup)) %>%
    group_by(file, igroup) %>%
    summarize(wtdvar=sum(wtdvar))
  dftot <- dfsum %>%
    group_by(file) %>%
    summarise(wtdvar=sum(wtdvar)) %>%
    mutate(igroup="TOTAL")
  dfall <- bind_rows(dfsum, dftot) %>%
    mutate(igroup=factor(igroup, levels=newigroup))
  dfall %>%
    spread(file, wtdvar) %>%
    mutate(pdiff.syn=syn / base * 100 - 100,
           pdiff.rwt=syn.rwt / base * 100 - 100,
           vname=var)
}


#****************************************************************************************************
#                ONETIME: Run taxcalc to create a 2014 file with AGI (c00100) and tax before credit (taxbc) ####
#****************************************************************************************************
# C:\ProgramData\Anaconda3\Scripts
# tc D:/Dropbox/IRS_pubuse_2011/puf.csv 2014 --dump --outdir "D:\Dropbox\RPrograms PC\OSPC\cps_puf_compare\tc_out"

# pp2a gets us the right results - it is compatible with tc 0.20.1

# define puf files
pufd <- "D:/Dropbox/IRS_pubuse_2011/"
ppufd <- paste0(pufd, "Previous PUFs/")
# the older pufs are:
# ppfn1 <- "puf02132018.csv"
# ppfn2 <- "puf03092018.csv"
puffn <- "puf.csv" # this is puf03092018.csv in "Previous PUFs", which is compatible with tc 0.20.1, renamed to puf.csv

puf.dirfname <- shQuote(paste0(ppufd, puffn)) # ; use shQuote because of space in directory name

# build the system command to run taxcalc
cmd.tc <- "C:/ProgramData/Anaconda3/Scripts/tc"
cmd.file <- puf.dirfname
cmd.year <- 2014
cmd.dump <- '--dump --outdir "D:/Dropbox/RPrograms PC/OSPC/syndata/data"'
cmd <- paste0(cmd.tc, " ", cmd.file, " ", cmd.year, " ", cmd.dump)
cmd

# run taxcalc
a <- proc.time()
system(cmd)
proc.time() - a # about 2.5 minutes

# result is puf-14-#-#-#.csv


#****************************************************************************************************
#                ONETIME - Set up puf data ####
#****************************************************************************************************
# get puf.csv so that we have the names of the puf variables as that is what we will want to recreate
puf <- read_csv("D:/Dropbox/IRS_pubuse_2011/Previous PUFs/puf.csv", col_types = cols(.default= col_double()), n_max=1) 
pufnames <- names(puf)
pufnames
saveRDS(pufnames, "./data/pufnames.rds")

# now get 2014 puf with tax calculations from prior run
# ptname <- "D:/Dropbox/RPrograms PC/OSPC/cps_puf_compare/tc_out/puf0309201_renamed-14-#-#-#.csv"
ptname <- "D:/Dropbox/RPrograms PC/OSPC/syndata/data/puf-14-#-#-#.csv"
puftax2014 <- read_csv(ptname, col_types = cols(.default= col_double()))
glimpse(puftax2014)
saveRDS(puftax2014, "./data/puftax2014.rds")


#****************************************************************************************************
#                ONETIME - Create variable description file for puf ####
#****************************************************************************************************
testd <- "D:/Dropbox/Open Projects/OSPC 2018/taxdata-master/tests/"
dfn <- "records_metadata.json"
json_file <- paste0(testd, dfn)

meta <- jsonlite::fromJSON(json_file) # , flatten=TRUE
d <- sapply(meta, "[[", "desc")
vdesc <- tibble(vname=names(d), desc=unname(d))
vdesc
write_csv(vdesc, "./data/vdesc.csv")

saveRDS(vdesc, paste0("./data/vdesc.rds"))


#****************************************************************************************************
#****************************************************************************************************
#                SEPARATOR between onetime tasks and analysis ####
#****************************************************************************************************
#****************************************************************************************************


#****************************************************************************************************
#                Get data ####
#****************************************************************************************************
pufnames <- readRDS("./data/pufnames.rds")
puftax2014 <- readRDS("./data/puftax2014.rds") # this is 2014 output from taxcalc
glimpse(puftax2014)
setdiff(pufnames, names(puftax2014)) # good, all pufnames are in puftax2014
setdiff(names(puftax2014), pufnames) # quite a few additional variables are in puftax2014

# get a streamlined version of the data to work with
puf2014 <- puftax2014 %>% dplyr::select(one_of(c(pufnames, "c00100", "taxbc")))
glimpse(puf2014)
names(puf2014)

vdesc <- readRDS(paste0("./data/vdesc.rds"))
vdesc # description of puf variables


#****************************************************************************************************
#                Make an extract ####
#****************************************************************************************************
count(puf2014, MARS, XTOT)
# MARS 2 XTOT 4 26,669 records -- married, 2 dependents
pufx <- puf2014 %>% filter(MARS==2, XTOT==4)
quantile(pufx$c00100, 0:10/10)

#.. get counts by income range ----

pufx <- pufx %>%
  mutate(igroup=cut(c00100, agibrks, right=FALSE))
count(pufx, igroup) # the <0 group only has 573 obs

#.. get weights by income group ----
pufx %>%
  group_by(igroup) %>%
  do(qtiledf(.$s006, c(0, .1, .25, .5, .75, .9, 1)))
# median weight is small for lowest and highest income groups


# distributions of returns -- update to do weighted kernel density
# gdist <- function(levgrp) {
#   pufx %>%
#     filter(igroup %in% levels(igroup)[levgrp]) %>%
#     ggplot(aes(c00100, colour=igroup)) + geom_density()
# }
# cbind(levels(pufx$igroup))
# # agi group and wages dist
# gdist(1) # neg agi: wages near 0
# gdist(2) # 0-25k a bit bimodal, wages near 0 and 15k
# gdist(3) # 25-50k bimodal 0, 35k
# gdist(4) # 50-75k bi 0, 65k
# gdist(5) # 75-100k, bi 0, 80k
# gdist(6) # 100-200k bi 0 110k
# gdist(7) # 200-500k bi 
# gdist(8) # 500k+


#****************************************************************************************************
#                Identify big puf items - for deciding variables to focus on ####
#****************************************************************************************************
# get a summary to identify variables with the largest value so we know what to focus on 
psum <- pufx %>% 
  select_if(is.numeric) %>%
  summarise_at(vars(-s006, -RECID, -FLPDYR), funs(sum(. * s006) / 1e9)) %>%
  gather(variable, value) %>%
  left_join(vdesc %>% rename(variable=vname)) %>% 
  arrange(-value)
psum


#..psum listing ----
# variable          value desc
# 1 c00100     1569.     NA
# 2 e00200     1232.     Wages, salaries, and tips for filing unit
# 3 e00200p     659.     Wages, salaries, and tips for taxpayer
# 4 e00200s     572.     Wages, salaries, and tips for spouse
# 5 taxbc       259.     NA
# 6 e02000      138.     Sch E total rental, royalty, partnership, S-corporation, etc, income/loss (includes e26270 and e27200)
# 7 e26270      126.     Sch E: Combined partnership and S-corporation net income/loss
# 8 p23250       77.6    Sch D: Net long-term capital gains/losses
# 9 e19200       66.2    Sch A: Interest paid
# 10 e18400       65.2    Sch A: State and local income/sales taxes
# 11 e00900       53.7    Sch C business net profit/loss for filing unit
# 12 e01500       52.5    Total pensions and annuities
# 13 e00900p      38.8    Sch C business net profit/loss for taxpayer
# 14 e18500       36.1    Sch A: Real-estate taxes paid
# 15 e19800       22.7    Sch A: Gifts to charity: cash/check contributions
# 16 e01700       21.1    Taxable pensions and annuities
# 17 e00600       21.0    Ordinary dividends included in AGI
# 18 e20400       17.7    Sch A: Miscellaneous deductions subject to 2% AGI limitation
# 19 e00650       15.8    Qualified dividends included in ordinary dividends
# 20 e00900s      14.9    Sch C business net profit/loss for spouse


#****************************************************************************************************
#                Get correlations to get insight into variable sequencing ####
#****************************************************************************************************
glimpse(pufx)
corvars <- c("s006", "e00200p", "e00200s", "e00200", "p23250", 
             "e00600", "divratio", "e00650", "nonqualdiv", "e26270", "otheragi", "c00100", "wageratio")
pufcors <- pufx %>%
  mutate(divratio=e00650 / e00600,
         divratio=ifelse(is.na(divratio), 0, divratio),
         nonqualdiv=e00600 - e00650,
         wageratio=e00200p / e00200,
         wageratio=ifelse(is.na(wageratio), 0, wageratio),
         otheragi=c00100 - e00200 - p23250 - e26270 - e00600) %>%
  do(cordf(.[, corvars])) %>%
  ungroup %>%
  arrange(-abs(cor))
pufcors

pufcors %>% filter(str_detect(combo, "s006")) # divratio, c00100, e00200, e00200p/s
pufcors %>% filter(str_detect(combo, "c00100")) # p23250, e00200, otheragi, e00200p/s e00600
pufcors %>% filter(str_detect(combo, "divratio")) # s006, e00200, c00100
pufcors %>% filter(str_detect(combo, "wageratio")) # p23250, e00200, otheragi, e00200p/s e00600
pufcors %>% filter(str_detect(combo, "wageratio")) # p23250, e00200, otheragi, e00200p/s e00600


#****************************************************************************************************
#                Create synthetic file ####
#****************************************************************************************************
# it is common to put weights in X, and use them as predictors, although alternatively they can be synthesized
# https://github.com/cran/synthpop/blob/master/R/syn.r
names(pufx) %>% sort
psum # use this to help decide variables of interest

# id vars to include in the results
idvars <- c("MARS", "XTOT", "FLPDYR")
# xvars <- c("s006")
xvars <- NULL

# create a visit sequence for the variables, and a set of methods for the variables
# vseq <- c("s006", "c00100", "e00200p", "e00200s", "e00200", "p23250", "e26270", "e00600", "divratio", "e00650", "otheragi")
# vseq <- c("e00200p", "e00200s", "e00200", "p23250", "e00600", "divratio", "e00650", "e26270", "otheragi", "c00100")
# vseq <- c("s006",
#           "c00100", 
#           "e00200", "wageratio", "e00200p", "e00200s",
#           "e00600", "divratio", "e00650", 
#           "p23250", 
#           "e26270", "otheragi")

# vseq <- c("c00100",
#           "e00200", "wageratio", "e00200p", "e00200s",
#           "nonqualdiv", "e00650", "e00600",
#           "p23250",
#           "e26270", "otheragi")

vseq <- c("s006",
          "c00100",
          "e00200", "wageratio", "e00200p", "e00200s",
          "e00600", "divratio", "e00650",
          "p23250",
          "e26270", "otheragi")

# note variables that must be created before running syn: divratio, otheragi
# note that taxcalc requires e00650 (qualified div) <= e00600 (total div)
# I create and use divratio = e00600 / e00650 to ensure this
# I also create otheragi


voi <- c(idvars, xvars, vseq) # variables of interest
methods <- rep("cart", length(voi)) # set default method

# override default method for some variables
lhs <- c(idvars, xvars)
methods[1:length(lhs)] <- "" # these will not be estimated

methods[voi=="s006"] <- "sample"

methods[voi=="e00200p"] <- "~I(e00200 * wageratio)"
methods[voi=="e00200s"] <- "~I(e00200 - e00200p)"

methods[voi=="e00650"] <- "~I(e00600 * divratio)"

otheragi.method <- "~I(c00100 - e00200 - p23250 - e26270 - e00600)"
methods[voi=="otheragi"] <- otheragi.method

cbind(voi, methods)


# get a file extract that only has the variables of interest that we are going to synthesize
# with var names in the order we want them in
pufx.base <- pufx %>%
  mutate(divratio=e00650 / e00600,
         divratio=ifelse(is.na(divratio), 0, divratio),
         nonqualdiv=e00600 - e00650,
         wageratio=e00200p / e00200,
         wageratio=ifelse(is.na(wageratio), 0, wageratio),
         otheragi=c00100 - e00200 - p23250 - e26270 - e00600) %>%
  dplyr::select(voi) # note that RECID is dropped; if we want it in dataset, we should remove it from predictors
glimpse(pufx.base)
# summary(pufx.base)

pufx.base %>%
  dplyr::select(vseq) %>%
  gather(variable, value) %>%
  group_by(variable) %>%
  do(qtiledf(.$value))

pufx.base %>%
  dplyr::select(vseq) %>%
  gather(variable, value) %>%
  group_by(variable) %>%
  summarise(n=n(), nlt0=sum(value<0), nzero=sum(value==0), ngt0=sum(value>0))

# we must define methods for all vars in the data, in varname order
# methods <- rep("", ncol(pufx.base)) # default
# idx <- match(vseq, names(pufx.base)) # the indexes within methods corresponding to vseq
# methods[idx] <- methods.vseq
# cbind(names(pufx.base), methods)

seed <- 1234
syn(pufx.base, visit.sequence=voi, method=methods, seed = seed, m=0)
# syn(pufx.base, visit.sequence=voi, method=methods, seed = seed, m=0, cont.na=list(divratio = c(0)))
system.time(synx <- syn(pufx.base, visit.sequence=voi, method=methods, seed = seed, m=1))
synx

# compare(synx, pufx.base, breaks=60)
# compare(synx, pufx.base, vars="c00100", breaks=40)
# compare(synx, pufx.base, vars="e00600", breaks=100)


saveRDS(synx, "./data/synx.rds")


#****************************************************************************************************
#                Create a combined file ####
#****************************************************************************************************

synxdf <- synx$syn
# create a combined file
names(pufx.base)
names(synxdf)
all.equal(names(pufx.base), names(synxdf))

combo.in <- bind_rows(pufx.base %>% mutate(file="base"), 
                      synxdf %>% mutate(file="syn")) %>%
  mutate(RECID=row_number()) # create our own RECID - NOT same as what was on original file

minmax <- combo.in %>%
  group_by(file) %>%
  summarise(RECID.min=min(RECID), RECID.max=max(RECID))
minmax


#****************************************************************************************************
#                Compare base file to syn file for variables that do not need to be calculated ####
#****************************************************************************************************
glimpse(combo.in)
names(combo.in)

#..correlations ----
cors <- combo.in %>%
  group_by(file) %>%
  do(cordf(.[, vseq])) %>%
  ungroup %>%
  arrange(combo, file)

cors %>% 
  dplyr::select(combo, file, cor) %>%
  spread(file, cor) %>%
  mutate(diff=syn - base) %>%
  filter(str_detect(combo, "c00100")) %>%
  arrange(desc(abs(diff))) %>%
  kable(digits=3)

cors %>% 
  dplyr::select(combo, file, cor) %>%
  spread(file, cor) %>%
  mutate(diff=syn - base) %>%
  arrange(desc(abs(diff))) %>%
  kable(digits=3)

#..aggregates ----
sumval <- function(compvar, wtvar=1){
  sumval <- sum(wtvar * compvar)
  return(sumval)
}

# define comparison variables
compvars <- c("one", setdiff(vseq, "s006")) # "one" must be created below
compvars
combo.in %>%
  mutate(one=1) %>%
  group_by(file) %>%
  summarise_at(vars(compvars), funs(sumval(., s006))) %>%
  gather(variable, value, -file) %>%
  spread(file, value) %>%
  mutate(diff=syn - base,
         pdiff= diff / base * 100) %>%
  mutate(variable=factor(variable, levels=compvars)) %>% # put vars in desired order
  arrange(variable)

saveRDS(combo.in, "./data/combo.in.rds")


#****************************************************************************************************
#                Set up problem for reweighting ####
#****************************************************************************************************

library("ipoptr")
source("./r/includes/functions_ipopt.r")
source("./r/includes/functions_target_setup_and_analysis(1).r")

combo.in <- readRDS("./data/combo.in.rds")
glimpse(combo.in)

# agibrks
combo2 <- combo.in %>%
  mutate(igroup=cut(c00100, agibrks),
         wt=s006)
glimpse(combo2)

combo2 %>%
  group_by(file, igroup) %>%
  summarise(n=n(), wtdn=sum(s006), c00100m=sum(c00100 * s006) / 1e6) %>%
  gather(variable, value, -file, -igroup) %>%
  unite(varfile, variable, file) %>%
  spread(varfile, value) %>%
  mutate(pc00100m=c00100m_syn / c00100m_base * 100 - 100,
         pn=n_syn / n_base * 100 - 100,
         pwtdn=wtdn_syn / wtdn_base * 100 - 100)

target <- combo2 %>%
  filter(file=="base")
newf <- combo2 %>%
  filter(file=="syn")

glimpse(target)
glimpse(newf)

sum(newf$wt) / sum(target$wt)
ilevs <- levels(newf$igroup)
ilevs
# target %>%
#   group_by(igroup) %>%
#   summarise(wt.sum=sum(wt))

newf %>%
  group_by(igroup) %>%
  summarise(wt.sum=sum(wt))
# glimpse(newf)


#..Define targets (constraints) ----
iranges <- c(
  "c00100 <= 0", 
  "c00100 > 0 & c00100 <= 25e3",
  "c00100 > 25e3 & c00100 <= 50e3",
  "c00100 > 50e3 & c00100 <= 75e3",
  "c00100 > 75e3 & c00100 <= 100e3",
  "c00100 > 100e3 & c00100 <= 200e3",
  "c00100 > 200e3 & c00100 <= 500e3",
  "c00100 > 500e3 & c00100 <= 1e12")
iranges

vtt <- c("s006", "c00100", "e00200")
# vtt <- c("s006", "c00100", "e00200", "e00200p")
# vtt <- c("s006", "c00100", "e00200", "e00200p", "e00600", "e00650")
# vtt <- c("s006", "c00100", "e00200", "e00200p", "e00600", "e00650", "p23250")
# vtt <- c("s006", "c00100", "e00200", "e00200p", "e00600", "e00650", "p23250", "e26270")
vtt

target.rules.s <- expand.grid(wtvar=vtt, subgroup=iranges, stringsAsFactors = FALSE)
target.rules.s

target.rules <- target.rules.s %>%
  mutate(cname=paste0(wtvar, "_", str_remove_all(subgroup, " ")),
         cnum=paste0("con_", row_number()))
target.rules

rules <- parse(text=target.rules$subgroup)
rules

# implement the rules


cc.full <- getcc.full(newf, target.rules, wtvar="s006")
colSums(cc.full)

con.rhs <- getcc.full(target, target.rules) %>% colSums
colSums(cc.full) / con.rhs

# create the full constraint coeff matrix
cnames <- names(cc.full)
cnames # constraint names

# get the constraint values as the df ccmat.df
# inputs$ccmat.df has the fixed constraint coefficients, in sparse matrix form, in a dataframe that has:
#   i -- the constraint number
#   cname -- the constraint name
#   j -- the variable number (an index into the vector x)
#   value
ccmat.df <- cc.full %>% 
  mutate(j=row_number()) %>%
  gather(cname, value, -j) %>%
  mutate(cname=factor(cname, levels=cnames), # factor so we can sort in order of appearance in cc.full
         i=match(cname, cnames)) %>%
  filter(value!=0) %>% 
  dplyr::select(i, cname, j, value) %>%
  arrange(i, j) # SORT ORDER IS CRITICAL
ht(ccmat.df)

# make the sparseness structure for the constraint coefficients: 
#   a list of vectors, where each vector contains the INDICES of the non-zero elements of one row
# cc.sparse.stru <- make.sparse(cc.full %>% as.matrix) # SLOW
cc.sparse.struc <- make.sparse.struc(cc.full %>% as.matrix %>% t)
length(cc.sparse.struc) # must equal number of constraints


#****************************************************************************************************
#                Run ipoptr ####
#****************************************************************************************************

inputs <- list()
inputs$p <- 2
inputs$wt <- newf$wt
inputs$ccmat.df <- ccmat.df
inputs$eval_jac_g_structure <- cc.sparse.struc
inputs$eval_h_structure <- lapply(1:length(inputs$wt), function(x) x) # diagonal elements of our Hessian

# get the constraint rhs
con.rhs <- getcc.full(target, target.rules) %>% colSums
con.rhs
start.rhs <- colSums(cc.full)
tol <- .001
clb <- con.rhs - abs(con.rhs) * tol
cub <- con.rhs + abs(con.rhs) * tol
startvals <- cbind(start.rhs, clb, con.rhs, cub, ratio=con.rhs / start.rhs * 100) %>% kable(digits=2)
startvals
# check <- c(32, 39)
# check <- 3
# target.rules[check, ]

x0 <- rep(1, nrow(newf))
xlb <- rep(0, nrow(newf))
xub <- rep(2, nrow(newf))

#..run Ipopt ----
opts <- list("print_level" = 5,
             "file_print_level" = 5, # integer
             "linear_solver" = "ma57", # mumps pardiso ma27 ma57 ma77 ma86 ma97
             "max_iter"=100,
             "output_file" = "syntarget.out")

res <- ipoptr(x0 = x0,
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
# str(res)
# res$solution
quantile(res$solution, 0:10/10)

tibble(xopt=res$solution) %>%
  ggplot(aes(xopt)) +
  geom_histogram(binwidth=.001, fill="blue") +
  geom_vline(xintercept = 1) +
  scale_x_continuous(breaks=seq(0, 2, .05)) +
  theme(axis.text.x=element_text(size=8, angle=30)) +
  ggtitle("Distribution of x values (ratio of new weight to old weight)")

# newf %>% mutate(x=res$solution, wt=wt*res$solution) %>% filter(x>1.4)


#****************************************************************************************************
#                Compare files ####
#****************************************************************************************************
glimpse(combo.in)
count(combo.in, file)

combo.out <- bind_rows(combo2 %>% mutate(x=1),
                       combo2 %>% filter(file=="syn") %>% mutate(x=res$solution, file="syn.rwt")) %>%
  mutate(wt2=s006 * x,
         RECID=row_number())
glimpse(combo.out)

combo.out %>%
  group_by(file, igroup) %>%
  summarise(n.wtd=sum(wt2), c00100m.wtd=sum(wt2 * c00100) / 1e6) %>%
  gather(variable, value, -file, -igroup) %>%
  unite(varfile, variable, file) %>%
  spread(varfile, value)


comp2("wt2", combo.out)
comp2("c00100", combo.out)
comp2("e00200", combo.out)
comp2("e00200p", combo.out)
comp2("e00200s", combo.out)
comp2("e00600", combo.out)
comp2("e00650", combo.out)
comp2("p23250", combo.out)
comp2("e26270", combo.out)


#****************************************************************************************************
#                Save combined files ####
#****************************************************************************************************
glimpse(combo.out)
saveRDS(combo.out, "./data/combo.out.rds")

# create a file suitable for tax calc
combo.out %>%
  group_by(file) %>%
  summarise(n=n(), RECID.min=min(RECID), recid.max=max(RECID))

pufnames
puf.combo <- combo.out %>%
  dplyr::select(one_of(pufnames))
glimpse(puf.combo)
write_csv(puf.combo, "./data/puf.combo.csv")


#****************************************************************************************************
#                Run taxcalc on the combined file to get AGI (c00100) and tax before credit (taxbc) ####
#****************************************************************************************************
# from Windows system command prompt (prob administrator): conda update -c PSLmodels taxcalc
dirfn <- shQuote("D:/Dropbox/RPrograms PC/OSPC/syndata/data/puf.combo.csv")
dirfn

# build the system command to run taxcalc
cmd.tc <- "C:/ProgramData/Anaconda3/Scripts/tc"
cmd.file <- dirfn
cmd.year <- 2014
# cmd.dump <- '--dump --outdir "D:/Dropbox/RPrograms PC/OSPC/syndata/data"'
cmd.dump <- '--dump --outdir "D:/Dropbox/RPrograms PC/OSPC/syndata/data/max"'
cmd <- paste0(cmd.tc, " ", cmd.file, " ", cmd.year, " ", cmd.dump)
cmd

# run taxcalc
a <- proc.time()
system(cmd)
proc.time() - a # 61 secs


#****************************************************************************************************
#                Compare taxbc for the three files ####
#****************************************************************************************************
combotax <- read_csv("./data/puf.combo-14-#-#-#.csv", col_types = cols(.default= col_double()))
glimpse(combotax)

combo3 <- combo.out %>%
  mutate(taxbc=combotax$taxbc)

comp2("taxbc", combo3)









