


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats
library("scales")

library("btools") # should not need this. If you do, it's at https://github.com/donboyd5/btools

library("knitr")

library("synthpop") # note: masks select in dplyr

#****************************************************************************************************
#                Globals - some may not be needed ####
#****************************************************************************************************
pufdir <- "D:/Dropbox/OSPC - Shared/IRS_pubuse_2011/" # location of puf2011.csv
synd <- "D:/Google Drive/synpuf/"

tc.cli <- "C:/ProgramData/Anaconda3/Scripts/tc" # location of Tax-Calculator command-line interface

# private directory for Tax-Calculator record-level output that we don't want moved from this machine
tc.dir <- "D:/tcdir/"

upper_case_vars <- c("RECID", "MARS", "XTOT", "DSI", "EIC", "FLPDYR", "MIDR") 

eval.dir <- "D:/Dropbox/RPrograms PC/OSPC/EvaluateWtdSynFile/"


#****************************************************************************************************
#                Get puf.names, and puf or other file to be synthesized ####
#****************************************************************************************************
puf.names <- read_csv("./data/pufvars.csv")
puf.names <- puf.names %>% mutate(vname=str_to_upper(vname))
puf.names

puf <- read_csv(paste0(synd, "puf_10p_sample_train.csv"), 
                col_types = cols(.default= col_double()), 
                n_max=-1)

names(puf) %>% sort
count(puf, MARS)

puf.base <- puf %>%
  mutate(E00600=e00600_minus_e00650 + E00650,
         E01500=e01500_minus_e01700 + E01700)


# use this for full puf
# puf <- readRDS(paste0(tc.dir, "puf.rds"))
# puf.base <- puf %>% 
#   filter(!RECID %in% 999996:999999) %>%
#   mutate(ftype="puf.full") %>%
#   dplyr::select(E00100, E09600, one_of(names(syn.comp))) # E09600 is alternative minimum ax

# use this for Max's sample


# identical(names(syn.comp), names(puf.base))
# setdiff(names(syn.comp), names(puf.base))
# setdiff(names(puf.base), names(syn.comp))

#****************************************************************************************************
#                Examine variables ####
#****************************************************************************************************
# what are some big variables to focus on?

# really should do this on the full file to fine out what the big vars are. Var lists below reflect full file.
psum <- puf.base %>% 
  select_if(is.numeric) %>%
  summarise_at(vars(-S006), funs(sum(. * S006 /  100) / 1e9)) %>%
  gather(variable, value) %>%
  left_join(puf.names %>% mutate(variable=str_to_upper(vname)) %>% dplyr::select(variable, vdesc)) %>% 
  arrange(-abs(value))
psum

# size order large to small - absolute value
# "E00100", "E00200", "E01500", "E01700", "E02400", "E02000", "E26270", "E19200", "P23250",
# "E00900", "E18400", "E01400", "E00600", "E18500", "E19800", "E17500", "E00650", "E20400",
# "E00300", "E02300", "E00400", "E20100", "E09600", "E87521", "E00700", "E03270", "E03300",
# "E32800", "E01200", "E87530", "P22250", "E07300", "E62900", "E24515", "E03150", "E03210",
# "E02100", "E03240", "E00800", "E03500", "E09900", "E27200", "E03230", "E24518", "E03290",
# "E07400", "E58990", "E01100", "E11200", "E07260", "E07240", "E03220", "E07600", "E03400"

# E02000 sched E is a problem



#****************************************************************************************************
#                function to synthesize a part of the file, based on a logical variable passed to it ####
#****************************************************************************************************
synpart <- function(puf.subset.rules, visit.sequence, methods, smoothing.rules=NULL, save.name=NULL, test=TRUE){
  
  # puf extract
  pufx <- puf.base %>%
    filter(eval(puf.subset.rules)) %>%
    mutate(divratio=E00650 / E00600,
           divratio=ifelse(is.na(divratio), 0, divratio),
           penratio=E01700 / E01500,
           penratio=ifelse(is.na(penratio), 0, penratio))
  
  # construct variables of interest
  pufx.base <- pufx %>%
    dplyr::select(visit.sequence) # note that RECID is dropped; if we want it in dataset, we should remove it from predictors
  
  seed <- 1234
  if(test) {
      a <- proc.time()
        synx <- syn(pufx.base, visit.sequence=visit.sequence, method=methods, smoothing=smoothing.rules, seed=seed, m=0)
      b <- proc.time()
      print(b - a)      
    } else {
      a <- proc.time()
        synx <- syn(pufx.base, visit.sequence=visit.sequence, method=methods, smoothing=smoothing.rules, seed=seed, m=1)
      b <- proc.time()
      print(b - a)
  }
  return(synx)
}


#****************************************************************************************************
#                Set up visit sequence ####
#****************************************************************************************************
idvars <- c("MARS")
xvars <- c("E00100", "E09600", "S006", "XTOT")

# create a visit sequence based on the variables above -- remove E00100, E09600 add penratio, divratio
vsize1 <- c("E00200", "E01500", "penratio", "E01700", "E02400", "E02000", "E26270", "E19200", "P23250") # E00100 removed, penratio added
vsize2 <- c("E00900", "E18400", "E01400", "E00600", "E18500", "E19800", "E17500", "divratio", "E00650", "E20400")
vsize3 <- c("E00300", "E02300", "E00400", "E20100", "E87521", "E00700", "E03270", "E03300") # "E09600" removed
vsize4 <- c("E32800", "E01200", "E87530", "P22250", "E07300", "E62900", "E24515", "E03150", "E03210")
vsize5 <- c("E02100", "E03240", "E00800", "E03500", "E09900", "E27200", "E03230", "E24518", "E03290")
vsize6 <- c("E07400", "E58990", "E01100", "E11200", "E07260", "E07240", "E03220", "E07600", "E03400")
vsize7 <- c("E09800", "E09700", "P08000")

# create the sequence for synthesis vars
visit.catvars <- c("DSI", "EIC", "F2441", "F6251", "FDED", "MIDR", "N24")
syn.vars.continuous <- c(vsize1, vsize2, vsize3, vsize4, vsize5, vsize6, vsize7)
syn.vars <- c(syn.vars.continuous, visit.catvars)
syn.vars <- c("E02000", setdiff(syn.vars, "E02000")) # move the problem variable to front - works much better!
# syn.vars <- c(setdiff(syn.vars, "E02000"), "E02000") # move the problem variable to back
syn.vars

visit.sequence <- c(idvars, xvars, syn.vars)

# setdiff(names(syn.comp), visit.sequence)


#****************************************************************************************************
#                Set up synthesis methods ####
#****************************************************************************************************
# note variables that must be created before running syn: divratio, penratio
# note that taxcalc requires e00650 (qualified div) <= e00600 (total div)
# I create and use divratio = e00600 / e00650 to ensure this
# similar reason, I create penratio=E01700 / E01500

methods <- rep("cart", length(visit.sequence)) # set default method

# override default method for some variables
keepers <- c(idvars, xvars)
methods[1:length(keepers)] <- "" # these will not be estimated
methods[visit.sequence=="E00650"] <- "~I(E00600 * divratio)"
methods[visit.sequence=="E01700"] <- "~I(E01500 * penratio)"

# problem variables - check density, too?
# methods[visit.sequence=="E03220"] <- "sample" # educator expenses
# methods[visit.sequence=="E07600"] <- "sample" # credit for prior year minimum tax

cbind(visit.sequence, methods)


#****************************************************************************************************
#                Set up smoothing rules ####
#****************************************************************************************************
# smoothing.rules <- list(P23250="density")

smoothing.rules <- rep("", length(syn.vars.continuous))
names(smoothing.rules) <- syn.vars.continuous
smoothing.rules[1:length(smoothing.rules)] <- rep("density", length(smoothing.rules))
smoothing.rules <- as.list(smoothing.rules)
smoothing.rules

#****************************************************************************************************
#                Synthesize ####
#****************************************************************************************************

# create synthpop3 from parts
sub1 <- expression(MARS==1)
sub2 <- expression(MARS==2)
sub34 <- expression(MARS %in% c(3, 4))

smoothing.rules <- NULL

# test
tmp <- synpart(sub1, visit.sequence=visit.sequence, methods=methods, smoothing.rules=smoothing.rules, test=TRUE)
str(tmp)

# now synthesize and save each part
fnames <- c("sampMARS1", "sampMARS2", "sampMARS34")
fnames <- paste0(fnames, ".rsd")
fnames

a <- proc.time()
syn1 <- synpart(sub1, visit.sequence=visit.sequence, methods=methods, smoothing.rules=smoothing.rules, test=FALSE)
saveRDS(syn1, paste0(tc.dir, fnames[1]))

syn2 <- synpart(sub2, visit.sequence=visit.sequence, methods=methods, smoothing.rules=smoothing.rules, test=FALSE)
saveRDS(syn2, paste0(tc.dir, fnames[2]))

syn34 <- synpart(sub34, visit.sequence=visit.sequence, methods=methods, smoothing.rules=smoothing.rules, test=FALSE)
saveRDS(syn34, paste0(tc.dir, fnames[3]))
b <- proc.time()
b - a  # 10% sample is ~ 2 mins

str(syn34)
syn34$smoothing

# create synthpop file ALL AT ONCE
# suball <- expression(1==1)
# synall <- synpart(suball, visit.sequence=visit.sequence, methods=methods, smoothing.rules=NULL, test=FALSE) # 1 hour 12 mins 4038.35
# saveRDS(synall, paste0(tc.dir, "synall.rds"))
# write_csv(synall$syn, paste0(synd, "synthpop2.csv"))
# str(synall)


#****************************************************************************************************
#                IF NEEDED: Prepare full synth file from parts ####
#****************************************************************************************************

syn1.s3 <- readRDS(paste0(tc.dir, fnames[1]))
syn2.s3 <- readRDS(paste0(tc.dir, fnames[2]))
syn34.s3 <- readRDS(paste0(tc.dir, fnames[3]))

synfile <- bind_rows(syn1.s3$syn, syn2.s3$syn, syn34.s3$syn)
saveRDS(synfile, paste0(tc.dir, "synthpop_samp.rds"))
write_csv(synfile, paste0(synd, "synthpop_samp.csv"))
glimpse(synfile)


