
# --reform ref3.json


tc.wincmd <- function(tc.fn, tc.dir, tc.cli, taxyear=2013, reform.fn=NULL, reform.plans.dir=NULL){
  # Build a Windows system command that will call the Tax-Calculator CLI. See:
  #   https://pslmodels.github.io/Tax-Calculator/
  # CAUTION: must use full dir names, not relative to working directory
  # 2013 is the FIRST possible tax year that Tax-Calculator will do
  
  tc.infile.fullpath <- shQuote(paste0(paste0(tc.dir, tc.fn)))
  tc.outdir <- shQuote(str_sub(tc.dir, 1, -1)) # must remove trailing "/"
  
  reformstring <- NULL
  if(!is.null(reform.fn)) reformstring <- paste0("--reform", " ", shQuote(paste0(paste0(reform.plans.dir, reform.fn))))
  
  cmd <- paste0(tc.cli, " ", tc.infile.fullpath, " ", taxyear, " ", reformstring, " ", "--dump --outdir ", tc.outdir)
  return(cmd)
}

# glimpse(synprep$tc.base)

altruns.dir <- paste0(globals$tc.dir, "altruns/")


# write tcbase to a file, because the Tax-Calculator CLI reads a csv file
# maybe use temp file?
tc.fn <- "tcbase.csv"
write_csv(synprep$tc.base, paste0(altruns.dir, tc.fn))

# reform.fullname <- "D:/Dropbox/RPrograms PC/OSPC/EvaluateWtdSynFile/tax_plans/rate_cut.json"
reform.plans.dir <- "D:/Dropbox/RPrograms PC/OSPC/EvaluateWtdSynFile/tax_plans/"
reform.fn <- "rate_cut.json"
reform.fn <- "toprate.json"
reform.fn <- "EITC.json"

cmd <- tc.wincmd(tc.fn=tc.fn, tc.dir=altruns.dir, tc.cli=globals$tc.cli, reform.fn=reform.fn, reform.plans.dir=reform.plans.dir)
cmd    # a good idea to look at the command

a <- proc.time()
system(cmd) # CAUTION: this will overwrite any existing output file that had same input filename!
proc.time() - a # it can easily take 5-10 minutes depending on the size of the input file

# tcbase-13-#-rate_cut-#.csv
tc.outfn <- paste0(str_remove(basename(tc.fn), ".csv"), "-", 13, "-#-", str_remove(basename(reform.fn), ".json"), "-#.csv")
tc.outfn
tc.output <- read_csv(paste0(altruns.dir, tc.outfn), 
                      col_types = cols(.default= col_double()), 
                      n_max=-1)

glimpse(tc.output)
quantile(tc.output$RECID)
saveRDS(tc.output, paste0(altruns.dir, str_remove(basename(reform.fn), ".json"), ".rds"))

