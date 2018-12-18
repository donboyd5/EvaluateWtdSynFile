
#****************************************************************************************************
#                file manipulation functions ####
#****************************************************************************************************

get_puf_vnames <- function(){
  # get data frame with PUF variables names and labels (vdesc)
  read_csv("./data/pufvars.csv")
}


get_puf.base <- function(keepnames){
  puf <- readRDS(paste0(tc.dir, "puf.rds"))
  
  puf.base <- puf %>% 
    filter(!RECID %in% 999996:999999) %>%
    mutate(ftype="puf.full") %>%
    dplyr::select(one_of(keepnames))
  return(puf.base)
}


fget <- function(ftype, fvec){
  # read a synthesized file that is in csv form
  # fvec is a named vector of filenames, 
  # ftype is one of the column names of the vector -- i.e., a
  #   short name of the file we want to get
  read_csv(fvec[ftype], 
           col_types = cols(.default= col_double()), 
           n_max=-1) %>%
    mutate(ftype=ftype)
}

get_synfiles <- function(use.files, synfile.fnames, remove.vars=NULL, bad.MARS.files){
  synfiles <- ldply(use.files, fget, synfile.fnames)
  # glimpse(synfiles)
  # count(synfiles, ftype)
  
  synfiles <- synfiles %>%
    dplyr::select(-remove.vars) %>% # temporary -- remove vars that are not in synpufs
    mutate(MARS=ifelse(ftype %in% bad.MARS.files, round(MARS), MARS)) # early files had non-integer MARS
  return(synfiles)
}



change_case <- function(oldnames, upper_case_vars=c("MARS", "XTOT", "DSI", "EIC", "MIDR")){
  # convert variable names to lower case, except for those that should
  # remain upper case
  newnames <- oldnames
  lower_case_indexes <- !newnames %in% upper_case_vars
  lower_case_names <- newnames[lower_case_indexes] %>% str_to_lower
  newnames[lower_case_indexes] <-lower_case_names
  return(newnames)
}


#****************************************************************************************************
#                correlation functions ####
#****************************************************************************************************
cordf <- function(df){
  # get correlations among vars in a df
  # put in long format with 3 columns:
  #   var1  - text variable name
  #   var2  - text variable name
  #   value - correlation between var1 and var2
  # return as a data frame
  cordf <- cor(df, use="pairwise.complete.obs") %>%
    as_tibble(rownames = "var1") %>%
    gather(var2, value, -var1)
  return(cordf)
}


trimcor <- function(df){
  # trim a long correlations data frame (such as produced by cordf)
  # so that it doesn't include "self-correlations" such as x1 with x1, which are always 1
  # and includes only 1 version of each correlation -- e.g., x1-x2 correlation, but not x2-x1 correlation
  # returns data frame with 3 columns:
  #   ftype - the grouping variable that was used
  #   combo - text variable naming the correlation -- e.g., "x1-x2"
  #   value - the correlation
  df2 <- df %>%
    filter(!var1==var2) %>%
    mutate(combo=ifelse(var1 < var2, paste0(var1, "-", var2), paste0(var2, "-", var1))) %>%
    arrange(combo) %>%
    group_by(combo) %>%
    filter(row_number()==1) %>%
    dplyr::select(ftype, combo, value)
  return(df2)
}

#****************************************************************************************************
#                loss measures ####
#****************************************************************************************************
qloss <- function(q, y, fitted){
  # computes qloss for an individual observation, or a
  # vector of qloss for a vector of observations
  # qloss is:
  #  q *e       if e>=0         
  #  (1-q) * e  if e <0
  
  # Examples:  e   q   qloss
  #            3   .1   0.3
  #            3   .9   2.7
  #           -3   .1   2.7
  #           -3   .9   0.3
  e <- y - fitted
  qloss <- pmax(q * e, (q-1) * e)
  return(qloss)
}


#****************************************************************************************************
#                graphing and plotting functions ####
#****************************************************************************************************
cdfplot.unwtd <- function(var){
  # print(var)
  vdesc <- vnames$vdesc[match(var, vnames$vname)]
  df <- stack %>%
    dplyr::select(ftype, wt, value=var) %>%
    group_by(ftype) %>%
    arrange(value) %>%
    mutate(cum.pct=cumsum(value) / sum(value))
  
  if(nrow(df) < 100){
    p <- paste0(var, " has only ", nrow(df), " rows.")
    return(p)
  }
  
  # find a good minimum x-axis value to start the plot on -- based on a desired cumulative percentage
  cum.pct.threshold <- .01
  iminval <- min(which(df$cum.pct[df$ftype=="puf.full"] > cum.pct.threshold))
  minval <- df$value[df$ftype=="puf.full"][iminval]
  # minval
  
  capt <- "- x-axis is log10 scale\n- For display purposes x-axis is truncated at left to start at puf.full's cum.pct=1%"
  gtitle <- paste0("Cumulative distribution of UNweighted ", var, ": ", vdesc)
  gsub <- "Aggregate records excluded"
  ylab <- paste0("Cumulative proportion of the sum of UNweighted ", var)
  
  # define x scale break points and associated labels
  sq10 <- c(0, 1e3, 10e3, 25e3, 50e3, 100e3, 250e3, 500e3, 750e3, 1e6,
            1.5e6, 2e6, 3e6, 4e6, 5e6, 10e6, 25e6, 50e6, 100e6)
  xlabs <- scales::comma(sq10 / 1e3)
  xscale.l10 <- scale_x_log10(name=paste0(var, " in $ thousands"), breaks=sq10, labels=xlabs)
  
  p <- df %>%
    filter(value > minval) %>%
    ggplot(aes(value, cum.pct, colour=ftype)) + 
    geom_line(size=1.5) +
    theme_bw() +
    ggtitle(gtitle, subtitle=gsub) +  labs(caption=capt) +
    scale_y_continuous(name=ylab, breaks=c(seq(0, .9, .05), seq(.92, 1, .02))) +
    xscale.l10 +
    theme(axis.text.x=element_text(angle=45, size=10, hjust=1, colour="black")) +
    theme(plot.caption = element_text(hjust=0, size=rel(.8)))
  return(p)
}


cdfplot <- function(var){
  # print(var)
  vdesc <- synvars$vdesc[match(var, synvars$vname)]
  df <- stack %>%
    dplyr::select(ftype, wt, value=var) %>%
    group_by(ftype) %>%
    arrange(value) %>%
    mutate(cum.pct=cumsum(wt * value) / sum(wt * value))
  
  if(nrow(df) < 100){
    p <- paste0(var, " has only ", nrow(df), " rows.")
    return(p)
  }
  
  # find a good minimum x-axis value to start the plot on -- based on a desired cumulative percentage
  cum.pct.threshold <- .01
  iminval <- min(which(df$cum.pct[df$ftype=="puf.full"] > cum.pct.threshold))
  minval <- df$value[df$ftype=="puf.full"][iminval]
  # minval
  
  capt <- "- x-axis is log10 scale\n- For display purposes x-axis is truncated at left to start at puf.full's cum.pct=1%"
  gtitle <- paste0("Cumulative distribution of weighted ", var, ": ", vdesc)
  gsub <- "Aggregate records excluded"
  ylab <- paste0("Cumulative proportion of the sum of weighted ", var)
  
  # define x scale break points and associated labels
  sq10 <- c(0, 1e3, 10e3, 25e3, 50e3, 100e3, 250e3, 500e3, 750e3, 1e6,
            1.5e6, 2e6, 3e6, 4e6, 5e6, 10e6, 25e6, 50e6, 100e6)
  xlabs <- scales::comma(sq10 / 1e3)
  xscale.l10 <- scale_x_log10(name=paste0(var, " in $ thousands"), breaks=sq10, labels=xlabs)
  
  p <- df %>%
    filter(value > minval) %>%
    ggplot(aes(value, cum.pct, colour=ftype)) + 
    geom_line(size=1.5) +
    theme_bw() +
    ggtitle(gtitle, subtitle=gsub) +  labs(caption=capt) +
    scale_y_continuous(name=ylab, breaks=c(seq(0, .9, .05), seq(.92, 1, .02))) +
    xscale.l10 +
    theme(axis.text.x=element_text(angle=45, size=10, hjust=1, colour="black")) +
    theme(plot.caption = element_text(hjust=0, size=rel(.8)))
  return(p)
}



