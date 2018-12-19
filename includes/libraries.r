
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats
library("scales")

library("btools") # You should not need this. If you do, it's at https://github.com/donboyd5/btools

library("knitr")

library("janitor") # for adorn_totals
