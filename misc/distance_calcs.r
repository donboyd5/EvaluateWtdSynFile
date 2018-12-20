
# distance

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


#****************************************************************************************************
#                How quickly can we compute distances between file a and file b? ####
#****************************************************************************************************
synd <- "D:/Google Drive/synpuf/"

a <- read_csv(paste0(synd, "puf_10p_sample_train.csv"), 
                     col_types = cols(.default= col_double()), 
                     n_max=-1)

b <- read_csv(paste0(synd, "puf_10p_sample_test.csv"), 
              col_types = cols(.default= col_double()), 
              n_max=-1)

identical(names(a), names(b))
names(a)

dvars <- names(a)[c(10:64)]

a2 <- a %>% filter(MARS==1) %>% dplyr::select(dvars) %>% mutate(id=row_number())
b2 <- b %>% filter(MARS==1) %>% dplyr::select(dvars) %>% mutate(id=row_number())
names(b2)
nrow(a2) * nrow(b2) / 1e6 # 31 million

am <- as.matrix(a2 %>% dplyr::select(-id))
bm <- as.matrix(b2 %>% dplyr::select(-id))

d <- dist(am)
str(d)




library("parallelDist")

x <- matrix(rnorm(100), nrow = 5)
x
dist(x)


