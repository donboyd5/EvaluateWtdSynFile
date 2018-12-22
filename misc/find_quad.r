
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

# obj <- function(armean, sd, target){
#   gmean <- r_to_gm.n(armean, sd, 1e9)
#   obj <- (gmean - gm)^2
#   return(obj)
# }
# opt <- optim(gm, obj, sd=sd, target=gm, method="BFGS")
# f(x) = b1 *(x-b2) ^ 3 + c1 * (x - c2)^2

f <- function(b1, b2, c1, c2, x){
  return(b1 *(x - b2)^3 + c1 * (x - c2)^2)
}

# b1	0.5
# b2	1
# c1	5
# c2	1


b1 <- .5
b2 <- 1
c1 <- 5
c2 <- 1

f(b1, b2, c1, c2, 0)
f(b1, b2, c1, c2, 10)

f <- function(v, x){
  b1 <- v[1]
  b2 <- v[2]
  c1 <- v[3]
  c2 <- v[4]
  return(b1 *(x - b2)^3 + c1 * (x - c2)^2)
}

f2 <- function(v, xlo, xhi){
  (f(v, xlo) - f(v, xhi))^2
}

v <- c(.5, 1, 5, 1)
f(v, 0)

xlo <- 0
xhi <- 20
best <-optim(v, f2, gr=NULL, xlo, xhi, method="L-BFGS-B", lower=c(-1, -.4, -Inf, .8), upper=c(Inf, Inf, Inf, 1.2))
best
best$par

v <-  c(-0.010, -1, .23, 1.5)
tibble(x=seq(0.01, 12, .01), y=f(v, x)) %>%
  ggplot(aes(x, y)) + geom_line() + geom_point()

best
f(best$par, xlo)
f(best$par, xhi)
f(best$par, 1)

f3 <- function(v, x){
  b1 <- v[1]
  b2 <- v[2]
  c1 <- v[3]
  c2 <- v[4]
  d <- v[5]
  return(b1 *(x - b2)^3 + c1 * (x - c2)^2 + d / x + .5)
}
v <-  c(-0.0088, -1.8, .215, 1.5, .025)
tibble(x=seq(0.1, 10, .01), y=f3(v, x)) %>%
  ggplot(aes(x, y)) + geom_line() + geom_point()

f3(v, .1)


f4 <- function(v, x){
  a1 <- v[1]
  a2 <- v[2]
  return(a1 * (x - a2)^2)
}
v <- c(1, 5)
tibble(x=seq(0.1, 10, .01), y=f4(v, x)) %>%
  ggplot(aes(x, y)) + geom_line() + geom_point()


v <- c(850, 12)
tibble(x=seq(0, 10000, 1), y=f4(v, x)) %>%
  ggplot(aes(x, y)) + geom_line() + geom_point()



f5 <- function(x){
  y <- 1 /(x-.1) - 20/(x+.1)
  return(y)
}


f5 <- function(v, x){
  b1 <- v[1]
  b2 <- v[2]
  b3 <- v[3]
  b4 <- v[4]
  b5 <- v[5]
  y <- b1 * (x-b2)^2 - b3*(x-b4) + b5/x
  return(y)
}
v <- c(2, .5, 4, 1, 1)
tibble(x=seq(0, 10, .1), y=f5(v, x)) %>%
  ggplot(aes(x, y)) + geom_line() + geom_point()











