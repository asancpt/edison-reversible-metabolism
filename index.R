#!/SYSTEM/R/3.5.1/bin/Rscript
# `simrc` should contain `module load gcc/5.3.0` and `module load R/3.5.1`

if (length(intersect(dir(), 'result')) == 0) { system('mkdir result') }
options(bitmapType='cairo')

# libraries ----

if (Sys.info()['sysname'] == 'Linux') { .libPaths('./lib') }
library(deSolve)
library(wnl)
library(ggplot2)
library(dplyr)
library(readr)
library(purrr)


# Arguments ----

input_deck <- 'number = PK02 ;
'

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) == 0) { arguments <- c("-inp", input_deck, "-file", "TBD.csv") }

table_args <- matrix(arguments, ncol = 2, byrow = TRUE) %>%
  as_tibble() %>%
  mutate(V1 = sub('-', '', V1)) %>%
  spread(V1, V2) %>%
  print()

# nlr(fPK34, 
#     dPK34, 
#     pNames=c("Vc", "Vm", "CLp", "CLm", "CLd1", "CLd2"), 
#     IE=c(15, 3, 0.5, 0.01, 0.003, 0.1), 
#     Error="P")  # different result

# main ----

knitr::knit2html("README.Rmd", "result/report.html", options = c("toc", "mathjax"), force_v1 = TRUE, encoding = 'UTF-8')
system('cp -rf assets figure result')

print(sessionInfo())
print(capabilities())

