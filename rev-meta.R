# setup ----

library(deSolve)
library(wnl)
library(ggplot2)
library(dplyr)
library(readr)
library(purrr)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

raw_dPK34 <- read_csv('data-raw/reversible-metabolism.csv', 
                      skip = 2,
                      col_names = c("TIME", "MOL", "DV", "ID")) %>% 
  print() # Unit: min   NA    umol/L    NA

# REQUIREMENT: dPK34

dPK34 <- raw_dPK34 %>% as.data.frame()

ggplot(data = raw_dPK34, 
       aes(x = TIME, y = DV, group = interaction(ID, MOL), 
           color = MOL)) +
  geom_line() +
  geom_point() + 
  theme_bw() +
  facet_wrap(. ~ ID) +
  labs(color = 'Molecules', shape = 'Infusion', 
       x = 'Time (min)', y = 'Concentration (umol/L)')

# main ----

S1 <- data.frame(TIME=c(0, 60), RATE1=c(2.375,0), RATE2=c(0.065, 0))
S2 <- data.frame(TIME=c(0,5), RATE1=c(0, 0), RATE2=c(2, 0))
infusion_history <- list(S1, S2)

PKde <- function(t, y, p)
{
  cInf = infusion_history[[cID]]
  Rate1 = cInf[findInterval(t, cInf[,"TIME"]),"RATE1"]
  Rate2 = cInf[findInterval(t, cInf[,"TIME"]),"RATE2"]
  
  Ke1 = p["CLp"]/p["Vc"]
  Ke2 = p["CLm"]/p["Vm"]
  
  K12 = p["CLd1"]/p["Vc"]
  K21 = p["CLd2"]/p["Vm"]
  
  dy1dt = Rate1/p["Vc"] - Ke1*y[1] - K12*y[1] + K21*y[2]
  dy2dt = Rate2/p["Vm"] - Ke2*y[2] + K12*y[1] - K21*y[2]
  
  return(list(c(dy1dt, dy2dt)))  
}

TIME <- c(0, dPK34$TIME) %>% unique() %>% sort()

iTime1 = TIME %in% dPK34[dPK34$ID==1 & dPK34$MOL == "Cp", "TIME"] ; iTime1 ; TIME[iTime1]
iTime2 = TIME %in% dPK34[dPK34$ID==1 & dPK34$MOL == "Cm", "TIME"] ; iTime2 ; TIME[iTime2]
iTime3 = TIME %in% dPK34[dPK34$ID==2 & dPK34$MOL == "Cp", "TIME"] ; iTime3 ; TIME[iTime3]
iTime4 = TIME %in% dPK34[dPK34$ID==2 & dPK34$MOL == "Cm", "TIME"] ; iTime4 ; TIME[iTime4]

## Figure 34.1, p 639
plot(0, 0, type="n", xlim=c(0, 180), ylim=c(0, 6), xlab="Time (min)", ylab="Concentration (uM)")

IDs = unique(dPK34[,"ID"])
nID = length(IDs)

dPK34


iTime1

fit_data <- deSolve::lsoda(y=c(0, 0), 
                           times=TIME, 
                           func=PKde, 
                           parms=c(Vc=14.1169, Vm=2.96671, 
                                   CLp=0.445693, CLm=0.00833429, 
                                   CLd1=0.00308422, CLd2=0.0632217))

as_tibble(fit_data) %>% 
  tidyr::gather(scenario, value, 2:3) %>% 
  ggplot(aes(time, value, color = scenario)) +
  geom_line() +
  geom_point()

y = vector()
for (i in 1:nID) {
  cID <<- IDs[i] # referencing wihtin PKde
  cy = lsoda(y=c(0, 0), times=TIME, func=PKde, parms=c(Vc=14.1169, Vm=2.96671, CLp=0.445693, CLm=0.00833429, CLd1=0.00308422, CLd2=0.0632217))
  iTime1 = TIME %in% dPK34[dPK34$ID==cID & dPK34$MOL == "Cp", "TIME"]
  iTime2 = TIME %in% dPK34[dPK34$ID==cID & dPK34$MOL == "Cm", "TIME"]
  points(dPK34[dPK34$ID==cID & dPK34$MOL == "Cp", "TIME"], dPK34[dPK34$ID==cID & dPK34$MOL == "Cp", "DV"], pch=19, col=i)
  points(dPK34[dPK34$ID==cID & dPK34$MOL == "Cm", "TIME"], dPK34[dPK34$ID==cID & dPK34$MOL == "Cm", "DV"], pch=15, col=i)
  lines(TIME, cy[,"1"], col=i)
  lines(TIME, cy[,"2"], lty=2, col=i)
  y = c(y, cy[iTime1,"1"], cy[iTime2,"2"])
} ; y

# REQUIREMENT: fPK34

fPK34 = function(THETA)
{
  Vc   = THETA[1]
  Vm   = THETA[2]
  CLp  = THETA[3]
  CLm  = THETA[4]
  CLd1 = THETA[5]
  CLd2 = THETA[6]
  
  y = vector()
  for (i in 1:nID) {
    cID <<- IDs[i] # referencing wihtin PKde
    cy = lsoda(y=c(0, 0), times=TIME, func=PKde, 
               parms=c(Vc=Vc, Vm=Vm, CLp=CLp, CLm=CLm, CLd1=CLd1, CLd2=CLd2))
    iTime1 = TIME %in% dPK34[dPK34$ID==cID & dPK34$MOL == "Cp", "TIME"]
    iTime2 = TIME %in% dPK34[dPK34$ID==cID & dPK34$MOL == "Cm", "TIME"]
    y = c(y, cy[iTime1,"1"], cy[iTime2,"2"])
  }
  return(y)
}

fPK34(c(14.1169, 2.96671, 0.445693, 0.00833429, 0.00308422, 0.0632217))

nlr(fPK34, 
    dPK34, 
    pNames=c("Vc", "Vm", "CLp", "CLm", "CLd1", "CLd2"), 
    IE=c(15, 3, 0.5, 0.01, 0.003, 0.1), 
    Error="P")  # different result

# Vc  12.3 vs 14.1 (R vs WinNonlin, NONMEM)
# AIC -131.0377 vs -131.05554 (R vs WinNonlin)
e$r # -214.8824 vs -214.895 (R vs NONMEM)
