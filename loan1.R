## Car loan scenarios

library(tidyverse)
library(readxl)
library(scales)
library(plotly) ## not available
library(FinancialMath)

## read original data from Excel ####
loan <- read_xlsx("C:\\Users\\John\\Google Drive\\John\\Expenses.xlsx", sheet='Tesla', range='D1:N6')

loan.t <- t(loan)
loan.t <- as.data.frame(loan.t)
loan.t$scenario <- row.names(loan.t)
row.names(loan.t) <- c(1:nrow(loan.t))
loan.t2 <- loan.t
write_csv(loan.t2, "loan-data.csv")
loan.data <- read_csv("loan-data.csv")
colnames(loan.data) <- loan.data[1,]
loan.data <- loan.data[-1,]
colnames(loan.data)[6] <- 'Scenario'
loan.data <- loan.data %>% select(6,1:5)

## [IDEALLY SHOULD'VE RENAMED COLS TO MORE FRIENDLY NAMES]
## done for second scenarios below

## set variable types ####
loan.data$`Down pmt` <- as.integer(loan.data$`Down pmt`)
loan.data$`Loan Amount` <- as.integer(loan.data$`Loan Amount`)
loan.data$`Period (yrs)` <- as.integer(loan.data$`Period (yrs)`)
loan.data$Int <- as.numeric(loan.data$Int)
loan.data$`PMT (mthly)` <- as.numeric(loan.data$`PMT (mthly)`)
loan.data$Scenario <- as.factor(loan.data$Scenario)

## calculations based on loan data ####
loan.data <- loan.data %>%
  mutate(
    pmt.annual=`PMT (mthly)`*12,
    pmt.life=pmt.annual*`Period (yrs)`,
    pmt.ttl=pmt.life+`Down pmt`)

## save results ####
write_csv(loan.data, "loan-data-complete.csv")

loan.data <- read_csv('loan-data-complete.csv')

## [another missed opportunity to change to friendlier col names]

## visualize ####

ggplot(loan.data, aes(x=`PMT (mthly)`, y=pmt.ttl, size=`Down pmt`, color=factor(`Period (yrs)`)))+
  geom_point()

ggplot(loan.data, aes(x=`PMT (mthly)`, y=pmt.ttl, size=`Down pmt`, color=factor(Scenario)))+
  geom_point()

ggplot(loan.data, aes(x=`PMT (mthly)`, y=pmt.ttl, size=`Down pmt`, color=factor(Scenario)))+
  geom_point()+
  scale_y_continuous(limit=c(0,max(loan.data$pmt.ttl)))

ggplot(loan.data, aes(x=`PMT (mthly)`, y=pmt.ttl, size=`Down pmt`, color=factor(`Period (yrs)`),
                      label=Scenario))+
  geom_point()+
  geom_text(vjust=-1)+
  scale_y_continuous(limit=c(0,max(loan.data$pmt.ttl)))

ggplot(loan.data, aes(x=`PMT (mthly)`, y=pmt.ttl, size=`Down pmt`, color=factor(`Period (yrs)`),
                      label=Scenario))+
  geom_point()+
  geom_text(vjust=-1)+
  scale_y_continuous(limit=c(40000,max(loan.data$pmt.ttl)))

ggplot(loan.data, aes(x=`PMT (mthly)`, y=pmt.ttl, size=`Down pmt`, color=factor(`Period (yrs)`),
                      label=Scenario))+
  geom_point()+
  geom_text(vjust=-1)+
  scale_y_continuous(limit=c(70000,max(loan.data$pmt.ttl)))

## more analysis -> rename data cols and resave ####
# loan.data2 <- loan.data %>%
#   rename(
#     Down.pmt=`Down pmt`,
#     Loan.amt=`Loan Amount`,
#     Period.yrs=`Period (yrs)`,
#     PMT.mth=`PMT (mthly)`
#   )
# write_csv(loan.data2, "loan-data-complete-rename.csv")
## reset loan.data with better names
loan.data <- read_csv('loan-data-complete-rename.csv')

## add a scenario ####
scen <- 'K'
ttl.cost <- 73200 ## full cost, all in is $73200
dp <- 21000
per.yr <- 6
int.rt <- 0.045

loan.scen <- data.frame(
  Scenario=scen,
  Down.pmt=dp,
  Loan.amt=ttl.cost-dp,
  Period.yrs=per.yr,
  Int=int.rt
)

## make loan calculations using library(FinancialMath) ####
## https://cran.r-project.org/web/packages/FinancialMath/FinancialMath.pdf
loan.scen <- loan.scen %>%
  mutate(PMT.mth=amort.period(Loan=ttl.cost-dp, n=per.yr*12, i=int.rt, ic=12, 
                              pf=12, t=1)[2],
         pmt.annual=PMT.mth*12,
         pmt.life=pmt.annual*per.yr,
         pmt.ttl=pmt.life+dp)

## add new scenario to original data ####
loan.data <- bind_rows(loan.data, loan.scen)

## visualize with new scenario
ggplot(loan.data, aes(x=PMT.mth, y=pmt.ttl, size=Down.pmt, color=factor(Period.yrs),
                      label=Scenario))+
  geom_point()+
  geom_text(vjust=-1)+
  scale_y_continuous(limit=c(70000,max(loan.data$pmt.ttl)))