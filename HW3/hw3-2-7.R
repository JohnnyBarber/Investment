# Problem Set 3 Solution Template
# mysoln is a list to store your answers

# the first element of the list is a vector of names for the students in your group
# make sure these match the names shown on the pdf document provided by the MFE office
# using group 1 as an example:
mysoln = list(student = c("Huanyu Liu", "Hyeuk Jung", "Jiaqi Li", "Xichen Luo"))

# 1

# your intermediary code to get your answers here
# 1. (a)
library(FinancialMath)
price = 10/(1+0.05) + 10/(1+0.055)^2 + 110/(1+0.065)^3
ytm = IRR(cf0 = price, cf = c(10,10,110), times = c(1,2,3))
price; ytm

# 1. (b)
forward_1_1 = ((1.055)^2 / 1.05) - 1
forward_1_2 = sqrt((1.065)^3 / 1.05) - 1
forward_2_1 = ((1.065)^3 / (1.055)^2) - 1
forward_1_1; forward_1_2; forward_2_1

# 1. (c)
# Roll over the investment. That being said;
# Method 1. we can exploit r1, 1f1, 2f1 in each period.
price_1_c_fv_1 = 10*(1+forward_1_1)*(1+forward_2_1) + 10*(1+forward_2_1) + 110
return.3yr_1 = (price_1_c_fv_1/price)^(1/3) - 1
return.3yr_1

# Method 2. reinvest year 1's coupon with 1f2 and the year 2's coupon with 2f1
price_1_c_fv2 = 10*(1+forward_1_2)^2 + 10*(1+forward_2_1) + 110
return.3yr2 = (price_1_c_fv2/price)^(1/3) - 1
return.3yr2


# save down your final answers for part a, b, and c
a = c(price, ytm)
b = c(forward_1_1,forward_1_2,forward_2_1) #increase/decrease vector depending on how many forward rates you can calculate
c = return.3yr

# add answers to list for "Q2"
mysoln[["Q1"]] = list(a=a, b=b, c=c)

# 2

# put a and c in pdf

# Forward rate
b=sqrt((1+0.07)^3/(1+0.06))-1

mysoln[["Q2"]] = list(b=b)

# 3

# your intermediary code to get your answers here

PV <- function(par, coupon, period, yield){
  PV = 0
  for(i in 1:period){
    PV = PV + (coupon / (1 + yield)^i)
  }
  PV = PV + (par / (1 + yield)^(period))
  return(PV)
}

#bond A
PA = PV(100,0.5,10,0.0175)
PA_in = PV(100,0.5,10,0.019)
delta_PA_in = PA_in - PA
percent_PA_in = delta_PA_in/PA
PA_de = PV(100,0.5,10,0.016)
delta_PA_de = PA_de - PA
percent_PA_de = delta_PA_de/PA
DA = 0
for (t in 1:9)
{
  DA = DA + 0.5/(1.0175^t)/PA*t
}
DA = (DA + 100.5/(1.0175^10)/PA*10)/2
DA_modified = DA/1.0175

#bond B
PB = PV(100,0.5,20,0.0175)
PB_in = PV(100,0.5,20,0.019)
delta_PB_in = PB_in - PB
percent_PB_in = delta_PB_in/PB
PB_de = PV(100,0.5,20,0.016)
delta_PB_de = PB_de - PB
percent_PB_de = delta_PB_de/PB
DB = 0
for (t in 1:19)
{
  DB = DB + 0.5/(1.0175^t)/PB*t
}
DB = (DB + 100.5/(1.0175^20)/PB*20)/2
DB_modified = DB/1.0175

#bond C
PC = PV(100,2,10,0.0175)
PC_in = PV(100,2,10,0.019)
delta_PC_in = PC_in - PC
percent_PC_in = delta_PC_in/PC
PC_de = PV(100,2,10,0.016)
delta_PC_de = PC_de - PC
percent_PC_de = delta_PC_de/PC
DC = 0
for (t in 1:9)
{
  DC = DC + 2/(1.0175^t)/PC*t
}
DC = (DC + 102/(1.0175^10)/PC*10)/2
DC_modified = DC/1.0175

#bond D
PD = PV(100,2,20,0.0175)
PD_in = PV(100,2,20,0.019)
delta_PD_in = PD_in - PD
percent_PD_in = delta_PD_in/PD
PD_de = PV(100,2,20,0.016)
delta_PD_de = PD_de - PD
percent_PD_de = delta_PD_de/PD
DD = 0
for (t in 1:19)
{
  DD = DD + 2/(1.0175^t)/PD*t
}
DD = (DD + 102/(1.0175^20)/PD*20)/2
DD_modified = DD/1.0175

INchangeA = -0.003*DA_modified*PA
INchangeB = -0.003*DB_modified*PB
INchangeC = -0.003*DC_modified*PC
INchangeD = -0.003*DD_modified*PD

DEchangeA = 0.003*DA_modified*PA
DEchangeB = 0.003*DB_modified*PB
DEchangeC = 0.003*DC_modified*PC
DEchangeD = 0.003*DD_modified*PD

# save down your final answers for part b,c,d,and e (a and f in PDF writeup)
#a = "Put in PDF Write Up"
b = c(PA,PB,PC,PD) #(Bond A, Bond B, Bond C, Bond D)
c.prices = c(PA_in,PB_in,PC_in,PD_in) #(Bond A, Bond B, Bond C, Bond D)
c.changes = c(percent_PA_in,percent_PB_in,percent_PC_in,percent_PD_in) #(Bond A % Chg, Bond B % Chg, Bond C % Chg, Bond D % Chg) in decimal form
d.prices = c(PA_de,PB_de,PC_de,PD_de) #(Bond A, Bond B, Bond C, Bond D)
d.changes = c(percent_PA_de,percent_PB_de,percent_PC_de,percent_PD_de) #(Bond A % Chg, Bond B % Chg, Bond C % Chg, Bond D % Chg) in decimal form
e = c(DA_modified,DB_modified,DC_modified,DD_modified) #(Bond A duration, Bond B duration, Bond C duration, Bond D duration)
eu = c(INchangeA,INchangeB,INchangeC,INchangeD) # price changes for 3.0->3.5 yld chg
ed = c(DEchangeA,DEchangeB,INchangeC,INchangeD) # price changes for 3.0->2.5 yld chg
#f = "Put in PDF Write up" 

# add answers to list for "Q2"
mysoln[["Q3"]] = list(#a=a, put in PDF writeup only
  b=b, 
  c.pric = c.prices, 
  c.chg = c.changes, #changes are percent changes in decimal
  d.pric = d.prices, 
  d.chg = d.changes, #changes are percent changes in decimal
  e = e,
  eu = eu,
  ed = ed)
#f = f put in PDF writeup only

# 4

# your intermediary code to get your answers here

bondA.weight = 2000000 / 100

A = matrix(data=c(4, 6, 104, 106), nrow=2, ncol=2, byrow=TRUE)    
b = matrix(data=c(0,1000000),nrow=2, ncol=1, byrow=FALSE)
b.and.c = solve(A, b)
bondB.weight = b.and.c[1]
bondC.weight = b.and.c[2]
# answers
# Note: Remember to add conclusions in write-up

discount = 1 + 0.06
price.D = 100 / discount^10
price.E = 100 / discount^15
liability = 1000000 / discount^30 + 2000000 / discount^31
weight.D = price.D / liability
weight.E = price.E / liability
liability_duration = 1000000/discount^30 / liability * 30 + 2000000/discount^31 / liability * 31
A = matrix(data = c(10 * weight.D, 15 * weight.E, price.D, price.E), nrow = 2, ncol = 2, byrow = TRUE)
b = matrix(data = c(liability_duration, liability), nrow = 2, ncol = 1, byrow = FALSE)
D.and.E = solve(A,b)
bondD.weight = D.and.E[1]
bondE.weight = D.and.E[2]



a = c(bondA.weight, bondB.weight, bondC.weight)
b = c(bondD.weight, bondE.weight)


#c = "Put in PDF writeup"
#d = "Put in PDF writeup"

# add answers to list for "Q4"
mysoln[["Q4"]] =  list(a=a, b=b) #c and d in writeup


# return my solution
mysoln