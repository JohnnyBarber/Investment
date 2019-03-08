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
# 2. (a) 1f2 
# 2. (b)
b = sqrt((1 + 0.07)^3 / (1 + 0.06)) - 1

# 2. (c) Short 1 year zcb, and long 3 year zcb using the money earned from 1y zcb short
#        1 year later, we pay the 1y zcb with $1m

mysoln[["Q2"]] = list(b=b)

# 3

# your intermediary code to get your answers here

price <- function(par = 100, coupon = 0, maturity = 1, payment = 1, yield = 0){
  PV = 0
  #payment = number of interest payment in a year (semi-annual=2 / quarterly=4 / ...)
  
  # calculating the PV of interest
  for(i in 1:(payment*maturity)){
    PV = PV + ((coupon/payment) / (1 + yield/payment)^i)
  }
  # calculating the PV of principal
  PV = PV + (par / (1 + yield/payment)^(payment*maturity))
}

mac.dur <- function(price, par = 100, coupon = 0, maturity = 1, payment = 1, yield = 0){
  duration = 0
  #payment = number of interest payment in a year (semi-annual=2 / quarterly=4 / ...)
  
  for(i in 1:(maturity*payment)){
    duration = duration + ((coupon/payment) / (1+yield/payment)^(i)) / price * i
  }
  duration = duration + (par / (1+yield/payment)^(maturity*payment)) / price * (maturity*payment)
  
  return (duration / payment)
}

# 3. (b)
p_A_35 = price(100, 1, 5, 2, 0.035)
p_B_35 = price(100, 1, 10, 2, 0.035)
p_C_35 = price(100, 4, 5, 2, 0.035)
p_D_35 = price(100, 4, 10, 2, 0.035)
# p_A_35; p_B_35; p_C_35; p_D_35; 

# 3. (c)
p_A_38 = price(100, 1, 5, 2, 0.038); chg_A_38 = (p_A_38 - p_A_35) / p_A_35
p_B_38 = price(100, 1, 10, 2, 0.038); chg_B_38 = (p_B_38 - p_B_35) / p_B_35
p_C_38 = price(100, 4, 5, 2, 0.038); chg_C_38 = (p_C_38 - p_C_35) / p_C_35
p_D_38 = price(100, 4, 10, 2, 0.038); chg_D_38 = (p_D_38 - p_D_35) / p_D_35
# p_A_38; chg_A_38; p_B_38; chg_B_38; p_C_38; chg_C_38; p_D_38; chg_D_38;

# 3. (d)
p_A_32 = price(100, 1, 5, 2, 0.032); chg_A_32 = (p_A_32 - p_A_35) / p_A_35
p_B_32 = price(100, 1, 10, 2, 0.032); chg_B_32 = (p_B_32 - p_B_35) / p_B_35
p_C_32 = price(100, 4, 5, 2, 0.032); chg_C_32 = (p_C_32 - p_C_35) / p_C_35
p_D_32 = price(100, 4, 10, 2, 0.032); chg_D_32 = (p_D_32 - p_D_35) / p_D_35
# p_A_32; chg_A_32; p_B_32; chg_B_32; p_C_32; chg_C_32; p_D_32; chg_D_32;

# 3. (e)
mac_A = mac.dur(p_A_35, 100, 1, 5, 2, 0.035); mod_A = mac_A / (1+0.035/2); dv01_A = mod_A * p_A_35 / 10000
mac_B = mac.dur(p_B_35, 100, 1, 10, 2, 0.035); mod_B = mac_B / (1+0.035/2); dv01_B = mod_B * p_B_35 / 10000
mac_C = mac.dur(p_C_35, 100, 4, 5, 2, 0.035); mod_C = mac_C / (1+0.035/2); dv01_C = mod_C * p_C_35 / 10000
mac_D = mac.dur(p_D_35, 100, 4, 10, 2, 0.035); mod_D = mac_D / (1+0.035/2); dv01_D = mod_D * p_D_35 / 10000

# change 3.5 -> 3.8
chg_A_03u = (-1) * p_A_35 * mod_A * (0.003); check_Au = p_A_38 - p_A_35
# chg_A_03u; check_A

chg_B_03u = (-1) * p_B_35 * mod_B * (0.003); check_Bu = p_B_38 - p_B_35
# chg_B_03u; check_B

chg_C_03u = (-1) * p_C_35 * mod_C * (0.003); check_Cu = p_C_38 - p_C_35
# chg_C_03u; check_Cu

chg_D_03u = (-1) * p_D_35 * mod_D * (0.003); check_Du = p_D_38 - p_D_35
# chg_D_03u; check_Du

# change 3.2 -> 3.5
chg_A_03d = (-1) * p_A_35 * mod_A * (-0.003); check_Ad = p_A_35 - p_A_38
# chg_A_03d; check_Ad

chg_B_03d = (-1) * p_B_35 * mod_B * (-0.003); check_Bd = p_B_35 - p_B_38
# chg_B_03d; check_Bd

chg_C_03d = (-1) * p_C_35 * mod_C * (-0.003); check_Cd = p_C_35 - p_C_38
# chg_C_03d; check_Cd

chg_D_03d = (-1) * p_D_35 * mod_D * (-0.003); check_Dd = p_D_35 - p_D_38
# chg_D_03d; check_Dd


# 3. (f)
# Simply put, the interest rate and the price of the bond has negative relationship.
# Maturity --> ...?
# Coupons --> Higher coupon rates diminishes the impact of interest rate on bond price.


# save down your final answers for part b,c,d,and e (a and f in PDF writeup)
#a = "Put in PDF Write Up"
b = c(p_A_35, p_B_35, p_C_35, p_D_35) #(Bond A, Bond B, Bond C, Bond D)
c.prices = c(p_A_38, p_B_38, p_C_38, p_D_38) #(Bond A, Bond B, Bond C, Bond D)
c.changes = c(chg_A_38, chg_B_38, chg_C_38, chg_D_38) #(Bond A % Chg, Bond B % Chg, Bond C % Chg, Bond D % Chg) in decimal form
d.prices = c(p_A_32, p_B_32, p_C_32, p_D_32) #(Bond A, Bond B, Bond C, Bond D)
d.changes = c(chg_A_32, chg_B_32, chg_C_32, chg_D_32) #(Bond A % Chg, Bond B % Chg, Bond C % Chg, Bond D % Chg) in decimal form
e = c(mod_A , mod_B, mod_C, mod_D) #(Bond A duration, Bond B duration, Bond C duration, Bond D duration)
eu = c(chg_A_03u, chg_B_03u, chg_C_03u, chg_D_03u) # price changes for 3.0->3.5 yld chg
ed = c(chg_A_03d, chg_B_03d, chg_C_03d, chg_D_03d) # price changes for 3.0->2.5 yld chg
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
# 4. (a)
# bond A -> x, B -> y, C -> z
# 4y + 6z = 0; year 1 ~ 29, there should be zero cash flow
# 104y + 106z = 1,000,000; in year 30 $1M
# 100x = 2,000,000; $2M in year 31.
bondA.weight = 2000000/100 #20,000 units of bond A
bondC.weight = -1000000/50 # -20,000 units of bond C
bondB.weight = (-6) * bondC.weight / 4 # 30,000 units of bond B

# 4. (b) Need to check
price_bond = price(1000000, 0, 30, 1, 0.06) + price(2000000, 0, 31, 1, 0.06)
md_bond = (1000000/(1.06)^30 * 30/price_bond + 2000000/(1.06)^31 * 31/price_bond) / 1.06 # 28.92
dv01_bond = price_bond * md_bond / 10000
# Let's suppose we are trading ZCB with the maturity of 12.5 years. (10+15)/2 = 12.5
md_zcb = 12.5 / 1.06 # 11.79
price_zcb = price(100, 0, 12.5, 1, 0.06) # 48.27
dv01_zcb = price_zcb * md_zcb / 10000 # 0.0569
# For the immunized portfolio, the amount of change for the duration should be the same
# dv01_bond*yield chg - dv01_zcb* units *yield chg = 0
units_zcb = dv01_bond / dv01_zcb # 25,534.91 units

# 4. (c) No, as the convexity was not considered, the portfolios will have the different value.
# 4. (d) Synthetic replication: Simple and safest. Matching maturity with ZCBs-> reinvestment risk & price risk x
#        Approximate hedging: Flexible and practical. 
#                             Available by futures, options, etc. (no need for changing underlying assets) 
#                             Structural risk (convexity should be considered).


# answers
# Note: Remember to add conclusions in write-up
a = c(bondA.weight, bondB.weight, bondC.weight)
b = c(bondC.weight, bondD.weight)
#c = "Put in PDF writeup"
#d = "Put in PDF writeup"

# add answers to list for "Q4"
mysoln[["Q4"]] =  list(a=a, b=b) #c and d in writeup


# return my solution
mysoln
