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
DA = 0
for (t in 1:9)
{
  DA = DA + 0.5/(1.0175^t)/PA*t
}
DA = DA + 100.5/(1.0175^10)/PA*10/2
DA_modified = DA/1.0175

#bond B
PB = PV(100,0.5,20,0.0175)
DB = 0
for (t in 1:19)
{
  DB = DB + 0.5/(1.0175^t)/PB*t
}
DB = DB + 100.5/(1.0175^20)/PB*20/2
DB_modified = DB/1.0175

#bond C
PC = PV(100,2,10,0.0175)
DC = 0
for (t in 1:9)
{
  DC = DC + 2/(1.0175^t)/PC*t
}
DC = DC + 102/(1.0175^10)/PC*10/2
DC_modified = DC/1.0175

#bond D
PD = PV(100,2,20,0.0175)
DD = 0
for (t in 1:19)
{
  DD = DD + 2/(1.0175^t)/PD*t
}
DD = DD + 102/(1.0175^20)/PD*20/2
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
b = c(0,0,0,0) #(Bond A, Bond B, Bond C, Bond D)
c.prices = c(0,0,0,0) #(Bond A, Bond B, Bond C, Bond D)
c.changes = c(0.01,0.01,0.01,0.01) #(Bond A % Chg, Bond B % Chg, Bond C % Chg, Bond D % Chg) in decimal form
d.prices = c(0,0,0,0) #(Bond A, Bond B, Bond C, Bond D)
d.changes = c(0.01,0.01,0.01,0.01) #(Bond A % Chg, Bond B % Chg, Bond C % Chg, Bond D % Chg) in decimal form
e = c(0,0,0,0) #(Bond A duration, Bond B duration, Bond C duration, Bond D duration)
eu = c(0,0,0,0) # price changes for 3.0->3.5 yld chg
ed = c(0,0,0,0) # price changes for 3.0->2.5 yld chg
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
