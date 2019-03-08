# Problem Set 2 Solution Template
# mysoln is a list to store your answers

# the first element of the list is a vector of names for the students in your group
# make sure these match the names shown on the pdf document provided by the MFE office
# using group 1 as an example:
mysoln = list(student = c("Hyeuk Jung", "", "", ""))

# 1
# your intermediary code to get your answers here

price <- function(par = 100, coupon = 0, year = 1){
  PV = 0
  ytm = 0
  spot_rate = c(0.05, 0.055, 0.06, 0.063)
  
  # calculating the PV of interest
  for(i in 1:year){
    PV = PV + (coupon / (1 + spot_rate[i])^i)
  }
  # calculating the PV of principal
  PV = PV + (par / (1 + spot_rate[year])^year)
}

PV_1_a = price(100, 0, 3) #PV_1_a_1 = 100 / (1+0.06)^3 # $83.96
PV_1_b = price(100, 6, 2) #PV_1_b_1 = (6 / (1+0.05)) + ((6+100) / (1+0.055)^2) #100.95
PV_1_c = price(100, 8, 4) #PV_1_c_1 = (8 / (1+0.05)) + (8 / (1+0.055)^2) + (8 / (1+0.06)^3) + ((100 + 8) / (1+0.063)^4) # 106.11

ytm <- function(pv = 100, par = 100, coupon = 0, year = 1) {

  # create empty vector for the polynomial
  a = vector(length = year + 1)
  for(i in 1:(year+1)) {
    if(i == 1) a[i] = -(par+coupon)
    else if(i == (year+1)) a[i] = pv
    else a[i] = -(coupon)
  }
  
  # polyroot(a)
  # cast solution of polynomial equation into double type
  result = as.double(polyroot(a))
  
  # find the solution which is greater or equal to 0 and return the value as YTM
  for(i in 1:length(a)) {
    if(result[i] >= 0)
      return (result[i] - 1)
  }
}

ytm_1_a = ytm(pv = PV_1_a, par = 100, year = 3)
ytm_1_b = ytm(pv = PV_1_b, par = 100, coupon = 6, year = 2)
ytm_1_c = ytm(pv = PV_1_c, par = 100, coupon = 8, year = 4)

# save down your final answers for part a, b, and c
a = c(PV_1_a, ytm_1_a) #ytm in decimal form
b = c(PV_1_b, ytm_1_b) #ytm in decimal form
c = c(PV_1_c, ytm_1_c) #ytm in decimal form

# add answers to list for "Q1"
mysoln[["Q1"]] = list(a=a, b=b, c=c)

# 2
# your intermediary code to get your answers here

par = 100
coupon_x = par * 0.04; price_x = 100.98
coupon_y = par * 0.06; price_y = 103.59

# (100+2)/100.98 = 1 + (6m spot rate/2) = 1.0101 -> annualized spot_6m (r.6month) = 0.0202
r.6month = (( (par + (coupon_x/2)) / price_x ) - 1 ) * 2

# compound_1y = (100+3) / (1 + 1y spot/2)^2 = 103.59 - pv(1st coupon) = 1.0237
# sqrt(compound_1y) = 1.0118 --> r.1yr = (1.0118 - 1)*2 = 0.0235
compound_1y = (par + coupon_y/2) / (price_y - (coupon_y/2 / (1 + r.6month/2)))
r.1yr = (sqrt(compound_1y) - 1)*2


# save down your final answers
a = c(r.6month, r.1yr) #in decimal form

# add answers to list for "Q2"
mysoln[["Q2"]] = list(a=a)


# 3
# your intermediary code to get your answers here
# Assumption: bond A and B are fairly priced
par_3 = 100
price_3a = 95.238
price_3b = 98.438; coupon_3b = par_3 * 0.05;
price_3c = 103.370; coupon_3c = par_3 * 0.07;

spot_1y = (par / price_3a) - 1 # spot_1y = 0.05
spot_2y = sqrt( (par_3 + coupon_3b) / ( price_3b - coupon_3b/(1+spot_1y) ) ) - 1 # spot_2y = 0.0587

# Using spot_1y and spot_2y to calculate the fair price of bond C, the result is:
fair_price_3c = (coupon_3c / (1+spot_1y)) + ( (par_3 + coupon_3c) / (1 + spot_2y)^2 )
# fair_price_3c = 102.127

arbitrage_3c = price_3c - fair_price_3c # $1.24 per bond C

# save down your final answers

# Put the answer in your PDF writeup



# (100+2)/100.98 = 1 + 6m spot rate -> (1+spot_6m)^2 - 1 = annualized spot_6m (r.6month)
#r.6month = ((par + (coupon_x/2)) / price_x)^2 - 1 

# (100+3) / (103.59 - (3 / (1+r.6month/2))) = 1 + r.1yr_a = 1 + 1y spot rate
#r.1yr_a = ((par + (coupon_y/2)) / (price_y - ((coupon_y/2)/(1+r.6month/2)))) - 1
# (1 + (spot_1y / 2))^2 - 1 = semiannual compounded 1y spot rate
#r.1yr = (1 + r.1yr_a/2)^2
