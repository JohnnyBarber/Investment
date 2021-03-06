# Problem Set 2 Solution Template
# mysoln is a list to store your answers

# the first element of the list is a vector of names for the students in your group
# make sure these match the names shown on the pdf document provided by the MFE office
# using group 1 as an example:
mysoln = list(student = c("Huanyu Liu", "Hyeuk Jung", "Jiaqi Li", "Xichen Luo"))
 
# 1
# your intermediary code to get your answers here

library(FinancialMath)

price_a = 100/(1+0.06)^3
ytm_a = IRR(-100/(1+0.06)^3,c(0,0,100),c(1,2,3))

price_b = 6/(1+0.05) + 106/(1+0.055)^2
ytm_b = IRR(-(6/(1+0.05) + 106/(1+0.055)^2),c(6,106),c(1,2))

price_c = 8/(1+0.05) + 8/(1+0.055)^2 + 8/(1+0.06)^3 + 108/(1+0.063)^4
ytm_c = IRR(-(8/(1+0.05) + 8/(1+0.055)^2 + 8/(1+0.06)^3 + 108/(1+0.063)^4),c(8,8,8,108),c(1,2,3,4))
# save down your final answers for part a, b, and c
a = c(price_a, ytm_a) #ytm in decimal form
b = c(price_b, ytm_b) #ytm in decimal form
c = c(price_c, ytm_c) #ytm in decimal form

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

# suppose we buy x Bond A, y Bond B, z Bond C (negative value means short that amount of bond)
# To eliminate any future payments, we have
# 100x + 5y + 7z = 0
# 105y + 107z = 0
# x = (-2/105)z
# y = (-107/105)z
# To get initial cash inflow at the beginning, we have
# - A * x - B * y - C * z > 0  (buy x Bond A will have A * x cash outflow)
# we can get z < 0, using all the above equations
# suppose we short 1 bond C
priceA = 95.238
priceB = 98.438
priceC = 103.370

cash_inflow = -2/105 * priceA - 107/105 * priceB + priceC
# save down your final answers
# Put the answer in your PDF writeup
mysoln[["Q3"]] = list(a=cash_inflow)


mysoln




