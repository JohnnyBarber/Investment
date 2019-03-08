# Problem Set 2 Solution Template
# mysoln is a list to store your answers

# the first element of the list is a vector of names for the students in your group
# make sure these match the names shown on the pdf document provided by the MFE office
# using group 1 as an example:
mysoln = list(student = c("Molin Liang", "Meghana Rao", "Chengbo Du", "Shardul Kulkarni"))

# 1

# your intermediary code to get your answers here
priceA=100/(1+0.06)^3
ytmA1=function(ytm)100 / ((1 + ytm)^3) -priceA
ytmA=uniroot(ytmA1,interval=c(0,25))  
priceA
ytmA
couponB=100*0.06
priceB=couponB/(1+0.05)+(couponB+100)/((1+0.055)^2)
ytmB1=function(ytm)couponB/(1+ytm)+(couponB+100)/((1+ytm)^2)-priceB
ytmB=uniroot(ytmB1,interval=c(0,25))  
priceB
ytmB
couponC=100*0.08
priceC=couponC/(1+0.05)+couponC/((1+0.055)^2)+couponC/((1+0.06)^3)+(100+couponC)/((1+0.063)^4)
ytmC1=function(ytm)couponC/(1+ytm)+couponC/((1+ytm)^2)+couponC/((1+ytm)^3)+(couponC+100)/((1+ytm)^4)-priceC
ytmC=uniroot(ytmC1,interval=c(0,25))  
priceC
ytmC
# save down your final answers for part a, b, and c
a = c(priceA,ytmA$root)#ytm in decimal form
b = c(priceB,ytmB$root) #ytm in decimal form
c = c(priceC,ytmC$root) #ytm in decimal form

# add answers to list for "Q1"
mysoln[["Q1"]] = list(a=a, b=b, c=c)

# 2

# your intermediary code to get your answers here
couponX=100*0.04/2
r.6month1 = (couponX+100)/100.98-1
r.6month = r.6month1*2
r.6month
couponY=100*0.06
r.1yr1 = (couponY+100)/103.59-1
r.1yr = r.1yr1
r.1yr
# save down your final answers
a = c(r.6month, r.1yr) #in decimal form

# add answers to list for "Q2"
mysoln[["Q2"]] = list(a=a)

# 3
# your intermediary code to get your answers her
Y1spot=100/95.238-1
Y1spot
couponB1=0.05*100
Y2spot1=function(ytm)couponB1/(1+Y1spot)+(couponB1+100)/((1+ytm)^2)-98.438
Y2spot=uniroot(Y2spot1,interval=c(0,25)) 
Y2spot
couponC1=100*0.07
priceC1=couponC1/(1+Y1spot)+(couponC1+100)/((1+Y2spot$root)^2)
priceC1
gain=103.37-priceC1
gain
# save down your final answers

#Bond C is underpriced and could achive the arbicharge by buy the replicating portfolio at 102.13 and sell the  sell the coupon rate at 102.13. The gain would be 1.24

# Put the answer in your PDF writeup
