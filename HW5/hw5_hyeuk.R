
# Hyeuk Jung
# MGMTMFE 400-2 Investments (Fall 2018)
# Problems for Lecture 5 - Calculating historical returns
mysoln = list(student = c("Hyeuk Jung", "Huanyu", "Jiaqi", "Xichen"))

library(xts)
df = read.csv("p5-sp500.csv", header = TRUE)

# change row names to dates
df$caldt <- lubridate::ymd(df$caldt)
row.names(df) <- df$caldt
df$caldt <- NULL

# change data frame into xts form
data = as.xts(df)

# calculate price (don't need to)
data$price <- 100/(1 + as.numeric(data$vwretd))


##### Arithmetic return
# 1. Daily
a.daily = mean(data$vwretd)
a.daily.annual = a.daily*365 # annualized
#a.daily.annual = (1+a.daily)^252 or 365 
# from other teams, they say we have to compound the results as the given data is APR --> return EAR

# 2. Monthly
a.monthly = apply.monthly(data$vwretd, mean) # values: each month's avg daily return ex. 1972-01-03 ~ 1972-01-31
a.monthly.annual = mean(a.monthly)*365 # annualized
#a.monthly.annual = (1+mean(a.monthly))^252
#sum(data["1972-01-03/1972-01-31", "vwretd"])/21

# 3. Annually
a.yearly = apply.yearly(data$vwretd, mean)
a.yearly.annual = mean(a.yearly)*365 # annualized

# 4. 5-year returns from 1/3/1972 through 12/30/2016
data2 <- data["1972-01-03/2016-12-30"]
r.5y = endpoints(data2, "years", k=5) # save 5y indexes using endpoints function
a.5y <- data.frame(matrix(ncol = 1, nrow = length(r.5y)-1))
colnames(a.5y) <- "5y daily return"
#r.5y <- r.5y[-1]
for( i in c( 1:(length(r.5y)-1) ) ) {
  a.5y[i,1] = sum(data2[c(r.5y[i+1]:(r.5y[i]+1)), "vwretd"])/length(c(r.5y[i+1]:(r.5y[i]+1))) # 5y daily return
}
a.5y.annual = mean(a.5y[,1])*365 # annualized

mysoln[["Arithmetic"]] = c("day" = a.daily.annual, "month" = a.monthly.annual, "year" = a.yearly.annual, "5yr" = a.5y.annual)


##### Geometric return
# 1. Daily
n_day = length(data$vwretd)
g.daily = 1+as.numeric(data[1,"vwretd"])
for(i in 2:n_day) {
  g.daily <- g.daily * (1 + as.numeric(data[i, "vwretd"])) # compounding daily returns
}
g.daily <- g.daily^(1/n_day) # 1 + average daily return
g.daily.annual <- g.daily^365 - 1 # annualized

# 2. Monthly
n_month = endpoints(data, "months", k=1) # indexes of the end of the months
g.monthly <- data.frame(matrix(ncol = 1, nrow = length(n_month)-1))
compound.monthly = 1
for(i in c( 1:(length(n_month)-1) ) ) {
  g.monthly[i,1] = 1 # initialize for compouding
  for(j in c( (n_month[i]+1):n_month[i+1] )) {
    # ex. n_month[1]+1 ~ n_month[2] -> 1~21 / 22~41 / ... / 11584 ~ 11063
    g.monthly[i,1] <- g.monthly[i,1]*(1+as.numeric(data[j,"vwretd"])) # compounding daily returns of each month
  }
  compound.monthly <- compound.monthly * g.monthly[i,1] # compounding monthly returns
}
compound.monthly <- compound.monthly^(1/(length(n_month)-1)) # 1 + average monthly return
g.monthly.annual <- compound.monthly^12 - 1

# 3. Annually
n_year = endpoints(data, "years", k = 1)
g.yearly <- data.frame(matrix(ncol = 1, nrow = length(n_year)-1))
compound.yearly = 1
for(i in c( 1:(length(n_year)-1) ) ) {
  g.yearly[i,1] = 1 # initialize for compouding
  for(j in c( (n_year[i]+1):n_year[i+1] )) {
    # ex. n_year[1]+1 ~ n_year[2] -> 1~251 / 252~503 / ... / 11353~11603
    g.yearly[i,1] <- g.yearly[i,1]*(1+as.numeric(data[j,"vwretd"])) # compounding daily returns of each year
  }
  compound.yearly <- compound.yearly * g.yearly[i,1] # compounding yearly returns
}
g.yearly.annual <- compound.yearly^(1/(length(n_year)-1)) - 1 # average annual return
#g.monthly.annual <- compound.monthly^12 - 1


# 4. 5-year returns from 1/3/1972 through 12/30/2016
#data2 <- data["1972-01-03/2016-12-30"]
#r.5y = endpoints(data2, "years", k=5) # save 5y indexes using endpoints function
g.5y <- data.frame(matrix(ncol = 1, nrow = length(r.5y)-1))
compound.5y = 1
for(i in c( 1:(length(r.5y)-1) ) ) {
  g.5y[i,1] = 1 # initialize for compouding
  for(j in c( (r.5y[i]+1):r.5y[i+1] )) {
    # ex. r.5y[1]+1 ~ r.5y[2] -> 1~756 / 757~2019 / ... / 10849~11352
    g.5y[i,1] <- g.5y[i,1]*(1+as.numeric(data2[j,"vwretd"])) # compounding daily returns of 5 years
  }
  compound.5y <- compound.5y * g.5y[i,1] # compounding 5yr returns
}
compound.5y <- compound.5y^(1/(length(r.5y)-1)) # 1 + average 5yr return
g.5y.annual <- compound.5y^(1/5) - 1 

mysoln[["Geometric"]] = c("day" = g.daily.annual, "month" = g.monthly.annual, "year" = g.yearly.annual, "5yr" = g.5y.annual)


mysoln
