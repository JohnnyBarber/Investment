mysoln = list(student = c("Hyeuk Jung", "Huanyu Liu", "Jiaqi Li", "Xichen Luo"))

library("xts")
sp500 = read.csv("p5-sp500.csv")
sp500$caldt = as.Date(as.character(sp500$caldt),"%Y%m%d")
rownames(sp500) = sp500[,1]
sp500$caldt = NULL
sp500 = as.xts(sp500)

year_end = endpoints(sp500,"years",k=1)
month_end = endpoints(sp500,"months",k =1)

annual_return = vector()
month_return = vector()
year5_return = vector()
month_return_sum = vector()
annual_return_sum = vector()
year5_return_sum = vector()

daily_returns = sp500["1972-01-03/2017-12-30"]
daily_return_a = mean(daily_returns) * 365
daily_return_g = prod(daily_returns + 1)^(1/length(daily_returns)) - 1
daily_return_g = (1 + daily_return_g)^365 - 1

for (i in 1:(length(month_end) - 1 )){
  returns = as.vector(sp500[(month_end[i] + 1):month_end[i+1]])
  month_return[i] = prod(1 + returns) - 1
  month_return_sum[i] = sum(returns)
}

month_return_a = mean(month_return_sum) * 12
month_return_g = prod(month_return + 1)^(1/length(month_return)) - 1
month_return_g = (1 + month_return_g)^12 - 1

for (i in 1:(length(year_end) - 1)){
  returns = as.vector(sp500[(year_end[i]+1):year_end[i+1]])
  annual_return[i] = prod(1 + returns) - 1
  annual_return_sum[i] = sum(returns)
}


annual_return_a = mean(annual_return_sum)
annual_return_g = prod(annual_return + 1)^(1/length(annual_return)) -1

for (i in seq(1,length(year_end) - 6, 5)){
  returns = as.vector(sp500[(year_end[i] + 1):year_end[i+5]])
  year5_return[i %/% 5 + 1] = prod(returns + 1) - 1
  year5_return_sum[i %/% 5 + 1] = sum(returns)
}

year5_return_a = mean(year5_return_sum) / 5
year5_return_g = prod(1 + year5_return)^(1/length(year5_return)) - 1
year5_return_g = (1 + year5_return_g)^(1/5) - 1


mysoln[["Arithmetic"]] = c("daily" = daily_return_a, "monthly" = month_return_a, "annually" = annual_return_a, "5year" = year5_return_a)
mysoln[["Geometric"]] = c("daily" = daily_return_g, "monthly" = month_return_g, "annually" = annual_return_g, "5year" = year5_return_g)

mysoln