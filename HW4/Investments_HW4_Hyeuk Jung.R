# Hyeuk Jung
# MGMTMFE 400-2 Investments (Fall 2018)
library(xts)

df = read.csv("/Users/hyeukjung/Desktop/UCLA related/400_Investments/Week 4 & HW by 20181022/HW 4/feds200628.csv", header = TRUE)

# Step 3. Starting with $1M on 12/30/1983, set up a DV01-neutral yield curve spread trade. 10% capital requirement.
df$X = as.Date(df$X)
df2 = subset.data.frame(df, df$X >= "1983-12-30" & df$X <= "2018-06-30")
rownames(df2) = df2[, 1] # updating the rownames
df2$X = NULL # delete the first row (dates)
df2[, c(1:30)] = df2[, c(1:30)]*.01 # changing the % data to decimals

# transform to xts
yields = as.xts(df2)
days = endpoints(yields, 'weeks', k = 1) # row numbers of every friday
days = days[-1]

NSS_model <- function(parameters, t) {
  beta0 = parameters[1]
  beta1 = parameters[2]
  beta2 = parameters[3]
  beta3 = parameters[4]
  t1 = t / parameters[5]
  t2 = t / parameters[6] 
  
  rt = beta0 + beta1*(1-exp(-t1))/t1 + beta2*( (1 - exp(-t1))/t1 - exp(-t1) ) + beta3*( (1 - exp(-t2))/t2 - exp(-t2) )
  
  return(rt)
}

portfolio <- function(init = 1000000, margin = 0.1) {
  # Q1 and Q3
  result1 = matrix(nrow = length(days), ncol = 13, byrow = TRUE)
  colnames(result1) = c("num 2y", "num 10y", "price 2y", "price 10y", "interest", "total", "wk return", "spread r", "conv.return", "time r", "residual", "d2", "d10")
  
  cash0 = init # initial capital
  
  y.9y358d = 10 - 7/365; 
  y.1y358d = 2 - 7/365; 
  week = 7/365; 
  
  for(i in c(1:(length(days) - 1))) {
    
    weekdata0 = as.vector(yields[days[i], ])
    if(any( is.na(weekdata0[c(2,10)]) )) {
      weekdata0 = as.vector( yields[days[i]+1, ] )
    }
    
    y.2.10 = weekdata0[c(2, 10)] # 2y- and 10y-bond yield
    
    start.2y = 100 * exp(-1*y.2.10[1]*2) # 80.98
    start.10y = 100 * exp(-1*y.2.10[2]*10) # 31.21
    md.2 = 2 * exp(-1*y.2.10[1])
    md.10 = 10 * exp(-1*y.2.10[2])
    
    # units of eash bond
    A = matrix(data = c(md.2*start.2y, md.10*start.10y, -start.2y, start.10y), nrow = 2, ncol = 2, byrow = TRUE)
    B = matrix(data = c(0, (cash0/margin)), nrow = 2, ncol = 1, byrow = TRUE)
    unit = solve(A, B)

    # convexity of each bond
    conv.2y = 2*3 / ( exp(y.2.10[1]) )^2
    conv.10y = 10*11 / ( exp(y.2.10[2]) )^2
    # Total value of the portfolio
    #value = units[1,1]*(start.2y) + units[2,1]*(start.10y)
    #conv.pf = (units[1,1]*(start.2y)/value)*conv.2y + (units[2,1]*(start.10y)/value)*conv.10y
    
    # cash position: $short position - $long position + $capital
    capital = -unit[1,1]*start.2y - unit[2,1]*start.10y + cash0
    
    # interest earned or paid for the cash position
    parameters0 = weekdata0[(length(weekdata0) - 5):length(weekdata0)]

    int.1w =NSS_model(parameters0, week) / 100
    interest = capital * exp(week * (int.1w)) - capital
    
    # a week later
    weekdata1 = as.vector(yields[days[i+1], ])
    if(any( is.na(weekdata1[c(2,10)]) )) {
      weekdata1 = as.vector( yields[days[i+1]+1, ] )
    }
    
    parameters1 = weekdata1[(length(weekdata1) - 5):length(weekdata1)]

    r.9y358d = NSS_model(parameters1, y.9y358d) / 100
    r.1y358d = NSS_model(parameters1, y.1y358d) / 100
    
    end.2y = 100 * exp(-1 * r.1y358d * y.1y358d) # 81.27
    end.10y = 100 * exp(-1 * r.9y358d * y.9y358d) # 31.38
    
    #   Total capital in $
    total =  unit[1,1]*end.2y + unit[2,1]*end.10y + interest + capital
    
    #   Calculate the weekly return
    w.return = (total - cash0) / cash0
    
    # spread return = duration return 
    # -md * start value(=units*start p) * yield change(=shorter y - maturity y)
    p.dv01.2y = -1*(md.2)*(r.1y358d - y.2.10[1])*(unit[1,1]*start.2y)
    p.dv01.10y = -1*(md.10)*(r.9y358d - y.2.10[2])*(unit[2,1]*start.10y)
    spread.return = (p.dv01.2y + p.dv01.10y) / cash0
    
    # convexity return
    # convexity = T(T+1)/(1+maturity y)^2
    # 1/2 * convexity * start value(=units*start p) * (yield change)^2
    p.conv.2y = unit[1,1]*start.2y * 1/2 * conv.2y * (r.1y358d - y.2.10[1])^2
    p.conv.10y = unit[2,1]*start.10y * 1/2 * conv.10y * (r.9y358d - y.2.10[2])^2
    conv.return = (p.conv.2y + p.conv.10y) / cash0
    
    # time return
    # (par*units)/(1+maturity y)^(left period) - start value + interest
    p.time.2y = (100/exp(y.2.10[1] * y.1y358d) - start.2y) * unit[1,1]
    p.time.10y = (100/exp(y.2.10[2] * y.9y358d) - start.10y) * unit[2,1]
    time.return = (p.time.2y + p.time.10y + interest) / cash0
    
    # residual
    residual = w.return - (spread.return + conv.return + time.return)
    
    # test
    delta_y2 = r.1y358d - y.2.10[1]
    delta_y10 = r.9y358d - y.2.10[2]
    
    ##
    result1[i, ] = c(unit[1,1], unit[2,1], end.2y, end.10y, interest, total, w.return, spread.return, conv.return, time.return, residual, delta_y2, delta_y10)

    cash0 = total
  }
  return(result1)
}

# Q1 and Q3
result1 = portfolio(1000000, 0.1)

# plot cumulative return of the portfolio
plot(x = result1[, "wk return"], type = 'l', main = "10% capital")

# Q4
result2 = portfolio(1000000, 0.02)

# plot cumulative return of the portfolio with capital of 10%(left) and 2%(right)
#par(mfrow=c(1,2))
plot(x = result1[, "wk return"], type = 'l', ylim = c(-0.5, 0.5), main = "10% capital")
plot(x = result2[, "wk return"], type = 'l', col = 'blue', ylim = c(-0.5, 0.5), main = "2% capital")

# Q2
return = matrix(nrow = length(days), ncol = 3, byrow = TRUE)
colnames(return) = c("conv $", "conv %", "total %")
# cash0 = 1000000 # okay to initialize as $1M?
total = 0
for(i in c(1:(length(days) - 1))) {
  
  y.9y358d = 10 - 7/365; 
  y.1y358d = 2 - 7/365; 
  week = 7/365; 
  
  weekdata0 = as.vector(yields[days[i], ])
  if(any( is.na(weekdata0[c(2,10)]) )) {
    weekdata0 = as.vector( yields[days[i]+1, ] )
  }
  
  y.2.10 = weekdata0[c(2, 10)] # 2- and 10-year bond yield
  
  start.2y = 100 * exp(-1*y.2.10[1]*2) # 80.98
  start.10y = 100 * exp(-1*y.2.10[2]*10) # 31.21
  md.2 = 2 * exp(-1*y.2.10[1])
  md.10 = 10 * exp(-1*y.2.10[2])
  
  # calculating units of eash bond
  # constant $1M position in the 10-year treasury in terms of face value
  # --> y = 1,000,000 / 100 = 10,000 units
  # 2y md*P(2y, start)x + 10y md*P(10y, start)*10,000 = 0 -> dv01-neutral
  # x = -1*10y md*P(10y, start)*10,000 / ( 2y md*P(2y, start) )
  y = 10000
  x = (-1*md.10*start.10y*10000) / (md.2*start.2y)
  
  # cash position: $short position - $long position + $capital
  #capital = -unit[1,1]*start.2y - units[2,1]*start.10y + cash0
  capital = -x*start.2y - y*start.10y
  cash0 = capital
  
  # interest earned or paid for the cash position
  parameters0 = weekdata0[(length(weekdata0) - 5):length(weekdata0)]

  int.1w =NSS_model(parameters0, week) / 100
  interest = capital * exp(week * (int.1w)) - capital
  
  # interest = capital *((1 + int.1w/(365/7))^(7/365) - 1)
  # interest = capital * exp(7 * int.1w) - capital
  
  # a week later
  weekdata1 = as.vector(yields[days[i+1], ])
  if(any( is.na(weekdata1[c(2,10)]) )) {
    weekdata1 = as.vector( yields[days[i+1]+1, ] )
  }
  
  parameters1 = weekdata1[(length(weekdata1) - 5):length(weekdata1)]

  r.9y358d = NSS_model(parameters1, y.9y358d) / 100
  r.1y358d = NSS_model(parameters1, y.1y358d) / 100
  
  end.2y = 100 * exp(-1 * r.1y358d * y.1y358d) # 81.27
  end.10y = 100 * exp(-1 * r.9y358d * y.9y358d) # 31.38
  
  # convexity of each bond
  conv.2y = 2*3 / ( exp(y.2.10[1]) )^2
  conv.10y = 10*11 / ( exp(y.2.10[2]) )^2
  # convexity return
  # convexity = T(T+1)/(1+maturity y)^2
  # 1/2 * convexity * start value(=units*start p) * (yield change)^2
  p.conv.2y = x*start.2y * 0.5 * conv.2y * (0.001)^2
  p.conv.10y = y*start.10y * 0.5 * conv.10y * (0.001)^2
  conv.return = (p.conv.2y + p.conv.10y) / cash0
  
  #   Total capital 
  #total =  units[1,1]*end.2y + units[2,1]*end.10y + interest + capital
  total =  x*end.2y + y*end.10y + interest + capital
  
  #   4. Calculate the weekly return
  w.return = (total - cash0) / cash0
  
  return[i, ] = c((p.conv.2y+p.conv.10y), conv.return, w.return)

  #cash0 = total
}


#par(mfrow=c(1,2))
plot(x = return[, 2], type = 'l', col = 'red', main = "conv. return")
plot(x = result1[, "conv.return"], type = 'l', main = "conv. return")


plot(x = return[, 3], type = 'l', col = 'blue', main = "total return")


