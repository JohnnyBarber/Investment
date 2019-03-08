library("readxl")
library("xts")
library("quadprog")
library("ggplot2")

companies = list(c("msft","intc","luv","mcd","jnj"))
returns = list()
result = list()
weekly_returns = list()
return_list = list()

# calculate annualized risk free rate
risk_free = as.data.frame(read_excel("./lecture6p.xlsx",sheet = 6,skip = 4))
risk_free$X__1 = as.Date(as.character(risk_free$X__1),"%Y%m%d")
rownames(risk_free) = risk_free$X__1
risk_free$X__1 = NULL
risk_free = as.xts(risk_free)
rf = risk_free["1989-12-29/2018-09-28"]
week_end = endpoints(rf,"weeks",k=1)[-1]
weekly_rf = vector()
for (i in 1:(length(week_end) - 1)){
  weekly_rf[i] = sum(rf[(week_end[i] + 1):week_end[i+1],4])
}
annualized_rf = mean(weekly_rf) * 52 /100


# returns of 5 stocks
for (i in c(1:5)){
  a = as.data.frame(read_excel("./lecture6p.xlsx",sheet = i))
  a$Date = as.Date(a$Date)
  rownames(a) = a$Date
  a$Date = NULL
  returns[[i]] = as.xts(a)
}
names(returns) = c("msft","intc","luv","mcd","jnj")


# annualized weekly return of 5 stocks
myfunc = function(data){
  week_end = endpoints(data,"weeks",k=1)[-1]
  weekly_return = vector()
  for (i in 1:(length(week_end) - 1)){
    weekly_return[i] = data[[week_end[i+1],5]] / data[[week_end[i],5]] - 1
  }
  mean = mean(weekly_return)
  sd = sd(weekly_return)
  annualized_mean = (1 + mean)^52 -1
  annualized_sd = sd * sqrt(52)
  return_list[[1]] = weekly_return
  return_list[[2]] = c(mean,sd,annualized_mean,annualized_sd)
  return(return_list)
}

for (i in c("msft","intc","luv","mcd","jnj")){
  b = myfunc(returns[[i]])
  weekly_returns[[i]] = b[[1]]
  result[[i]] = b[[2]]
}
n = 5
err = vector()
sigmarr = vector()
corr = vector()
covariance = vector()
for (i in c(1:n)){
  err[i] = result[[i]][3]
  sigmarr[i] = result[[i]][4]  
}


for (i in c(1:n)){
  for (j in c(1:n)){
    corr[(i - 1) * n + j] = cor(weekly_returns[[i]],weekly_returns[[j]])
  }
}
corr = matrix(corr,n,n)

vrr = diag(sigmarr) %*% corr %*% diag(sigmarr)
vrr = round(vrr,6)
inv_cov = solve(vrr)

df = data.frame("er" = err, "sd" = sigmarr)

ef_params = function(returns,inv_cov){
  
  i = matrix(1,nrow = length(returns))
  alpha = t(i) %*% inv_cov %*% i
  beta = t(i) %*% inv_cov %*% returns
  gamma = t(returns) %*% inv_cov %*% returns
  delta = alpha * gamma - beta * beta
  return_list = list(alpha = as.numeric(alpha),
  beta = as.numeric(beta),
  gamma = as.numeric(gamma),
  delta = as.numeric(delta))
  return(return_list)
}

parameters = ef_params(err, inv_cov)

ef_values = function(x, parameter,upper=T){
  alpha = parameter$alpha
  beta = parameter$beta
  gamma = parameter$gamma
  delta = parameter$delta
  
  if (upper){
    returns = beta/alpha + sqrt((beta/alpha)^2  - (gamma - delta * x ^ 2) / (alpha))
  }
  else{
    returns = beta/alpha - sqrt((beta/alpha)^2  - (gamma - delta * x ^ 2) / (alpha))
  }
  return(returns)
}

f_table <- melt(df)[, .(er = mean(value),
                        sd = sd(value)), by = variable]

# plot the values
ggplot(df,aes(x = sd, y = er)) +
  # add the stocks
  geom_point(size = 4, color = "red", shape = 18) +
  # add the upper efficient frontier
  stat_function(fun = ef_values, args = list(parameter = parameters, upper = T), n = 10000,
                color = "red", size = 1) +
  # add the lower "efficient" frontier
  stat_function(fun = ef_values, args = list(parameter = parameters, upper = F), n = 10000,
                color = "blue", size = 1) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Efficient Frontier with Short-Selling") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(df$er) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(df$sd) * 1.2))
