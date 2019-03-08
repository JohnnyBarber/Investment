### MGMTMFE400-2 Problems for Lecture 5
# Hyeuk Jung (Cohort 2)

library(quantmod)
getSymbols(c('INTC', 'MSFT'), from = "1989-12-29" ,to = '2018-09-29') # get apple symbol from yahoo finance (default)

##### Question 1 
# Get dividends data and merge data into xts object
# Calculate weekly simple return (what does simple return mean..?)
intel.week <- endpoints(INTC, 'weeks', 1)
intel.wk.r <- INTC$INTC.Adjusted[intel.week] / lag(INTC$INTC.Adjusted[intel.week]) - 1
ms.week <- endpoints(MSFT, 'weeks', 1)
ms.wk.r <- MSFT$MSFT.Adjusted[ms.week] / lag(MSFT$MSFT.Adjusted[ms.week]) - 1

# Calculate the mean and SD of the returns (mean, SD)
m.sd <- matrix(c(mean(intel.wk.r[-1]), sd(intel.wk.r[-1]), 
                 mean(ms.wk.r[-1]), sd(ms.wk.r[-1])), ncol = 2, byrow = T)

# Annualize the mean and volatility
m.sd_an <- cbind(m.sd[,1]*52, m.sd[,2]*sqrt(52))


##### Question 2
#Optimal PF weight = E(R)-Rf / A*var(R) = equity premium / A*var(R)
rf <- 0.01 # Risk-free rate
A <- 4 # Risk aversion coefficient
weight_intel <- (m.sd_an[1,1] - rf)/(A*(m.sd_an[1,2])^2) #Intel
weight_ms <- (m.sd_an[2,1] - rf)/(A*(m.sd_an[2,2])^2) #MS


##### Question 3
pf_intel_r <- rf*(1-weight_intel) + m.sd_an[1,1]*weight_intel # Rf and Intel: 4.19%
#pf_intel_sd <- 
pf_ms_r <- rf*(1-weight_ms) + m.sd_an[2,1]*weight_ms # Rf and MS: 8.09%
# Choose MS as we can expect higher return given the same risk-aversion coefficient
# Need to calculate pf's SD? Covariance..?
#corr(cbind(rep(rf, length()), ))
u_intel <- weight_intel*(m.sd_an[1,1]-rf) + rf - A/2*weight_intel^2*m.sd_an[1,2]^2
u_ms <- weight_ms*(m.sd_an[2,1]-rf) + rf - A/2*weight_ms^2*m.sd_an[2,2]^2


##### Question 4
#Construct the mean-variance frontier for the Intel-Microsoft combination. 
#Indicate the minimum-variance portfolio and the efficient frontier.
#The efficient frontier: a set of expected returns-risks that you would want to consider investing in
weight <- seq(0, 1, by = 0.01)
corr <- cor(intel.wk.r[-1], ms.wk.r[-1])
return <- weight*m.sd_an[1,1] + (1-weight)*m.sd_an[2,1] # w*intel + (1-w)*ms
sd <- sqrt( weight^2*m.sd_an[1,2]^2 + (1-weight)^2*m.sd_an[2,2]^2 + 2*weight*(1-weight)*corr*m.sd_an[1,2]*m.sd_an[2,2] )

index <- which(sd == min(sd)) # index of the min SD
min.var.pf <- c(return[index], min(sd))

# plot the efficient frontier
plot(x = sd, y = return, type = 'l', main = "mean-variance frontier",
     xlab = "standard deviation", ylab = "expected return", ylim = c(0.220, 0.24))

grid(NULL,NULL, col = "lightgrey", lty = "dotted")
points(min(sd), return[index], pch = 19)
text(min(sd), return[index], labels = "min-variance portfolio",pos = 4)
#points(std_annu_INTC,mu_annu_INTC, pch = 19)
#text(std_annu_MSFT,mu_annu_MSFT,labels = "MSFT",pos = 4)
#text(std_annu_INTC,mu_annu_INTC,labels = "INTC",pos = 3)

# get the global min. var. pf

