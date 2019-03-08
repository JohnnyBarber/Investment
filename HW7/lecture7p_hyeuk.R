# MGMTMFE400-2 Investments 
# Project for Lectures 6/7: Portfolio allocation
# Hyeuk Jung

library(quantmod)
getSymbols(c('INTC', 'MSFT', 'LUV', 'MCD', 'JNJ'), from = "1989-12-29" ,to = '2018-09-29')
df <- read.csv("/Users/hyeukjung/Desktop/UCLA related/400_Investments/Week 7 & HW by/lecture6p.csv", header = T)
row.names(df) <- as.Date(as.character(df$X), "%Y%m%d")
df$X <- NULL
rf = as.xts(df)
rf = rf["1989-12-29/2018-09-29"]

# Weekly returns
week <- endpoints(INTC, 'weeks', 1)
intc.wr <- INTC$INTC.Adjusted[week] / lag(INTC$INTC.Adjusted[week]) - 1
msft.wr <- MSFT$MSFT.Adjusted[week] / lag(MSFT$MSFT.Adjusted[week]) - 1
luv.wr <- LUV$LUV.Adjusted[week] / lag(LUV$LUV.Adjusted[week]) - 1
mcd.wr <- MCD$MCD.Adjusted[week] / lag(MCD$MCD.Adjusted[week]) - 1
jnj.wr <- JNJ$JNJ.Adjusted[week] / lag(JNJ$JNJ.Adjusted[week]) - 1
rf.wr <- rf$RF[week] / lag(rf$RF[week]) - 1


##### Question 1~3 ----------------------------------------------
# Number of risky assets
n <- 5
# Annualized 
# colnames(an_return) <- c('INTC', 'MSFT', 'LUV', 'MCD', 'JNJ')
an_return <- c(mean(intc.wr[-1]), mean(msft.wr[-1]), mean(luv.wr[-1]), mean(mcd.wr[-1]), mean(jnj.wr[-1]))*52
an_sd <- c(sd(intc.wr[-1]), sd(msft.wr[-1]), sd(luv.wr[-1]), sd(mcd.wr[-1]), sd(jnj.wr[-1]))*sqrt(52)
#an_rf <- mean(rf.wr[is.finite(rf.wr)], na.rm = TRUE)*52 # -0.1645?????????????
an_rf <- mean(rf$RF/100)*365

# Correlation
corr <- cor(cbind(intc.wr[-1], msft.wr[-1], luv.wr[-1], mcd.wr[-1], jnj.wr[-1]))

# Var-Cov matrix
var <- diag(an_sd)%*%corr%*%diag(an_sd) # same result as the cov() function
var <- round(var, 6)

# Calculating weights, returns, and SDs for the given target returns
library(quadprog)
onen <- rep(1, n)
w <- rep(1/n, n) # an example of PF weights
er <- w%*%an_return # an example of PF return
vr <- w%*%var%*%w # an example of PF risk

A <- cbind(onen, an_return)
etarget <- seq(from = 0.00, to = 0.4, by = 0.001)

pf_weight <- matrix(nrow = length(etarget), ncol = n)
colnames(pf_weight) <- c('INTC', 'MSFT', 'LUV', 'MCD', 'JNJ')
pf_r <- matrix(nrow = length(etarget), ncol = 1)
pf_sd <- matrix(nrow = length(etarget), ncol = 1)

for ( i in c(1:length(etarget)) ) {
  pf_weight[i, ] <- solve.QP(2*var, rep(0,n), A, c(1, etarget[i]), meq=2)$solution
  pf_r[i] <- pf_weight[i, ]%*%t(t(an_return))
  pf_sd[i] <- sqrt(t(pf_weight[i, ])%*%var%*%pf_weight[i, ])
}

##### Plot
plot(pf_sd, pf_r, type = 'l', main = 'Mean-Variance Frontier',
     xlab = 'standard deviation', ylab = 'expected return',
     xlim = c(0, 0.6), ylim = c(0, 0.4)) #      xlim = c(0, 0.6), ylim = c(0, 0.4)
grid(NULL,NULL, col = "lightgrey", lty = "dotted")
legend("topright", legend = c("efficient frontier", 'sharpe ratio', 'sharpe ratio'), lty = 1, col = c("red", 'blue', 'green'))

points(an_sd[1], an_return[1], pch = 17, col = 'red')
text(an_sd[1], an_return[1], labels = "INTC", pos = 4, col = 'red')

points(an_sd[2], an_return[2], pch = 17, col = 'blue')
text(an_sd[2], an_return[2], labels = "MSFT", pos = 4, col = 'blue')

points(an_sd[3], an_return[3], pch = 17, col = 'green')
text(an_sd[3], an_return[3], labels = "LUV", pos = 4, col = 'green')

points(an_sd[4], an_return[4], pch = 17, col = 'yellow')
text(an_sd[4], an_return[4], labels = "MCD", pos = 4, col = 'yellow')

points(an_sd[5], an_return[5], pch = 17, col = 'purple')
text(an_sd[5], an_return[5], labels = "JNJ", pos = 4, col = 'purple')

points(sqrt(vr), er, pch = 17)
text(sqrt(vr), er, labels = "Equally-weighted", pos = 4)

min_sigma = min(pf_sd)
pos = which(pf_sd == min_sigma)
best_E = pf_r[pos]
points(min_sigma, best_E, pch = 19, col = 'red')
text(min_sigma, best_E, labels = "global MVP", pos = 2, col = 'red')

# Efficient frontier
pf_r_effi <- pf_r[pf_r >= best_E]
pf_sd_effi <- pf_sd[-(1:(pos-1))]
par(new = T)
plot(pf_sd_effi, pf_r_effi, type = 'l', col = 'red', 
     xlim = c(0, 0.6), ylim = c(0, 0.4), 
     xlab = 'standard deviation', ylab = 'expected return')

# Tangent line
sharpe <- (pf_r - an_rf) / pf_sd
abline(an_rf, max(sharpe), col = 'blue')
max(sharpe)
pf_weight[which(sharpe == max(sharpe)), ]

# Utility (A = 5) --> optimal mix of assets
utility = pf_r - 5/2*pf_sd^2
pos <- which(utility == max(utility))
pf_weight[pos, ]
points(pf_sd[pos], pf_r[pos], pch = 17, col = 'red')
text(pf_sd[pos], pf_r[pos], labels = 'optimal mix', pos = 2, col = 'red')


### Q1: wider efficient frontier (wider return range & lower SDs)
#       -> can earn higher returns while maintaining the same exposure of standard deviation
#       -> diversification effect o (can invest in the PF of lower SDs)
### Q2: higher sharpe ratio (0.8763 vs. 0.6927)
### Q3: Weights (same as the tangent point)
#             INTC       MSFT        LUV        MCD        JNJ 
#       0.07526205 0.25728097 0.11678687 0.17682649 0.37384362 


##### HW 6 ----------------------------------------------------
INTC = INTC["1989-12-29/2018-09-28"]
MSFT = MSFT["1989-12-29/2018-09-28"]

last_INTC = with(INTC, apply.weekly(INTC.Adjusted, last))
last_MSFT = with(MSFT, apply.weekly(MSFT.Adjusted, last))
n = length(last_INTC)
weekly_INTC = last_INTC[-1]/lag(last_INTC)-1
weekly_MSFT = last_MSFT[-1]/lag(last_MSFT)-1

mu_INTC = mean(weekly_INTC)
std_INTC = sd(weekly_INTC)
mu_MSFT = mean(weekly_MSFT)
std_MSFT = sd(weekly_MSFT)

mu_annu_INTC = mu_INTC*52
std_annu_INTC = sqrt(52)*std_INTC
mu_annu_MSFT = mu_MSFT*52
std_annu_MSFT = sqrt(52)*std_MSFT

w = seq(from = 0, to = 1, by = 0.001)
E = w*mu_annu_INTC+(1-w)*mu_annu_MSFT
covariance_r = as.vector(std_annu_INTC*std_annu_MSFT*cor(weekly_INTC,weekly_MSFT))
sigma = sqrt(w^2*std_annu_INTC^2+(1-w)^2*std_annu_MSFT^2+2*w*(1-w)*covariance_r)

par(new = TRUE)
plot(sigma, E, type = "l", xlab = 'standard deviation', ylab = 'expected return',
     xlim = c(0, 0.6), ylim = c(0, 0.4))

min_sigma = min(sigma)
pos = which(sigma == min_sigma)
best_E = E[pos]
w_best_INTC = (best_E-mu_annu_MSFT)/(mu_annu_INTC-mu_annu_MSFT)
points(min_sigma, best_E, pch = 19)
text(min_sigma, best_E, labels = "global minimum-variance portfolio", pos = 4)
w_effi = seq(from = 0, to = w_best_INTC, by = 0.001)
E_effi = w_effi*mu_annu_INTC+(1-w_effi)*mu_annu_MSFT
sigma_effi = sqrt(w_effi^2*std_annu_INTC^2+(1-w_effi)^2*std_annu_MSFT^2+2*w_effi*(1-w_effi)*covariance_r)
lines(sigma_effi, E_effi,type = "l", col = "red")
#curve between the "global minimum-variance portfolio" and the "MSFT"
#is the efficient frontier (red line)

# Tangent line
sharpe_2 <- (E - an_rf) / sigma
abline(an_rf, max(sharpe_2), col="green")
max(sharpe_2)

