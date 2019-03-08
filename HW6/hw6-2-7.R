#1-----------------------------------------
library(quantmod)
getSymbols("INTC", src = "yahoo", from = "1989-12-29")
getSymbols("MSFT", src = "yahoo", from = "1989-12-29")

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

#2--------------------------------------------
A = 4
Rf = 0.01
w_INTC = (mu_annu_INTC-Rf)/(A*std_annu_INTC^2)
w_MSFT = (mu_annu_MSFT-Rf)/(A*std_annu_MSFT^2)

#3--------------------------------------------
U_INTC = w_INTC*(mu_annu_INTC-Rf)+Rf-A/2*w_INTC^2*std_annu_INTC^2
U_MSFT = w_MSFT*(mu_annu_MSFT-Rf)+Rf-A/2*w_MSFT^2*std_annu_MSFT^2

#4--------------------------------------------
w = seq(from = 0, to = 1, by = 0.001)
E = w*mu_annu_INTC+(1-w)*mu_annu_MSFT
covariance_r = as.vector(std_annu_INTC*std_annu_MSFT*cor(weekly_INTC,weekly_MSFT))
sigma = sqrt(w^2*std_annu_INTC^2+(1-w)^2*std_annu_MSFT^2+2*w*(1-w)*covariance_r)
plot(sigma, E, type = "l", main = "mean-variance frontier",
     xlab = "standard deviation", ylab = "expected return",
     xlim = c(0.27,0.365), ylim = c(0.215,0.24))
grid(NULL,NULL, col = "lightgrey", lty = "dotted")
points(std_annu_MSFT,mu_annu_MSFT, pch = 19)
points(std_annu_INTC,mu_annu_INTC, pch = 19)
text(std_annu_MSFT,mu_annu_MSFT,labels = "MSFT",pos = 4)
text(std_annu_INTC,mu_annu_INTC,labels = "INTC",pos = 3)

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
legend("topright", legend = "efficient frontier", lty = 1, col = "red")
#curve between the "global minimum-variance portfolio" and the "MSFT"
#is the efficient frontier (red line)
