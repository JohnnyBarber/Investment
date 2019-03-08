library(readxl)
library(quantmod)
getSymbols(Symbols = c("MSFT","INTC","LUV","MCD","JNJ"), src = "yahoo",
           from = "1989-12-29", to = "2018-09-28")
research = read_excel("lecture6p.xlsx", sheet = "F-F_Research_Data_Factors_daily")
research = data.frame(research)
research$X__1 = as.Date(as.character(research$X__1),"%Y%m%d")
RF = xts(research$X__1, order.by = research$X__1)
RF = merge(RF,research$RF)
RF = RF["1989-12-29/2018-09-28"]
RF$RF = NULL
colnames(RF$research.RF) = "RF"

#previous hw data---------------------------------------------------
last_INTC = with(INTC, apply.weekly(INTC.Adjusted, last))
last_MSFT = with(MSFT, apply.weekly(MSFT.Adjusted, last))
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

A_p = 4
Rf_p = 0.01
w_INTC = (mu_annu_INTC-Rf_p)/(A_p*std_annu_INTC^2)
w_MSFT = (mu_annu_MSFT-Rf_p)/(A_p*std_annu_MSFT^2)

w_p = seq(from = 0, to = 1, by = 0.001)
E_p = w_p*mu_annu_INTC+(1-w_p)*mu_annu_MSFT
covariance_r_p = as.vector(std_annu_INTC*std_annu_MSFT*cor(weekly_INTC,weekly_MSFT))
sigma_p = sqrt(w_p^2*std_annu_INTC^2+(1-w_p)^2*std_annu_MSFT^2+2*w_p*(1-w_p)*covariance_r_p)
plot(sigma_p, E_p, type = "l", col = "red")
points(std_annu_MSFT,mu_annu_MSFT, pch = 19)
points(std_annu_INTC,mu_annu_INTC, pch = 19)
text(std_annu_MSFT,mu_annu_MSFT,labels = "MSFT",pos = 4)
text(std_annu_INTC,mu_annu_INTC,labels = "INTC",pos = 3)

min_sigma_p = min(sigma_p)
pos_p = which(sigma_p == min_sigma_p)
best_E_p = E_p[pos_p]
w_best_INTC = (best_E_p-mu_annu_MSFT)/(mu_annu_INTC-mu_annu_MSFT)
w_effi_p = seq(from = 0, to = w_best_INTC, by = 0.001)
E_effi_p = w_effi_p*mu_annu_INTC+(1-w_effi_p)*mu_annu_MSFT
sigma_effi_p = sqrt(w_effi_p^2*std_annu_INTC^2+(1-w_effi_p)^2*std_annu_MSFT^2+2*w_effi_p*(1-w_effi_p)*covariance_r_p)

#hw 7 data-----------------------------------------------
last_INTC = with(INTC, apply.weekly(INTC.Adjusted, last))
last_MSFT = with(MSFT, apply.weekly(MSFT.Adjusted, last))
last_LUV = with(LUV, apply.weekly(LUV.Adjusted, last))
last_MCD = with(MCD, apply.weekly(MCD.Adjusted, last))
last_JNJ = with(JNJ, apply.weekly(JNJ.Adjusted, last))

weekly_INTC = last_INTC[-1]/lag(last_INTC)-1
weekly_MSFT = last_MSFT[-1]/lag(last_MSFT)-1
weekly_LUV = last_LUV[-1]/lag(last_LUV)-1
weekly_MCD = last_MCD[-1]/lag(last_MCD)-1
weekly_JNJ = last_JNJ[-1]/lag(last_JNJ)-1

w_scale = seq(from = 0, to = 1, by = 0.001)

weekly = cbind(weekly_INTC,weekly_MSFT,weekly_LUV,weekly_MCD,weekly_JNJ)
weekly = weekly[!is.na(weekly$INTC.Adjusted),]
mu = sapply(weekly, mean)
std = sapply(weekly, sd)

mu_annu = mu*52
std_annu = sqrt(52)*std

week = length(apply.weekly(RF, mean))
RF_annu = sum(RF)/week*52/100

n = 5

cov_annu = diag(std_annu)%*%cor(weekly)%*%diag(std_annu)
cov_annu = round(cov_annu,6)
one = rep(1,n)
library(quadprog)
A = cbind(one, mu_annu)
target = seq(0.05,0.3,0.001)
w = matrix(nrow = length(target), ncol = n)
E = matrix(nrow = length(target), ncol = 1)
V = matrix(nrow = length(target), ncol = 1)
for (i in 1:length(target))
{
  w[i,] = solve.QP(2*cov_annu,rep(0,n),A,c(1,target[i]),meq = 2)$solution
  E[i,] = w[i,]%*%mu_annu
  V[i,] = w[i,]%*%cov_annu%*%w[i,]
}
sigma = sqrt(V)
min_sigma = min(sigma)
pos = which(sigma == min_sigma)
best_E = E[pos,]
effi_E = E[pos:length(E),]
effi_sigma = sigma[pos:length(E),]

sharpe_2 = (E_p-RF_annu)/sigma_p
sharpe_5 = (E-RF_annu)/sigma

U_5 = E - (5/2)*V
pos_T5 = which(U_5 == max(U_5))
U_2 = E_p - (5/2)*sigma_p
pos_T2 = which(U_2 == max(U_2))

#plot-------------------------------------------------------------------
plot(sigma, E, type = "l", main = "mean-variance frontier",
     xlab = "standard deviation", ylab = "expected return",
     xlim = c(0,0.4), ylim = c(0,0.3))
grid(NULL,NULL, col = "lightgrey", lty = "dotted")
lines(sigma_p, E_p, type = "l")
lines(sigma_effi_p, E_effi_p,type = "l", col = "red")
lines(effi_sigma,effi_E, type = "l", col = "red")
abline(RF_annu, max(sharpe_2), col = "blue")
abline(RF_annu, max(sharpe_5), col = "green")
points(std_annu_MSFT,mu_annu_MSFT, pch = 19)
points(std_annu_INTC,mu_annu_INTC, pch = 19)
points(std_annu[3],mu_annu[3], pch = 19)
points(std_annu[4],mu_annu[4], pch = 19)
points(std_annu[5],mu_annu[5], pch = 19)
points(min_sigma_p, best_E_p, pch = 19)
points(min_sigma, best_E, pch = 19)
points(sigma[pos_T5], E[pos_T5], pch = 10, col = "red")
points(sigma_p[pos_T2], E_p[pos_T2], pch = 10, col = "red")
text(std_annu_MSFT,mu_annu_MSFT,labels = "MSFT",pos = 4)
text(std_annu_INTC,mu_annu_INTC,labels = "INTC",pos = 4)
text(std_annu[3],mu_annu[3],labels = "LUV",pos = 4)
text(std_annu[4],mu_annu[4],labels = "MCD",pos = 4)
text(std_annu[5],mu_annu[5],labels = "JNJ",pos = 4)
text(min_sigma_p, best_E_p, labels = "global minimum-variance portfolio", pos = 4)
text(min_sigma, best_E, labels = "global minimum-variance portfolio", pos = 4)
text(sigma[pos_T5], E[pos_T5], labels = "tangent porfolio", pos = 2)
text(sigma_p[pos_T2], E_p[pos_T2], labels = "tangent porfolio", pos = 2)
legend("topleft", legend = c("efficient frontier",
                             "capital allocation line of INTL-MSFT case",
                             "capital allocation line of full set of stocks"),
       lty = 1, col = c("red","blue","green"), 
       bty = "n", x.intersp = 0.5, y.intersp = 0.5)

