#1--------------------------
#a)
ret.CAPM = 0.05 + 1.4*0.06
print(ret.CAPM)

#b)
p.now = 35
p.next = p.now*(1+ret.CAPM)
print(p.next)

#c)
div = 2
p.div = p.next - div
print(p.div)

#2--------------------------
#a)
b.a = (0.32-0.02)/(0.2-0.05)
b.d = (0.14-0.035)/(0.2-0.05)
beta = c(b.a,b.d)
names(beta) = c("aggressive", "defensive")
print(c(b.a,b.d))

#b)
ret.a = (0.02+0.32)/2
ret.d = (0.035+0.14)/2
return = c(ret.a,ret.d)
names(return) = c("aggressive", "defensive")
print(c(ret.a,ret.d))

#c)
ret.mkt = 0.5*0.05 + 0.5*0.2
limit.b = c(0,1)
limit.E = c(0.08, ret.mkt)
plot(limit.b,limit.E, type = "l", xlim = c(0,3), ylim = c(0,0.3),
     main = "Security Market Line", col = "green")
rf = 0.08
slope = ret.mkt-0.08
abline(rf,slope, col = "green", lwd = 2)

#d)
points(b.a,ret.a, pch = 19, col = "red")
points(b.d,ret.d, pch = 19, col = "blue")
text(b.a, ret.a, labels = "Aggressive", pos = 3)
text(b.d,ret.d, labels = "Defensive", pos = 1)
a.a = ret.a - (rf+slope*b.a)
a.d = ret.d - (rf+slope*b.d)
a = c(a.a,a.d)
names(a) = c("aggressive", "defensive")
print(a)

#3--------------------------------
#a)
library(readxl)
MSFT = read_excel("lecture8p.xlsx", sheet = "MSFT")
INTC = read_excel("lecture8p.xlsx", sheet = "INTC")
LUV = read_excel("lecture8p.xlsx", sheet = "LUV")
MCD = read_excel("lecture8p.xlsx", sheet = "MCD")
JNJ = read_excel("lecture8p.xlsx", sheet = "JNJ")
FF = read_excel("lecture8p.xlsx", sheet = "F-F_Research_Data_Factors_daily",skip = 4)

library(xts)
FF.xts = xts(FF, order.by = as.Date(as.character(FF$X__1), "%Y%m%d"))
start = MSFT$Date[1]
FF.xts = FF.xts["1989-12-29/"]
FF = data.frame(FF.xts)

MSFT.zoo = read.zoo(MSFT)
INTC.zoo = read.zoo(INTC)
LUV.zoo = read.zoo(LUV)
MCD.zoo = read.zoo(MCD)
JNJ.zoo = read.zoo(JNJ)

MSFT.xts = as.xts(MSFT.zoo)
INTC.xts = as.xts(INTC.zoo)
MCD.xts = as.xts(MCD.zoo)
LUV.xts = as.xts(LUV.zoo)
JNJ.xts = as.xts(JNJ.zoo)

library(quantmod)
ret.MSFT = dailyReturn(MSFT.xts$`Adj Close`)
ret.INTC = dailyReturn(INTC.xts$`Adj Close`)
ret.LUV = dailyReturn(LUV.xts$`Adj Close`)
ret.MCD = dailyReturn(MCD.xts$`Adj Close`)
ret.JNJ = dailyReturn(JNJ.xts$`Adj Close`)

diff.MSFT = (ret.MSFT$daily.returns[-1]-FF$RF/100)
diff.INTC = (ret.INTC$daily.returns[-1]-FF$RF/100)
diff.LUV = (ret.LUV$daily.returns[-1]-FF$RF/100)
diff.MCD = (ret.MCD$daily.returns[-1]-FF$RF/100)
diff.JNJ = (ret.JNJ$daily.returns[-1]-FF$RF/100)

FF.percent = (FF$Mkt.RF/100)

lm.MSFT = lm(diff.MSFT~FF.percent)
lm.INTC = lm(diff.INTC~FF.percent)
lm.LUV = lm(diff.LUV~FF.percent)
lm.MCD = lm(diff.MCD~FF.percent)
lm.JNJ = lm(diff.JNJ~FF.percent)

sum.MSFT = summary(lm.MSFT)
sum.INTC = summary(lm.INTC)
sum.LUV = summary(lm.LUV)
sum.MCD = summary(lm.MCD)
sum.JNJ = summary(lm.JNJ)

beta = c(sum.MSFT$coefficients[2], sum.INTC$coefficients[2],
          sum.LUV$coefficients[2], sum.MCD$coefficients[2], sum.JNJ$coefficients[2])
std.beta = c(sum.MSFT$coefficients[2,2], sum.INTC$coefficients[2,2],
          sum.LUV$coefficients[2,2], sum.MCD$coefficients[2,2], sum.JNJ$coefficients[2,2])
beta.info = data.frame(rbind(beta,std.beta)); names(beta.info) = c("MSFT", "INTC", "LUV", "MCD", "JNJ")
print(beta.info)

#b)
alpha = c(sum.MSFT$coefficients[1], sum.INTC$coefficients[1],
          sum.LUV$coefficients[1], sum.MCD$coefficients[1], sum.JNJ$coefficients[1])
std.alpha = c(sum.MSFT$coefficients[1,2], sum.INTC$coefficients[1,2],
          sum.LUV$coefficients[1,2], sum.MCD$coefficients[1,2], sum.JNJ$coefficients[1,2])
alpha.info = data.frame(rbind(alpha,std.alpha)); names(alpha.info) = c("MSFT", "INTC", "LUV", "MCD", "JNJ")
print(alpha.info)

#c)
name = c("MSFT", "INTC", "LUV", "MCD", "JNJ")

h.ret = which(beta == max(beta))
l.ret = which(beta == min(beta))
print(paste("CAPM: highest is ", name[h.ret]))
print(beta.info[h.ret])
print(paste("CAPM: lowest is ", name[l.ret]))
print(beta.info[l.ret])

mret.MSFT = mean(ret.MSFT)
mret.INTC = mean(ret.INTC)
mret.LUV = mean(ret.LUV)
mret.MCD = mean(ret.MCD)
mret.JNJ = mean(ret.JNJ)

mret = c(mret.MSFT,mret.INTC,mret.LUV,mret.MCD,mret.JNJ)
h.pos = which(mret == max(mret))
l.pos = which(mret == min(mret))
print(paste("Mean: highest is ", name[h.pos]))
print(paste("Mean: lowest is ", name[l.pos]))
