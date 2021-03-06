# MGMTMFE400-2 Investments 
# Problems for Lectures 8
# Hyeuk Jung


##### Q1 ----------------------------------------
capm <- function(beta, rf, rm.rf, price = 0, div = 0) {
  
  if(div == 0 | price == 0) {
    # E(R) - Rf = beta(Rm - Rf) --> risk-adjusted expected return = E(R) - Rf
    expected.r <- rf+ beta*rm.rf
  }
  else if(div != 0 && price != 0) {
    # E(R) = Rf + beta(Rm - Rf) + dividend yield
    expected.r <- rf + beta*rm.rf + div/price
  }
  expected.price <- price*(1 + expected.r) # CHECK NEEDED (Rf should be added or not?)
  
  data.frame(
    expected.r = expected.r,
    expected.price = expected.price
  )
}

# 1. (a), (b)
beta = 1.4; rf = 0.05; rm.rf = 0.06; price = 35
q1_ab <- capm(beta, rf, rm.rf, price) # 0.134 / 39.69

# 1. (c) dividend $2 
q1_c <- q1_ab$expected.price - 2
#q1_c <- capm(beta, rf, rm.rf, price, div = 2) # should be 37.69, but getting 41.69


##### Q2 ----------------------------------------
# 2. (a)
# Aggressive
# 0.02 - rf = beta(0.05 - rf)
# 0.32 - rf = beta(0.2 - rf)
# (0.02-rf)/(0.05-rf) = (0.32-rf)/(0.2-rf) -> rf = 0.08 -> beta = 2

# Defensive
# 0.035 - rf = beta(0.05 - rf)
# 0.14 - rf = beta(0.2 - rf)
# (0.035-rf)/(0.05-rf) = (0.14-rf)/(0.2-rf) -> rf = 0.067 -> 0.08 -> beta = 1.9023 -> 0.7
q2_a <- matrix(c(0.08, 2, 0.08, 0.7), nrow = 2, ncol = 2, byrow = T, 
               dimnames = list(c("Aggressive", "Defensive"), c("Rf", "Beta")))


# 2. (b)
# Aggressive
agg_rf = 0.08; agg_beta = 2
agg.r <- (0.32+0.02) / 2

# Defensive
def_rf = 0.067; def_beta = 0.7 # 1.90
def.r <- (0.14 + 0.035) / 2

q2_b <- matrix(c(agg.r, def.r), nrow = 1, ncol = 2, byrow = T, 
               dimnames = list(c("Expected return"), c("Aggressive", "Defensive")))


# 2. (c)
RF = 0.08
expected.rm = (0.2 + 0.05)/2
slope <- (expected.rm - RF)/1

x = c(0, 1)
y = c(RF, expected.rm)
plot(x, y, xlim = c(0, 2.2), pch = 20, ylim = c(0.05, 0.2), xlab = "Beta", ylab = "Expected return", main = "Security Market Lines")
abline(a = RF, b = slope, col = "blue")
text(1, expected.rm, labels = paste("slope = ", slope), pos = 4, col = "blue")


# 2. (d)
# Aggressive
points(agg_beta, agg.r, pch = 20, col = "red")
text(agg_beta, agg.r, labels = "Aggressive", pos = 3, cex = .7, col = "red")
agg.a = agg.r - (slope*agg_beta + RF)# 0.16

# Defensive
points(def_beta, def.r, pch = 20, col = "red")
text(def_beta, def.r, labels = "Defensive", pos = 1, cex = .7, col = "red")
def.a = def.r - (slope*def_beta + RF) # 0.152


q2_d <- matrix(c(agg.a, def.a), nrow = 1, ncol = 2, byrow = T, 
               dimnames = list(c("Alpha"), c("Aggressive", "Defensive")))


##### Q3 ----------------------------------------
library(magrittr)
library(xts)
library(data.table)
library(readxl)

# Load Data
msft <- read_xlsx("lecture8p.xlsx", sheet = "MSFT") %>% as.data.table; gc()
intc <- read_xlsx("lecture8p.xlsx", sheet = "INTC") %>% as.data.table; gc()
luv	 <- read_xlsx("lecture8p.xlsx", sheet = "LUV")  %>% as.data.table; gc()
mcd	 <- read_xlsx("lecture8p.xlsx", sheet = "MCD")  %>% as.data.table; gc()
jnj	 <- read_xlsx("lecture8p.xlsx", sheet = "JNJ")  %>% as.data.table; gc()
rf <- read_xlsx("lecture8p.xlsx", sheet = "F-F_Research_Data_Factors_daily", skip = 4) %>% as.data.table; gc()

# change column name and delete unused columns in rf
names(rf)[1] <- "Date"
rf[, names(rf)[3:4] := list(NULL, NULL)]

# convert to Date
msft[, Date := as.Date(as.character(Date))]
intc[, Date := as.Date(as.character(Date))]
luv[, Date := as.Date(as.character(Date))]
mcd[, Date := as.Date(as.character(Date))]
jnj[, Date := as.Date(as.character(Date))]
rf[, Date := as.Date(as.character(Date), format = "%Y%m%d")]

colnames(msft)[6] <- "msft" 
colnames(intc)[6]<- "intc"
colnames(luv)[6]<- "luv"	
colnames(mcd)[6]<- "mcd"	
colnames(jnj)[6]<- "jnj"	

msft <- msft[, .(Date, msft)]
intc <- intc[, .(Date, intc)]
luv	<- luv[, .(Date, luv)]
mcd	<- mcd[, .(Date, mcd)]
jnj	<- jnj[, .(Date, jnj)]

setkey(msft, Date)
setkey(intc, Date)
setkey(luv, Date)
setkey(mcd, Date)
setkey(jnj, Date)
setkey(rf, Date)

# merge data
data <-rf[msft[intc[luv[mcd[jnj]]]]]

# RF and Mkt-RF: convert to weekly returns
# riskless rate and market premium
#with(data, data[, RF := ((1 + RF/100)^5 - 1)])
#with(data, data[, `Mkt-RF` := ((1 + `Mkt-RF`/100)^5 - 1)])

# calculate daily reutrns
days <- endpoints(data[["Date"]], on = "days")
data <- data[days, ]
data[, msft := msft / shift(msft) - 1]
data[, intc := intc / shift(intc) - 1]
data[, luv := luv / shift(luv) - 1]
data[, mcd := mcd / shift(mcd) - 1]
data[, jnj := jnj / shift(jnj) - 1]
data[, RF := RF/100]
data[, `Mkt-RF` := `Mkt-RF`/100]
data <- data[-1,]

# weekly excess return of each stock
data$ex.msft <- data$msft - data$RF
data$ex.intc <- data$intc - data$RF
data$ex.luv <- data$luv - data$RF
data$ex.mcd <- data$mcd - data$RF
data$ex.jnj <- data$jnj - data$RF


# 3. (a), (b) beta and alpha(intercept) calculation + standard deviations
out_msft <- summary(lm(data$ex.msft ~ data$`Mkt-RF`), digits = 4)
reg_msft <- rbind(out_msft$coef[, 1:2], c(out_msft$sigma, NA))
rownames(reg_msft) <- c("msft_alpha", "msft_beta", "msft_idiosyncratic")

out_intc <- summary(lm(data$ex.intc ~ data$`Mkt-RF`), digits = 4)
reg_intc <- rbind(out_intc$coef[, 1:2], c(out_intc$sigma, NA))
rownames(reg_intc) <- c("intc_alpha", "intc_beta", "intc_idiosyncratic")

out_luv <- summary(lm(data$ex.luv ~ data$`Mkt-RF`), digits = 4)
reg_luv <- rbind(out_luv$coef[, 1:2], c(out_luv$sigma, NA))
rownames(reg_luv) <- c("luv_alpha", "luv_beta", "luv_beta")

out_mcd <- summary(lm(data$ex.mcd ~ data$`Mkt-RF`), digits = 4)
reg_mcd <- rbind(out_mcd$coef[, 1:2], c(out_mcd$sigma, NA))
rownames(reg_mcd) <- c("mcd_alpha", "mcd_beta", "mcd_idiosyncratic")

out_jnj <- summary(lm(data$ex.jnj ~ data$`Mkt-RF`), digits = 4)
reg_jnj <- rbind(out_jnj$coef[, 1:2], c(out_jnj$sigma, NA))
rownames(reg_jnj) <- c("jnj_alpha", "jnj_beta", "jnj_idiosyncratic")

q3_ab <- rbind(reg_msft, reg_intc, reg_luv, reg_mcd, reg_jnj)

# 3. (c) expected returns from sample avg vs. CAPM / lowest & highest
# CAPM: E(R) = Rf + beta(E(Rm) - Rf)
beta <- rbind(out_msft$coef[2, 1], out_intc$coef[2, 1], out_luv$coef[2, 1], out_mcd$coef[2, 1], out_jnj$coef[2, 1]) 
CAPM <- capm(beta = beta, rf = mean(data$RF), rm.rf = mean(data$`Mkt-RF`))[, 1]

# Sample: average of each stock's weekly return
sample_avg <- as.data.frame(apply(data[, c(4:8)], MARGIN = 2, FUN = mean))

q3_c <- cbind(CAPM, sample_avg) # for Rf, should we use long-term average Rf
rownames(q3_c) <- c("msft", "intc", "luv", "mcd", "jnj")
colnames(q3_c) <- c("CAPM", "Sample")
#row.names(q3_c[which(q3_c$CAPM == max(q3_c$CAPM)), ]) # highest expected return under CAPM
#row.names(q3_c[which(q3_c$CAPM == min(q3_c$CAPM)), ]) # lowest expected return under CAPM

#<- function(beta, rf, rm.rf, price = 0, div = 0) {

##### Results -----------------------------------
q1_ab # expected return = 0.134, expected price = $39.69
q1_c # expected return = 0.1911429, expected price = $37.69 --> not sure (price: before dividend is distributed)

q2_a
q2_b
#q2_c: plot
q2_d

q3_ab
q3_c
row.names(q3_c[which(q3_c$CAPM == max(q3_c$CAPM)), ]) # highest expected return under CAPM
row.names(q3_c[which(q3_c$CAPM == min(q3_c$CAPM)), ]) # lowest expected return under CAPM
row.names(q3_c[which(q3_c$Sample == max(q3_c$Sample)), ]) # highest expected return under Sample
row.names(q3_c[which(q3_c$Sample == min(q3_c$Sample)), ]) # lowest expected return under Sample

