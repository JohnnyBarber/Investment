#read in data
data = read.csv("p5-sp500.csv")

#transform data
library("xts")
data = transform(data, caldt = as.Date(as.character(caldt), "%Y%m%d"))
data = xts(data$vwretd, order.by = data$caldt)
names(data) = "vwretd"

#Arithmetic---------------------------------------------------------------------
##day------------------------------
a_day = mean(data$vwretd)
a_day_annu = mean(data$vwretd)*365

##month------------------------------
a_month = 0
n_month = (2018-1972)*12
for (i in 1:n_month)
{
  month = as.vector(last(first(data, paste(i, "month", sep = " ")), "1 month"))
  a_month = a_month + (prod(1+month)-1)
}
a_month = a_month/n_month
a_month_annu = a_month*12

##year------------------------------
a_year = 0
n_year = (2018-1972)
for (i in 1:n_year)
{
  year = as.vector(last(first(data, paste(i, "year", sep = " ")), "1 year"))
  a_year = a_year + (prod(1+year)-1)
}
a_year = a_year/n_year
a_year_annu = a_year

##5year------------------------------
a_5year = 0
n_5year = (2017-1972)/5
for (i in 1:n_5year)
{
  year5 = as.vector(last(first(data, paste(i*5, "year", sep = " ")), "5 year"))
  a_5year = a_5year + (prod(1+year5)-1)
}
a_5year = a_5year/n_5year
a_5year_annu = a_5year/5

##non-annu and annu------------------------------
non_annualized = c(a_day, a_month, a_year, a_5year)
annualized = c(a_day_annu, a_month_annu, a_year_annu, a_5year_annu)

#Geometric---------------------------------------------------------------------
##day------------------------------
n = length(data$vwretd)
vwretd = as.vector(data$vwretd)
for (i in 1:n)
{
  day = prod(1+vwretd)
}
g_day = g_day^(1/n)-1
g_day_annu = (1+g_day)^365-1

##month------------------------------
n_month = (2018-1972)*12
g_month = day^(1/n_month)-1
g_month_annu = (1+g_month)^(12)-1

##year------------------------------
n_year = 2018-1972
g_year = day^(1/n_year)-1
g_year_annu = g_year

##5year------------------------------
n_5year = (2017-1972)/5
g_5year = day^(1/n_5year)-1
g_5year_annu = (1+g_5year)^(1/5)-1

##non-annu and annu------------------------------
non_annualized_geo = c(g_day, g_month, g_year, g_5year)
annualized_geo = c(g_day_annu, g_month_annu, g_year_annu, g_5year_annu)