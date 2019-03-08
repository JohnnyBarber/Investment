mysoln <- list(student = c("Huanyu Liu", "Jiaqi Li", "Hyeuk", ""))

payment_func = function(present_v, r, n, step = 1){
  a1 = 1 / (1 + r) ^ step
  q = a1
  return(present_v * (1 - q) / a1 / (1 - q^(n/step)))
}

#1

#(a)

invest = 10000
year = 3
ear = 0.06
total_asset_a = invest * (1 + ear) ^ 3
total_asset_a

#(b)

apr_q = 0.06
total_asset_b = invest * (1 + apr_q/4) ^ 12
total_asset_b

#(c)

apr_m = 0.06
total_asset_c = invest * (1 + apr_m/12) ^ 36
total_asset_c

#2

#(a)

payment = 500000 * 0.1
pv = 0
annual_rate = 0.05
for (i in 1:10){
  pv = pv + payment / (1 + annual_rate) ^ (i * 3)
}
pv

#(b)

pv_b = 700000
n = 30
step = 3
payment_b = payment_func(pv_b,annual_rate,n,step)
payment_b

#3

#(a)

year = 35
r = 0.04
fv = 0
save = 200000 * 0.3
for (i in 0:34){
  fv = fv + save * (1 + r) ^ i
}
fv

#(b)

retirement_year = 20
pv = fv

a1 = 1 / (1 + r)
q = a1
consume = pv * (1 - q) / a1 / (1 - q^retirement_year)
consume

#4

#(a)

apr_m = 0.07
ear = (1 + apr_m / 12) ^ 12 - 1
ear