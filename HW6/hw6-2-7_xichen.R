library(quantmod)
aa = new.env()
getSymbols(c('INTC','MSFT'), env = aa, src='yahoo', from = "1989-12-29", to="2018-09-28")

intel=aa$INTC
micro=aa$MSFT

# Problem 1
# Intel 
# calculate the mean of the returns
intel_mean=mean(to.weekly(intel$INTC.Adjusted))
# calculate the standard deviation of the returns
intel_std=sd(to.weekly(intel$INTC.Adjusted))
# annualize the mean of the returns
a_intel_mean=mean(to.weekly(intel$INTC.Adjusted))*52
# annualize the standard deviation of the returns
a_intel_std=intel_std*sqrt(52)

# Microsoft
# calculate the mean of the returns
micro_mean=mean(to.weekly(micro$MSFT.Adjusted))
# calculate the standard deviation of the returns
micro_std=sd(to.weekly(micro$MSFT.Adjusted))
# annualize the mean of the returns
a_micro_mean=mean(to.weekly(micro$MSFT.Adjusted))*52
# annualize the standard deviation of the returns
a_micro_std=micro_std*sqrt(52)

# Problem 2
# Find the maximum utility:
rf=0.01
a = 4

# The weight of Intel that maximizes utility
w_intel=(a_intel_mean-rf)/a/a_intel_std
# Borrow 1.6 total captal of risk-free asset and invest in 2.6 Intel stocks

# The weight of Microsoft that maximizes utility
w_micro=(a_micro_mean-rf)/a/a_micro_std
# Borrow 1.1 total captal of risk-free asset and invest in 2.1 Intel stocks

# Problem 3
u_intel=w_intel*(a_intel_mean-rf)+rf-a/2*(w_intel^2)*a_intel_std
u_micro=w_micro*(a_micro_mean-rf)+rf-a/2*(w_micro^2)*a_micro_std
# I would allocate to risk-free asset and Microsoft, because the utility is larger.

# Problem 4
# calculate the covariance
cov_xy=cov(intel$INTC.Adjusted, micro$MSFT.Adjusted)

# create 10000 portfolio weights 
x_weights = seq(from = 0, to = 1, length.out = 10000)

# create a data.table that contains the weights for the two assets
two_assets = data.table(wx = x_weights, wy = 1 - x_weights)

# calculate the expected returns and standard deviations for the 1000 possible portfolios
two_assets[, ':=' (er_p = wx * a_intel_mean + wy * a_micro_mean, sd_p = sqrt(wx^2 * a_intel_std^2 + wy^2 * a_micro_std^2 + 2 * wx * (1 - wx) * cov_xy))]

two_assets

# lastly plot the values
ggplot() +
  geom_point(data = two_assets, aes(x = sd_p, y = er_p, color = wx)) +
  geom_point(data = data.table(sd = c(a_intel_std, a_micro_std), mean = c(a_intel_mean, a_micro_mean)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Possible Portfolios with Two Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(two_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(two_assets$sd_p) * 1.2)) +
  scale_color_continuous(name = expression(omega[x]), labels = percent)


