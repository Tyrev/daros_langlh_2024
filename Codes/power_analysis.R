############################
# Power Analysis for Linear Model
# https://www.r-bloggers.com/2017/07/power-analysis-and-sample-size-calculation-for-agriculture/
#############################
library(pwr)
country_sample <- c("chile" = 1301, "uruguay" = 1450, "ecuador" = 5235, "brazil" = 9412, "colombia" = 22694)
# model1 = brazil; model2 = brazil, chile, uruguay; model3 = brazil, colombia, ecuador; model4 = all countries
models_sample <- c("model1" = country_sample[["brazil"]],
                   "model2" = sum(country_sample[c("brazil","chile","uruguay")]),
                   "model3" = sum(country_sample[c("brazil","colombia","ecuador")]),
                   "model4" = sum(country_sample))
# f2 calculation from R^2 --> f2 = R^2/(1 - R^2)
r_squared <- c("model1" = 0.15, "model2" = 0.1, "model3" = 0.22, "model4" = 0.2)
####
u <- 14 # Number of predictors - terms in the right-hand side of the equation
f2 <- r_squared^2/(1 - r_squared^2)
sig_level <- c(0.01)
v = models_sample - (u + 1) # degrees of freedom
res_mtx <- data.frame("u" = u, "sig.level" = sig_level, "f2" = f2, "v" = v, "model" = names(v))
power <- NULL
for(r in seq(nrow(res_mtx))) {
     pwr_val <- pwr.f2.test(u = res_mtx$u[r],
                            f2 = res_mtx$f2[r],
                            sig.level = res_mtx$sig.level[r],
                            v = res_mtx$v[r])
     power <- c(power, pwr_val$power)
}
res_mtx$power <- power