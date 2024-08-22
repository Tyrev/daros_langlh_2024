#############################
# Package Install/Load
#############################
library(pwr)
library(ggplot2)
library(ggpubr)

#############################
# Custom Function
#############################
power_mtx <- function(mod_sampsize_obs, r2_obs, model_nm) {
     x_mod_sampsize_obs <- mod_sampsize_obs[model_nm]
     y_r2_obs <- r2_obs[model_nm]
     ## sample size grid
     by_grid <- ifelse(test = mod_sampsize_obs<10000,
                       yes = 500, no = 2500)
     models_samples_grid <- seq(from = 500, to = x_mod_sampsize_obs + by_grid, by = by_grid)
     ##
     u <- 14 # Number of predictors - terms in the right-hand side of the equation
     f2 <- y_r2_obs^2/(1 - y_r2_obs^2)
     sig_level <- c(0.01)
     v <- models_samples_grid - (u + 1) # degrees of freedom
     names(models_samples_grid) <- v
     res_mtx <- expand.grid("u" = u, "sig.level" = sig_level, "f2" = f2, "v" = v, "models" = model_nm)
     res_mtx$N <- models_samples_grid[as.character(res_mtx$v)]
     power <- NULL
     for(r in seq(nrow(res_mtx))) {
          pwr_val <- pwr.f2.test(u = res_mtx$u[r],
                                 f2 = res_mtx$f2[r],
                                 sig.level = res_mtx$sig.level[r],
                                 v = res_mtx$v[r])
          power <- c(power, pwr_val$power)
     }
     res_mtx$power <- power
     
     p <- ggplot(data = res_mtx, mapping = aes(x = N, 
                                               y = power)) +
          geom_point() + geom_line() +
          geom_vline(xintercept = x_mod_sampsize_obs, linewidth = 1, linetype = "dashed") +
          scale_color_manual(name = "Power", 
                             values = RColorBrewer::brewer.pal(n = 8, name = "Accent")) +
          labs(title = "Power Analysis for General Linear Model",
               subtitle = paste("MODEL = ", names(x_mod_sampsize_obs), "| R² = ", y_r2_obs),
               y = "Power", 
               x = "Sample Size (N)") +
          scale_x_continuous(limits = c(0, max(res_mtx$N) + by_grid), breaks = seq(from = 0,
                                                                         to = max(res_mtx$N),
                                                                         by = by_grid)) +
          theme_bw() + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank())
     
}

#############################
# Observed Data
#############################
country_sample <- c("chile" = 1301, "uruguay" = 1450, "ecuador" = 5235, "brazil" = 9412, "colombia" = 22694)
# model1 = brazil; model2 = brazil, chile, uruguay; model3 = brazil, colombia, ecuador; model4 = all countries
models_sample_observed <- c("model1" = country_sample[["brazil"]],
                            "model2" = sum(country_sample[c("brazil","chile","uruguay")]),
                            "model3" = sum(country_sample[c("brazil","colombia","ecuador")]),
                            "model4" = sum(country_sample))
# f2 calculation from R^2 --> f2 = R^2/(1 - R^2)
r_squared_observed <- c("model1" = 0.15, "model2" = 0.1, "model3" = 0.22, "model4" = 0.2)

############################
# Power Analysis for Linear Model
# https://www.r-bloggers.com/2017/07/power-analysis-and-sample-size-calculation-for-agriculture/
#############################
res <- list()
for(mod in names(models_sample_observed)) {
     res[[mod]] <- power_mtx(mod_sampsize_obs = models_sample_observed[mod],
                             r2_obs = r_squared_observed[mod],
                             model_nm = mod)
}
final_plot <- ggarrange(plotlist = res, ncol = 2, nrow = 2)
ggsave(filename = "pwr_analysis_plot.pdf", plot = final_plot, width = 20, height = 10)
