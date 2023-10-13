### TO DO
bn_model_diag_qq_lowlevel <- function(bn.fit, data, halfyear) {
  for (i in c("X10321", "X10304", "X10303", "X10302")) {
    fitted_res <- data.frame(fitted = bn.fit[[paste0("drainage_", i)]]$fitted.values,
                             residuals = bn.fit[[paste0("drainage_", i)]]$residuals)
    res_lowlevel <- na.omit(fitted_res[data[, paste0("drainage_", i)] < get(paste0("NM7Q_", i, "_", halfyear)), "residuals"])
    z_res_lowlevel <- scale(res_lowlevel)
    qqnorm(z_res_lowlevel, ylab = paste("Sample Quantiles", i))
    qqline(z_res_lowlevel)
  }
  

  
}

bn_model_diag_predic <- function(bn.fit, data) {
  for (i in c("X10321", "X10304", "X10303", "X10302")) {
    fitted_observ <- data.frame(fitted = bn.fit[[paste0("drainage_", i)]]$fitted.values,
                             observ = data[, paste0("drainage_", i)])
    print(ggplot(data = fitted_observ, aes(x = observ, y = fitted)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1) +
      labs(x = "values from data",
           y = "predictions",
           title = i)) +
      theme_bw()
  }
}