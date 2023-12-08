# create dataset with observed values and residuals
data_obs_res <- function(model, data, catchment) {
  data.frame(obs = na.omit(data)[, paste0("drainage_", catchment)],
             res = na.omit(data)[, paste0("drainage_", catchment)] - model$fitted.values)
}

# create dataset with observed values and residuals for member kbj
data_obs_res_kbj <- function(model, data_kbj, catchment) {
  data_tmp <- data.frame(obs = na.omit(data_kbj)[, paste0("drainage_", catchment)],
                         pred = predict(model, na.omit(data_kbj), type="response"))
  cbind(data_tmp, res = data_tmp$obs - data_tmp$pred)
}

# create dataset with observed values and residuals for member kbo
data_obs_res_kbo <- function(model, data_kbo, catchment) {
  data_tmp <- data.frame(obs = na.omit(data_kbo)[, paste0("drainage_", catchment)],
                         pred = predict(model, na.omit(data_kbo), type="response"))
  cbind(data_tmp, res = data_tmp$obs - data_tmp$pred)
}

# plot residuals vs. observed
plot_obs_res <- function(data_obs_res, halfyear, catchment, title,
                         ylim_l = NULL, ylim_u = NULL, xlim_l = NULL, xlim_u = NULL, all = FALSE) {
  if (all == TRUE) {
    probs_tmp <- c(0.99999999, 0.99, 0.95, 0.8, 0.5)
  } else {
    probs_tmp <- c(0.99, 0.95, 0.8, 0.5)
  }
  
  if (!is.null(ylim_l) & !is.null(xlim_l)) {
    plot_tmp <- ggplot(data_obs_res, aes(x = obs, y = res)) +
      ggdensity::geom_hdr(probs = probs_tmp) +
      geom_vline(xintercept = get(paste0("NM7Q_", catchment, "_", halfyear)), col = "blue") +
      geom_hline(yintercept = 0) +
      xlim(xlim_l, xlim_u) +
      ylim(ylim_l, ylim_u) +
      labs(x = "observed drainage", y = "residuals", title = title) +
      theme_bw()
  } else if (!is.null(ylim_l) & is.null(xlim_l)) {
      plot_tmp <- ggplot(data_obs_res, aes(x = obs, y = res)) +
        ggdensity::geom_hdr(probs = probs_tmp) +
        geom_vline(xintercept = get(paste0("NM7Q_", catchment, "_", halfyear)), col = "blue") +
        geom_hline(yintercept = 0) +
        ylim(ylim_l, ylim_u) +
        labs(x = "observed drainage", y = "residuals", title = title) +
        theme_bw()
  } else if (is.null(ylim_l) & !is.null(xlim_l)) {
    plot_tmp <- ggplot(data_obs_res, aes(x = obs, y = res)) +
      ggdensity::geom_hdr(probs = probs_tmp) +
      geom_vline(xintercept = get(paste0("NM7Q_", catchment, "_", halfyear)), col = "blue") +
      geom_hline(yintercept = 0) +
      xlim(xlim_l, xlim_u) +
      labs(x = "observed drainage", y = "residuals", title = title) +
      theme_bw()
  } else if (is.null(ylim_l) & is.null(xlim_l)) {
      plot_tmp <- ggplot(data_obs_res, aes(x = obs, y = res)) +
        ggdensity::geom_hdr(probs = probs_tmp) +
        geom_vline(xintercept = get(paste0("NM7Q_", catchment, "_", halfyear)), col = "blue") +
        geom_hline(yintercept = 0) +
        labs(x = "observed drainage", y = "residuals", title = title) +
        theme_bw()
  }
  print(plot_tmp)
}

# plot residuals vs. observed for member kbj
plot_obs_res_kbj <- function(data_obs_res_kbj, halfyear, catchment, title,
                             ylim_l = NULL, ylim_u = NULL, xlim_l = NULL, xlim_u = NULL, all = FALSE) {
  if (all == TRUE) {
    probs_tmp <- c(0.99999999, 0.99, 0.95, 0.8, 0.5)
  } else {
    probs_tmp <- c(0.99, 0.95, 0.8, 0.5)
  }
  
  if (!is.null(ylim_l) & !is.null(xlim_l)) {
    plot_tmp <- ggplot(data_obs_res_kbj, aes(x = obs, y = res)) +
      ggdensity::geom_hdr(probs = probs_tmp) +
      geom_vline(xintercept = get(paste0("NM7Q_", catchment, "_", halfyear)), col = "blue") +
      geom_hline(yintercept = 0) +
      xlim(xlim_l, xlim_u) +
      ylim(ylim_l, ylim_u) +
      labs(x = "observed drainage", y = "residuals", title = title) +
      theme_bw()
  } else if (!is.null(ylim_l) & is.null(xlim_l)) {
    plot_tmp <- ggplot(data_obs_res_kbj, aes(x = obs, y = res)) +
      ggdensity::geom_hdr(probs = probs_tmp) +
      geom_vline(xintercept = get(paste0("NM7Q_", catchment, "_", halfyear)), col = "blue") +
      geom_hline(yintercept = 0) +
      ylim(ylim_l, ylim_u) +
      labs(x = "observed drainage", y = "residuals", title = title) +
      theme_bw()
  } else if (is.null(ylim_l) & !is.null(xlim_l)) {
    plot_tmp <- ggplot(data_obs_res_kbj, aes(x = obs, y = res)) +
      ggdensity::geom_hdr(probs = probs_tmp) +
      geom_vline(xintercept = get(paste0("NM7Q_", catchment, "_", halfyear)), col = "blue") +
      geom_hline(yintercept = 0) +
      xlim(xlim_l, xlim_u) +
      labs(x = "observed drainage", y = "residuals", title = title) +
      theme_bw()
  } else if (is.null(ylim_l) & is.null(xlim_l)) {
    plot_tmp <- ggplot(data_obs_res_kbj, aes(x = obs, y = res)) +
      ggdensity::geom_hdr(probs = probs_tmp) +
      geom_vline(xintercept = get(paste0("NM7Q_", catchment, "_", halfyear)), col = "blue") +
      geom_hline(yintercept = 0) +
      labs(x = "observed drainage", y = "residuals", title = title) +
      theme_bw()
  }
  print(plot_tmp)
}

# plot predicted vs. observed for member kbo
plot_obs_pred_kbo <- function(data_obs_res_kbo, halfyear, catchment, title,
                             ylim_l = NULL, ylim_u = NULL, xlim_l = NULL, xlim_u = NULL, all = FALSE) {
  if (all == TRUE) {
    probs_tmp <- c(0.99999999, 0.99, 0.95, 0.8, 0.5)
  } else {
    probs_tmp <- c(0.99, 0.95, 0.8, 0.5)
  }
  
  if (!is.null(ylim_l) & !is.null(xlim_l)) {
    plot_tmp <- ggplot(data_obs_res_kbo, aes(x = obs, y = pred)) +
      ggdensity::geom_hdr(probs = probs_tmp) +
      geom_vline(xintercept = get(paste0("NM7Q_", catchment, "_", halfyear)), col = "blue") +
      geom_abline(intercept = 0, slope = 1) +
      xlim(xlim_l, xlim_u) +
      ylim(ylim_l, ylim_u) +
      labs(x = "observed drainage", y = "predicted drainage", title = title) +
      theme_bw() +
      theme(axis.title = element_text(size = 20),
            axis.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20))
  } else if (!is.null(ylim_l) & is.null(xlim_l)) {
    plot_tmp <- ggplot(data_obs_res_kbo, aes(x = obs, y = pred)) +
      ggdensity::geom_hdr(probs = probs_tmp) +
      geom_vline(xintercept = get(paste0("NM7Q_", catchment, "_", halfyear)), col = "blue") +
      geom_abline(intercept = 0, slope = 1) +
      ylim(ylim_l, ylim_u) +
      labs(x = "observed drainage", y = "predicted drainage", title = title) +
      theme_bw() +
      theme(axis.title = element_text(size = 20),
            axis.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20))
  } else if (is.null(ylim_l) & !is.null(xlim_l)) {
    plot_tmp <- ggplot(data_obs_res_kbo, aes(x = obs, y = pred)) +
      ggdensity::geom_hdr(probs = probs_tmp) +
      geom_vline(xintercept = get(paste0("NM7Q_", catchment, "_", halfyear)), col = "blue") +
      geom_abline(intercept = 0, slope = 1) +
      xlim(xlim_l, xlim_u) +
      labs(x = "observed drainage", y = "predicted drainage", title = title) +
      theme_bw() +
      theme(axis.title = element_text(size = 20),
            axis.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20))
  } else if (is.null(ylim_l) & is.null(xlim_l)) {
    plot_tmp <- ggplot(data_obs_res_kbo, aes(x = obs, y = pred)) +
      ggdensity::geom_hdr(probs = probs_tmp) +
      geom_vline(xintercept = get(paste0("NM7Q_", catchment, "_", halfyear)), col = "blue") +
      geom_abline(intercept = 0, slope = 1) +
      labs(x = "observed drainage", y = "predicted drainage", title = title) +
      theme_bw() +
      theme(axis.title = element_text(size = 20),
            axis.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20))
  }
  print(plot_tmp)
}

# MSE for low drainage values for member kbj
res_mse_kbj <- function(data_obs_res_kbj, halfyear, catchment) {
  NM7Q_tmp <- get(paste0("NM7Q_", catchment, "_", halfyear))
  mean((data_obs_res_kbj[data_obs_res_kbj$obs <= NM7Q_tmp, ]$res)^2)
}

# MSE for low drainage values for member kbo
res_mse_kbo <- function(data_obs_res_kbo, halfyear, catchment) {
  NM7Q_tmp <- get(paste0("NM7Q_", catchment, "_", halfyear))
  mean((data_obs_res_kbo[data_obs_res_kbo$obs <= NM7Q_tmp, ]$res)^2)
}

# MSE for all observations for member kbj
res_mse_all_kbo <- function(data_obs_res_kbo, halfyear, catchment) {
  mean((data_obs_res_kbo$res)^2)
}

# find model with interaction that minimizes the MSE for low drainage values
find_best_interac <- function(vars, formula_chr, data, data_kbj, family, catchment, halfyear) {
  gam_best_list <- list()
  gam_best_mse <- numeric()
  
  gam_best_list[[1]] <- bam(formula = as.formula(formula_chr), data = data, family = family, control = gam.control(maxit = 300))
  data_obs_res_kbj_tmp <- data_obs_res_kbj(model = gam_best_list[[1]], data_kbj = data_kbj, catchment = catchment)
  NM7Q_tmp <- get(paste0("NM7Q_", catchment, "_", halfyear))
  res_mse_kbj_tmp <- res_mse_kbj(data_obs_res_kbj = data_obs_res_kbj_tmp, halfyear = halfyear, catchment = catchment)
  gam_best_mse[1] <- res_mse_kbj_tmp
  
  for (i in vars) {
    for (j in vars) {
      if (i != j) {
        model_tmp <- bam(formula = as.formula(paste0(formula_chr, ' + ', i, ":", j)),
                                        data = data, family = family, control = gam.control(maxit = 300))
        data_obs_res_kbj_tmp <- data_obs_res_kbj(model = model_tmp, data_kbj = data_kbj, catchment = catchment)
        res_mse_kbj_tmp <- res_mse_kbj(data_obs_res_kbj = data_obs_res_kbj_tmp, halfyear = halfyear, catchment = catchment)
        print(res_mse_kbj_tmp)
        
        if (length(gam_best_list) < 10) {
          gam_best_list[[length(gam_best_list) + 1]] <- model_tmp
          gam_best_mse[length(gam_best_mse) + 1] <- res_mse_kbj_tmp
        } else {
          if (any(gam_best_mse - res_mse_kbj_tmp > 0)) {
            gam_best_list[[which.max(gam_best_mse - res_mse_kbj_tmp)]] <- model_tmp
            gam_best_mse[which.max(gam_best_mse - res_mse_kbj_tmp)] <- res_mse_kbj_tmp
          }
        }
        
      }
    }
  }
  assign("gam_best_list", gam_best_list)
  print(gam_best_mse)
  summary(gam_best_list[[which.min(gam_best_mse)]])
}
