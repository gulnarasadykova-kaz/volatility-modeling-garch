rm(list = ls())
cat("\014")
#packages
pkgs <- c("tidyverse","quantmod","tseries","moments","gridExtra","zoo","FinTS",
          "knitr","kableExtra","rugarch")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))
#settings
Data <- "GLD"
start_date <- "2015-01-01"
end_date <- "2025-12-24"
table <- "tables"
if (!dir.exists(table)) dir.create(table)
getSymbols(
  "GLD",
  src = "yahoo",
  from = start_date,
  to   = end_date
)
prices_daily <- Ad(get(Data))
prices_weekly <- Ad(to.weekly(get(Data)))

ret_daily  <- na.omit(diff(log(prices_daily)))
ret_weekly <- na.omit(diff(log(prices_weekly)))

#split data
n_obs <- length(ret_daily)
split <- floor((2/3) * n_obs)

train_daily <- ret_daily[1:split]
test_daily <- ret_daily[(split + 1):n_obs]

n_obs_w <- length(ret_weekly)
split_w <- floor((2/3) * n_obs_w)

train_weekly <- ret_weekly[1:split_w]
test_weekly <- ret_weekly[(split_w + 1):n_obs_w]
#best GARCH based on AIC
select_best_garch <- function(data) {
  #Standard GARCH with Normal Distribution
  spec1 <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "norm"
  )
  #Standard GARCH with Student-t 
  spec2 <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std"
  )
  #GJR-GARCH with Student-t 
  spec3 <- ugarchspec(
    variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std"
  )
  models <- list(spec1, spec2, spec3)
  names(models) <- c("sGARCH-norm", "sGARCH-std", "gjrGARCH-std")
  
  best_aic <- Inf
  best_fit <- NULL
  best_name <- ""
  
  for(name in names(models)) {
    fit <- tryCatch(ugarchfit(models[[name]], data = data), error=function(e) NULL)
    
    if(!is.null(fit)) {
      current_aic <- infocriteria(fit)[1]
      cat("Model:", name, " | AIC:", current_aic, "\n")
      
      if(current_aic < best_aic) {
        best_aic <- current_aic
        best_fit <- fit
        best_name <- name
      }
    }
  }
  
  cat("Selected Best Model:", best_name, "\n")
  return(list(fit = best_fit, best_name = best_name, best_aic = best_aic))
}

cat("\n--- Selection for Daily Data ---\n")
sel_d <- select_best_garch(train_daily)
best_fit_daily <- sel_d$fit

cat("\n--- Selection for Weekly Data ---\n")
sel_w <- select_best_garch(train_weekly)
best_fit_weekly <- sel_w$fit

cat("\nDaily Model Coefficients:\n")
print(coef(best_fit_daily))
cat("\nWeekly Model Coefficients:\n")
print(coef(best_fit_weekly))

#save AIC table
aic_table <- tibble(
  Frequency = c("Daily", "Weekly"),
  Best_Model = c(sel_d$best_name, sel_w$best_name),
  Best_AIC = c(sel_d$best_aic, sel_w$best_aic)
)

aic_latex <- kable(
  aic_table, format = "latex", booktabs = TRUE,
  caption = "Model selection based on AIC (Best model)",
  label = "tab:aic_best"
) %>% kable_styling(latex_options = "hold_position")

writeLines(aic_latex, file.path(table, "table_aic_best.tex"))

#save coefficient table
coef_table <- bind_rows(
  tibble(Frequency = "Daily",
         Parameter = names(coef(best_fit_daily)),
         Estimate = as.numeric(coef(best_fit_daily))),
  tibble(Frequency = "Weekly",
         Parameter = names(coef(best_fit_weekly)),
         Estimate = as.numeric(coef(best_fit_weekly)))
)

coef_latex <- kable(
  coef_table,
  format = "latex",
  booktabs = TRUE,
  caption = "Estimated GARCH Model Parameters (Selected Models)",
  label = "tab:garch_coef",
  digits = 6
) %>% kable_styling(latex_options = "hold_position")

writeLines(coef_latex, file.path(table, "table_garch_coefficients.tex"))

#residual diagnostics
check_residuals <- function(fit, label) {
  z <- as.numeric(residuals(fit, standardize = TRUE))
  
  lb_z  <- Box.test(z,   lag = 10, type = "Ljung-Box")
  lb_z2 <- Box.test(z^2, lag = 10, type = "Ljung-Box")
  
  archlm <- FinTS::ArchTest(z, lags = 10)
  
  cat("\n---", label, "Residual Diagnostics ---\n")
  cat("Ljung-Box (Std Resid) p-value:", lb_z$p.value, "\n")
  cat("Ljung-Box (Squared Std Resid) p-value:", lb_z2$p.value, "\n")
  cat("ARCH LM test (lags=10) p-value:", archlm$p.value, "\n")
  
  if(lb_z2$p.value > 0.05 && archlm$p.value > 0.05) {
    cat("Result: No strong remaining ARCH effects. Model looks adequate.\n")
  } else {
    cat("Result: Some remaining ARCH/serial dependence. Model may be misspecified.\n")
  }
  
  tibble(
    Frequency = label,
    LB_z_p = lb_z$p.value,
    LB_z2_p = lb_z2$p.value,
    ARCH_p = archlm$p.value
  )
}

diag_daily  <- check_residuals(best_fit_daily, "Daily")
diag_weekly <- check_residuals(best_fit_weekly, "Weekly")

diag_table <- bind_rows(diag_daily, diag_weekly)

diag_latex <- kable(
  diag_table, format = "latex", booktabs = TRUE, digits = 4,
  caption = "Residual Diagnostics for Standardized Residuals",
  label = "tab:diag"
) %>% kable_styling(latex_options = "hold_position")

writeLines(diag_latex, file.path(table, "table_residual_diagnostics.tex"))

#forcasts
forc_daily_point <- ugarchforecast(best_fit_daily, n.ahead = 5)
cat("\n--- Daily Point Forecasts (Sigma) ---\n")
print(sigma(forc_daily_point))

forc_weekly_point <- ugarchforecast(best_fit_weekly, n.ahead = 1)
cat("\n--- Weekly Point Forecast (Sigma) ---\n")
print(sigma(forc_weekly_point))

# VaR: compute quantiles + backtest (Kupiec)
# FIX: use rugarch::VaRTest (same output field used)
spec_fixed <- getspec(best_fit_daily)
setfixed(spec_fixed) <- as.list(coef(best_fit_daily))

filter_full <- ugarchfilter(spec = spec_fixed, data = ret_daily)

sigma_test <- sigma(filter_full)[(split + 1):n_obs]
mu_test <- fitted(filter_full)[(split + 1):n_obs]
actual_test <- test_daily

get_q <- function(fit, p){
  dist <- fit@model$modeldesc$distribution
  if(dist == "std"){
    shape <- coef(fit)["shape"]
    return(qdist(distribution = "std", p = p, shape = shape))
  } else if(dist == "norm"){
    return(qnorm(p))
  } else {
    stop(paste("Distribution not handled:", dist))
  }
}

q01 <- get_q(best_fit_daily, 0.01)
q05 <- get_q(best_fit_daily, 0.05)

VaR_01 <- mu_test + sigma_test * q01
VaR_05 <- mu_test + sigma_test * q05

evaluate_risk <- function(actual, VaR, alpha, freq_label) {
  actual <- as.numeric(actual)
  VaR    <- as.numeric(VaR)
  exceedances <- sum(actual < VaR)
  n <- length(actual)
  rate <- exceedances / n
  
  cat("\n--- VaR Evaluation (", freq_label, ", Alpha =", alpha, ") ---\n")
  cat("Expected Exceedances:", round(n * alpha, 0), "\n")
  cat("Actual Exceedances:", exceedances, "\n")
  cat("Failure Rate:", round(rate * 100, 2), "%\n")
  
  test <- rugarch::VaRTest(alpha = alpha, actual = actual, VaR = VaR, conf.level = 0.95)
  
  cat("Kupiec Test p-value:", test$uc.LRp, "\n")
  if(test$uc.LRp > 0.05) {
    cat("Result: Model Accepted (Correct coverage)\n")
  } else {
    cat("Result: Model Rejected (Incorrect coverage)\n")
  }
  
  tibble(
    Frequency = freq_label,
    Level = paste0(alpha*100, "%"),
    Exceedances = exceedances,
    Expected = round(n * alpha, 0),
    Kupiec_p = test$uc.LRp
  )
}

var_d1 <- evaluate_risk(actual_test, VaR_01, 0.01, "Daily")
var_d5 <- evaluate_risk(actual_test, VaR_05, 0.05, "Daily")

#weekly VaR
spec_fixed_w <- getspec(best_fit_weekly)
setfixed(spec_fixed_w) <- as.list(coef(best_fit_weekly))

filter_full_w <- ugarchfilter(spec = spec_fixed_w, data = ret_weekly)

sigma_test_w <- sigma(filter_full_w)[(split_w + 1):n_obs_w]
mu_test_w    <- fitted(filter_full_w)[(split_w + 1):n_obs_w]
actual_test_w <- test_weekly

q01_w <- get_q(best_fit_weekly, 0.01)
q05_w <- get_q(best_fit_weekly, 0.05)

VaR_01_w <- mu_test_w + sigma_test_w * q01_w
VaR_05_w <- mu_test_w + sigma_test_w * q05_w

var_w1 <- evaluate_risk(actual_test_w, VaR_01_w, 0.01, "Weekly")
var_w5 <- evaluate_risk(actual_test_w, VaR_05_w, 0.05, "Weekly")

#save VaR backtesting table
var_table <- bind_rows(var_d1, var_d5, var_w1, var_w5) %>%
  mutate(Kupiec_p = ifelse(Kupiec_p < 0.001, "<0.001", sprintf("%.3f", Kupiec_p)))

var_latex <- kable(
  var_table,
  format = "latex",
  booktabs = TRUE,
  caption = "Value-at-Risk Backtesting Results (Kupiec Unconditional Coverage Test)",
  label = "tab:var"
) %>% kable_styling(latex_options = "hold_position")

writeLines(var_latex, file.path(table, "table_var_backtesting.tex"))

cat("\nSaved tables to:", table, "\n")
