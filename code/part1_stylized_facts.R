rm(list = ls())
cat("\014")
#packages
pkgs <- c(
  "tidyverse", "quantmod", "tseries", "moments",
  "gridExtra", "zoo", "FinTS", "knitr", "kableExtra"
)
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))
#settings
Data <- "GLD"
start_date <- "2015-01-01"
end_date <- "2025-12-24"
figure <- "figures"
table <- "tables"
if (!dir.exists(figure)) dir.create(figure, recursive = TRUE)
if (!dir.exists(table)) dir.create(table, recursive = TRUE)
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
#helper
plot_series <- function(x, title, color_line = "black") {
  df <- tibble(Date = zoo::index(x), Value = as.numeric(x))
  ggplot(df, aes(Date, Value)) +
    geom_line(color = color_line, linewidth = 0.6) +
    labs(title = title, x = "", y = "") +
    theme_minimal()
}

plot_hist_with_normal <- function(x, title) {
  df <- tibble(x = as.numeric(x))
  mu <- mean(df$x)
  s  <- sd(df$x)
  
  ggplot(df, aes(x)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 80, fill = "gray80", color = "white") +
    stat_function(fun = dnorm, args = list(mean = mu, sd = s),
                  color = "red", linewidth = 0.6) +
    labs(title = title, x = "Log returns", y = "Density") +
    theme_minimal()
}

fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

#figures
fig1 <- gridExtra::grid.arrange(
  plot_series(prices_daily,  "GLD Price (Daily Level)", "darkblue"),
  plot_series(ret_daily,     "GLD Log Returns (Daily)", "darkred"),
  plot_hist_with_normal(ret_daily, "Daily Returns: Histogram (Red = Normal)"),
  ncol = 1
)

ggsave(
  filename = file.path(figure, "Fig1_Daily_Price_Returns.pdf"),
  plot = fig1, width = 8, height = 10
)

fig2 <- gridExtra::grid.arrange(
  plot_series(prices_weekly, "GLD Price (Weekly Adjusted Close)", "darkblue"),
  plot_series(ret_weekly,    "GLD Log Returns (Weekly)", "darkred"),
  plot_hist_with_normal(ret_weekly, "Weekly Returns: Histogram (Red = Normal)"),
  ncol = 1
)

ggsave(
  filename = file.path(figure, "Fig2_Weekly_Price_Returns.pdf"),
  plot = fig2, width = 8, height = 10
)

pdf(file.path(figure, "Fig3_ACF_Returns.pdf"), width = 8, height = 6)
par(mfrow = c(2, 2), mar = c(4, 4, 4, 1))

acf(as.numeric(ret_daily),    main = "ACF: Daily Returns")
acf(as.numeric(ret_daily)^2,  main = "ACF: Daily Returns^2")
acf(as.numeric(ret_weekly),   main = "ACF: Weekly Returns")
acf(as.numeric(ret_weekly)^2, main = "ACF: Weekly Returns^2")

par(mfrow = c(1, 1))
dev.off()

#Statistical tests
# Ljung–Box
lb_d_ret <- Box.test(as.numeric(ret_daily),  lag = 20, type = "Ljung-Box")
lb_d_sq  <- Box.test(as.numeric(ret_daily)^2, lag = 20, type = "Ljung-Box")
lb_w_ret <- Box.test(as.numeric(ret_weekly), lag = 10, type = "Ljung-Box")
lb_w_sq  <- Box.test(as.numeric(ret_weekly)^2, lag = 10, type = "Ljung-Box")

# ARCH LM
arch_d <- FinTS::ArchTest(as.numeric(ret_daily),  lags = 12)
arch_w <- FinTS::ArchTest(as.numeric(ret_weekly), lags = 6)

# Jarque–Bera
jb_d <- tseries::jarque.bera.test(as.numeric(ret_daily))
jb_w <- tseries::jarque.bera.test(as.numeric(ret_weekly))

# ADF (note: default settings; for reporting you may want explicit lags/trend choices)
adf_price_d <- tseries::adf.test(as.numeric(prices_daily))
adf_price_w <- tseries::adf.test(as.numeric(prices_weekly))
adf_ret_d   <- tseries::adf.test(as.numeric(ret_daily))
adf_ret_w   <- tseries::adf.test(as.numeric(ret_weekly))

#Summary tables
# Prices summary
price_sum <- tibble(
  Frequency = c("Daily", "Weekly"),
  N = c(length(prices_daily), length(prices_weekly)),
  Start = c(as.character(start(prices_daily)), as.character(start(prices_weekly))),
  End   = c(as.character(end(prices_daily)),   as.character(end(prices_weekly))),
  Min   = c(min(as.numeric(prices_daily)), min(as.numeric(prices_weekly))),
  Max   = c(max(as.numeric(prices_daily)), max(as.numeric(prices_weekly))),
  Mean  = c(mean(as.numeric(prices_daily)), mean(as.numeric(prices_weekly))),
  SD    = c(sd(as.numeric(prices_daily)),   sd(as.numeric(prices_weekly)))
) %>%
  mutate(
    Min  = sprintf("%.2f", Min),
    Max  = sprintf("%.2f", Max),
    Mean = sprintf("%.4f", as.numeric(Mean)),
    SD   = sprintf("%.4f", as.numeric(SD))
  )

# Returns summary
one_sum <- tibble(
  Frequency = c("Daily", "Weekly"),
  N = c(length(ret_daily), length(ret_weekly)),
  Mean = c(mean(ret_daily), mean(ret_weekly)),
  SD = c(sd(ret_daily), sd(ret_weekly)),
  Skewness = c(moments::skewness(ret_daily), moments::skewness(ret_weekly)),
  Kurtosis = c(moments::kurtosis(ret_daily), moments::kurtosis(ret_weekly)),
  JB_pvalue = c(jb_d$p.value, jb_w$p.value),
  LjungBox_ret_p   = c(lb_d_ret$p.value, lb_w_ret$p.value),
  LjungBox_sqret_p = c(lb_d_sq$p.value,  lb_w_sq$p.value),
  ARCHLM_pvalue    = c(arch_d$p.value,   arch_w$p.value)
) %>%
  mutate(
    Mean = sprintf("%.6f", Mean),
    SD = sprintf("%.6f", SD),
    Skewness = sprintf("%.3f", Skewness),
    Kurtosis = sprintf("%.3f", Kurtosis),
    JB_pvalue = fmt_p(JB_pvalue),
    LjungBox_ret_p = fmt_p(LjungBox_ret_p),
    LjungBox_sqret_p = fmt_p(LjungBox_sqret_p),
    ARCHLM_pvalue = fmt_p(ARCHLM_pvalue)
  )

#latex tables
price_latex <- kable(
  price_sum,
  format = "latex",
  booktabs = TRUE,
  caption = "Summary Statistics of GLD Prices",
  label = "tab:prices",
  align = "lccccccc",
  escape = FALSE
) %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(
    general = "Prices are adjusted closing prices. Weekly prices are weekly closes obtained by aggregating adjusted daily prices.",
    threeparttable = TRUE
  )

writeLines(price_latex, file.path(table, "table_prices.tex"))

returns_latex <- kable(
  one_sum %>% select(Frequency, N, Mean, SD, Skewness, Kurtosis,
                     JB_pvalue, LjungBox_ret_p, LjungBox_sqret_p, ARCHLM_pvalue),
  format = "latex",
  booktabs = TRUE,
  caption = "Summary Statistics and Stylized Facts of GLD Returns",
  label = "tab:returns",
  align = "lccccccccc",
  col.names = c("Freq.", "N", "Mean", "SD", "Skew", "Kurt",
                "JB p", "LB($r$) p", "LB($r^2$) p", "ARCH p"),
  escape = FALSE
) %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(
    general = "JB is the Jarque--Bera normality test. LB($r$) and LB($r^2$) are Ljung--Box tests on returns and squared returns. ARCH is the Engle ARCH LM test. Entries are p-values.",
    threeparttable = TRUE
  )

writeLines(returns_latex, file.path(table, "table_returns.tex"))


