# Volatility Modeling with GARCH

This project analyzes the volatility dynamics of the SPDR Gold Shares ETF (GLD) using GARCH-family models and evaluates Value-at-Risk (VaR) forecasts.

The work was completed as a take-home assignment for a Financial and Time Series Econometrics course, University of Bologna (2025),
and focuses on empirical volatility modeling and risk forecasting.

## Project Overview

The analysis includes:

- Data exploration and stylized facts of financial returns
- Estimation of GARCH-type volatility models
- Model selection using AIC
- Volatility forecasting
- Value-at-Risk (VaR) estimation and backtesting

## Repository Structure

code/ R scripts for analysis 

figures/ Generated figures

tables/ LaTeX tables used in the report

volatility-modeling-garch.pdf Final report

## Data

Market data are downloaded directly from Yahoo Finance using the `quantmod` package.
No local datasets are required.

## How to Run

Run scripts in order:

1. `code/part1_stylized_facts.R`
2. `code/part2_garch_var_backtesting.R`

All outputs are automatically saved into `figures/` and `tables/`.

## Authors

Gulnara Sadykova, https://github.com/gulnarasadykova-kaz
Alnur Zhussupov

Master's in Economics and Econometrics
