# 📈 Portfolio Optimization – Lorenzo Zullo

A financial modeling project built in R to explore optimal portfolio construction using modern portfolio theory. The goal was to analyze risk-return trade-offs, identify efficient frontiers, and compute asset weights that minimize portfolio volatility or maximize Sharpe ratio.

## 💡 Objectives
- Construct a diversified portfolio using 4 selected stocks and a bond index.
- Calculate expected returns, standard deviations, and covariances.
- Generate the **efficient frontier** using simulation and optimization techniques.
- Identify the **Minimum Variance Portfolio (MVP)** and **Tangency Portfolio**.

## 🧰 Tools & Methods
- **Language**: R  
- **Libraries**: `tidyverse`, `quadprog`, `ggplot2`  
- **Data Sources**: Yahoo Finance or CSV input of adjusted closing prices  
- **Key Techniques**:
  - Log return calculation  
  - Covariance matrix estimation  
  - Constrained optimization using `solve.QP()`  
  - Sharpe ratio maximization

## 📊 Outputs
- Efficient frontier plot displaying risk-return profiles of 1000+ portfolios
- Highlighted points for:
  - **Minimum Variance Portfolio**
  - **Tangency Portfolio (Max Sharpe Ratio)**
- Asset allocation breakdowns for each optimized portfolio

## 🧠 Key Insights
- Diversification reduces overall portfolio volatility
- Optimization allows dynamic allocation based on investor risk preference
- Sharpe ratio helps identify portfolios that offer the best risk-adjusted returns

## 🚀 How to Run
1. Load `project.R` in RStudio  
2. Ensure required libraries are installed  
3. Run all chunks sequentially  
4. Modify asset tickers or weights for custom optimization scenarios

