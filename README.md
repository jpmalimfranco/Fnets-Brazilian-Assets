# Learning Volatility Structures in Emerging Markets: A Data-Driven Network Approach Using Stochastic Volatility Models

This repository contains the code and data for the paper "Learning Volatility Structures in Emerging Markets: A Data-Driven Network Approach Using Stochastic Volatility Models" by João Pedro M. Franco and Márcio P. Laurini.

## About the Project

This study investigates volatility interdependencies in the Brazilian equity market using an approach that combines Factor-Adjusted Networks (FNETS) with latent volatilities estimated via Stochastic Volatility (SV) models. The analysis focuses on firms within the Bovespa Theoretical Portfolio from 2022 to 2025.

The results uncover a heterogeneous network structure, with "core" stocks (those with higher portfolio weights) displaying strong systemic linkages, while "peripheral" firms exhibit weaker connections. The objective is to enhance systemic risk monitoring and offer practical insights for policymakers and investors.

### Keywords

Volatility spillovers, Factor-adjusted networks, Stochastic volatility, Systemic risk, Brazilian equity market.

## Methodology

The methodological framework integrates two main techniques to analyze network volatility:

1.  **Stochastic Volatility (SV) Model with INLA:** For each asset, latent volatility is estimated using univariate Stochastic Volatility (SV) models. To overcome the computational inefficiency of traditional methods like MCMC, we employ Integrated Nested Laplace Approximations (INLA), which offers a significant speedup in estimation while maintaining high accuracy.

2.  **Factor-Adjusted Networks (FNETS):** The estimated volatility time series are used as inputs for the FNETS methodology. This approach decomposes the series into common (systemic factor-driven) and idiosyncratic (firm-specific) components. From the idiosyncratic components, three types of networks are constructed for a comprehensive analysis of interdependencies:
    * **Granger Causal:** Directed predictive (lead-lag) relationships.
    * **Partial Correlation (Contemporaneous):** Simultaneous, direct interactions between assets after controlling for common factors.
    * **Long-Run Partial Correlation:** Persistent, structural connections that filter out short-term noise.

The superiority of the approach is validated by comparing the forecasting performance of the SV model against GARCH(1,1), OHLC, and HL volatility measures.

## Dataset

The analysis uses data from the theoretical portfolio of the Bovespa Index (Ibovespa), valid for the period from January to April 2025.

* **Analysis Period:** January 4, 2022, to January 28, 2025 (totaling 765 trading days).
* **Final Sample:** After excluding stocks with limited trading activity to ensure data consistency, the final sample comprises 79 Brazilian companies.

## Key Findings

* **Core-Periphery Network Structure:** The partial correlation networks (contemporaneous and long-run) reveal a clear 'core-periphery' structure. The core consists of a highly interconnected "financial-commodity cluster" (e.g., ITUB4, BBDC4, VALE3, PETR4), while the periphery contains less integrated firms (e.g., retail, utilities).
* **Superior Forecasting Performance of the SV Model:** The Stochastic Volatility (SV) model demonstrated consistently superior forecasting performance, yielding the lowest Mean Absolute Error (MAE), Mean Absolute Percentage Error (MAPE), and Mean Squared Error (MSE) values compared to GARCH(1,1), OHLC, and HL models.
* **Network Dynamics:** The market's connectivity varies significantly across different network types. The Granger Causal network is extremely sparse, the Partial Correlation network is dense, and the Long-Run network shows intermediate connectivity, revealing the market's structural backbone.
* **Policy Implications:** The FNETS-SV framework can be used by regulators, such as the Central Bank of Brazil, to identify systemically important stocks, serve as an early-warning system for risk accumulation, and enhance stress-testing models.
