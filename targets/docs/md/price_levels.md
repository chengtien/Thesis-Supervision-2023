# Restaurant price levels

  * What is the distribution of price levels for restaurants in the dataset?  
  * What is the distribution of price levels for restaurants in the dataset, by city?  
  * What is the distribution of price levels for restaurants in the dataset, by city and business hour category?  
  * The above questions but substitute price levels with price change (i.e. change in log price).  
  
## Inflation of Restaurant Dinning Cost

|                 | (-Inf,-1]| (-1,0)|     0| (0,1]| (1, Inf]|
|:----------------|---------:|------:|-----:|-----:|--------:|
|summary_cpi_wax  |         0|    279| 11469|  3504|        0|
|summary_cpi_wane |         0|    263| 12579|  2410|        0|

## Demand and supply

A typical demand and supply functions are as follows:  

```math
\begin{aligned}
Q_d &= a - bP + \epsilon_{d} \\
Q_s &= c + dP + \epsilon_{s}\\
\end{aligned}
```  
    
where $Q_d$ is the quantity demanded, $Q_s$ is the quantity supplied, $P$ is the price, and $\epsilon_{d}$ and $\epsilon_{s}$ are the error terms.

## Identification  

If we solve the above demand and supply functions for $P$, we get:  

```math
\begin{aligned}
P &= \frac{a - c}{b + d} + \frac{\epsilon_{d} - \epsilon_{s}}{b + d} \\
\end{aligned}
```

For $Q$, we get:  

```math
\begin{aligned}
Q &= \frac{ad - bc}{b + d} + \frac{b\epsilon_{d} - d\epsilon_{s}}{b + d} \\
\end{aligned}
```

Consider the inverse supply function:  

```math
\begin{aligned}
P &= \frac{Q - c-\epsilon_{s} - dP}{d} \\
P &= \frac{Q - c-\epsilon_{s}}{d + 1} \\
\end{aligned}
```

We can express the inverse supply function as a function of $Q$ and $P$ as follows:  

```math
\begin{aligned}
P = \beta_0^s+\beta_1^s Q+\tilde{\epsilon}_{s}
\end{aligned}
```

where $\beta_0^s = \frac{-c}{d + 1}$, $\beta_1^s = \frac{1}{d + 1}$, and $\tilde{\epsilon}_{s} = -\frac{\epsilon_{s}}{d + 1}$.

Conditional on $P$, 

```math
P = \beta_0^s+\beta_1^s Q -\frac{\mathbb{E}(\epsilon_{s}|P)}{d + 1}
```
