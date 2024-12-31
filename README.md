# R Code to estimate a semi-ordered bivariate probit model of the type found in Cai & Kalb (2006). 

The log-likelihood function is as shown below: 

### Equations
```math
\\ (P_{00} = \text{Prob}(j = 0, k = 1) = \Phi_2(-\beta_2' x_{i,2}, -\beta_1' x_{i,1}, \rho) \\)
```

```math
\\(P_{10} = \Phi_2(\tau - \beta_2' x_{i,2}, -\beta_1' x_{i,1}, \rho) - \Phi_2(-\beta_2' x_{i,2}, -\beta_1' x_{i,1}, \rho) \\)  
   ```
```math
\\( P_{20} = \Phi(-\beta_1' x_{i,1}) - \Phi_2(\tau - \beta_2' x_{i,2}, -\beta_1' x_{i,1}, \rho) \\)  
```
```math
\\( P_{01} = \Phi(-\beta_2' x_{i,2}) - \Phi_2(-\beta_2' x_{i,2}, -\beta_1' x_{i,1}, \rho) \\)  
```

```math
\\( P_{11} = \Phi(\tau - \beta_2' x_{i,2}) - \Phi(-\beta_2' x_{i,2}) - \Phi_2(\tau - \beta_2' x_{i,2}, -\beta_1' x_{i,1}, \rho) + \Phi_2(-\beta_2' x_{i,2}, -\beta_1' x_{i,1}, \rho) \\)  
```

```math
\\( P_{21} = 1 - \Phi(\tau - \beta_2' x_{i,2}) - \Phi(-\beta_1' x_{i,1}) + \Phi_2(\tau - \beta_2' x_{i,2}, -\beta_1' x_{i,1}, \rho) \\)  
```
