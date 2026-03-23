``` r
library(MASS)
data = read.table("Miete-22.dat")


data$floor.space = "under 50"
data$floor.space[data$wfl> 50] = "50 to 100"
data$floor.space[data$wfl> 100] = "above 100"

data$center = "yes"
data$center[data$Makrolage==0] = "no"
data$new.contract = "yes"
data$new.contract[data$NeuerVertrag==0 ] = "no"

data$district = data$SBez

tab = table(data$floor.space, data$center, data$new.contract , data$district)
dat_df = as.data.frame(tab)
colnames(dat_df) = c("floor.space","center","new.contract","district","Freq" )

n = sum(dat_df$Freq)
ind2 = loglm(Freq ~ floor.space + center + new.contract + district, 
             data=dat_df)

full =  loglm(Freq ~ floor.space* center * new.contract * district, 
              data=dat_df)
mod = step(ind2, scope=list(lower=formula(ind2),upper=formula(full)),
           direction="forward", k = log(n))
```

    ## Start:  AIC=3756.72
    ## Freq ~ floor.space + center + new.contract + district
    ## 
    ##                            Df    AIC
    ## + center:district          24  787.3
    ## + floor.space:center        2 3736.5
    ## + floor.space:new.contract  2 3751.4
    ## <none>                        3756.7
    ## + center:new.contract       1 3763.2
    ## + new.contract:district    24 3875.6
    ## + floor.space:district     48 4024.1
    ## 
    ## Step:  AIC=787.33
    ## Freq ~ floor.space + center + new.contract + district + center:district
    ## 
    ##                            Df     AIC
    ## + floor.space:center        2  767.12
    ## + floor.space:new.contract  2  781.96
    ## <none>                         787.33
    ## + center:new.contract       1  793.81
    ## + new.contract:district    24  906.23
    ## + floor.space:district     48 1054.68
    ## 
    ## Step:  AIC=767.12
    ## Freq ~ floor.space + center + new.contract + district + center:district + 
    ##     floor.space:center
    ## 
    ##                            Df     AIC
    ## + floor.space:new.contract  2  761.75
    ## <none>                         767.12
    ## + center:new.contract       1  773.61
    ## + new.contract:district    24  886.02
    ## + floor.space:district     48 1068.58
    ## 
    ## Step:  AIC=761.75
    ## Freq ~ floor.space + center + new.contract + district + center:district + 
    ##     floor.space:center + floor.space:new.contract
    ## 
    ##                         Df     AIC
    ## <none>                      761.75
    ## + center:new.contract    1  768.66
    ## + new.contract:district 24  880.83
    ## + floor.space:district  48 1063.21

``` r
summary(mod)
```

    ## Formula:
    ## Freq ~ floor.space + center + new.contract + district + center:district + 
    ##     floor.space:center + floor.space:new.contract
    ## attr(,"variables")
    ## list(Freq, floor.space, center, new.contract, district)
    ## attr(,"factors")
    ##              floor.space center new.contract district center:district
    ## Freq                   0      0            0        0               0
    ## floor.space            1      0            0        0               0
    ## center                 0      1            0        0               1
    ## new.contract           0      0            1        0               0
    ## district               0      0            0        1               1
    ##              floor.space:center floor.space:new.contract
    ## Freq                          0                        0
    ## floor.space                   1                        1
    ## center                        1                        0
    ## new.contract                  0                        1
    ## district                      0                        0
    ## attr(,"term.labels")
    ## [1] "floor.space"              "center"                  
    ## [3] "new.contract"             "district"                
    ## [5] "center:district"          "floor.space:center"      
    ## [7] "floor.space:new.contract"
    ## attr(,"order")
    ## [1] 1 1 1 1 2 2 2
    ## attr(,"intercept")
    ## [1] 1
    ## attr(,"response")
    ## [1] 1
    ## attr(,".Environment")
    ## <environment: R_GlobalEnv>
    ## attr(,"predvars")
    ## list(Freq, floor.space, center, new.contract, district)
    ## attr(,"dataClasses")
    ##         Freq  floor.space       center new.contract     district 
    ##    "numeric"     "factor"     "factor"     "factor"     "factor" 
    ## 
    ## Statistics:
    ##                       X^2  df    P(> X^2)
    ## Likelihood Ratio 301.9788 243 0.005954323
    ## Pearson               NaN 243         NaN

``` r
# loglm(~)
```
