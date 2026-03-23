---
layout: default
title: "Likelihood Principle"
---

``` r
x = 1 
n = 10
p = 0.2

p = seq(0,1,by=0.01)
pdf("likelihoodprinciple.pdf",height=4,width =8)
plot( p, dbinom(x, size=n, prob=p), type="l", ylab="Likelihood")
lines(p, dgeom(n,prob=p),lty=2)
```

    ## Warning in dgeom(n, prob = p): NaNs produced

``` r
abline(v=0.1)
legend(0.6,0.3,legend = c("Binomial","Geometric"), lty=c(1,2))
dev.off()
```

    ## quartz_off_screen 
    ##                 2
