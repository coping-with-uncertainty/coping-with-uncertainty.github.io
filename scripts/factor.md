---
layout: default
title: "Factor"
---

``` r
data = read.csv("btw2021kreis.csv", sep=";",skip=4)
data = data[-1,]
data = data[,c(1,7,8,9,10,11,12,13,14,15)]
BL = unique(data[,1])
#data[data$BL != "",] 
Data = c()
N = dim(data)[1]
for ( i in 1:16)
{
  index = (1:N)[data$Land == BL[i]]
  Datai = data[index[1],]
  if (length(index)>1)
  {
    for (j in 2:length(index))
    {
      Datai[,-1] = as.numeric(Datai[,-1]) + 
        as.numeric(data[index[j], -1] )
    }
  }
  Data = rbind(Data, Datai)
}

p = dim(Data)[2]
for (j in 2:p)
{
Data[,j] = as.numeric(Data[,j])
}
Data$CDU = Data$CDU + Data$CSU
Data[,3:10] = diag(1/Data[,2]) %*% as.matrix(Data[,3:10])

mod = factanal(Data[,c(3:8,10)], factors =3)
mod$loadings
```

    ## 
    ## Loadings:
    ##              Factor1 Factor2 Factor3
    ## CDU           0.995                 
    ## SPD                  -0.115  -0.990 
    ## AfD          -0.390   0.777   0.443 
    ## FDP           0.348  -0.249   0.192 
    ## DIE.LINKE    -0.825   0.315   0.303 
    ## GRÜNE                -0.996         
    ## FREIE.WÄHLER  0.572   0.313   0.419 
    ## 
    ##                Factor1 Factor2 Factor3
    ## SS loadings      2.273   1.868   1.488
    ## Proportion Var   0.325   0.267   0.213
    ## Cumulative Var   0.325   0.592   0.804

``` r
p = dim(data)[2]
for (j in 2:p)
{
  data[,j] = as.numeric(data[,j])
}
data$CDU = data$CDU + data$CSU
data[,3:10] = diag(1/data[,2]) %*% as.matrix(data[,3:10])


mod = factanal(data[,c(3:8,10)], factors =3)
mod$loadings
```

    ## 
    ## Loadings:
    ##              Factor1 Factor2 Factor3
    ## CDU          -0.696   0.653  -0.176 
    ## SPD          -0.139  -0.939  -0.307 
    ## AfD           0.812   0.230  -0.401 
    ## FDP                           0.335 
    ## DIE.LINKE     0.808  -0.108         
    ## GRÜNE        -0.282  -0.196   0.936 
    ## FREIE.WÄHLER          0.622  -0.182 
    ## 
    ##                Factor1 Factor2 Factor3
    ## SS loadings      1.907   1.800   1.308
    ## Proportion Var   0.272   0.257   0.187
    ## Cumulative Var   0.272   0.529   0.716
