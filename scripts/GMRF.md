---
layout: default
title: "GMRF"
---

``` r
# Script: Gaussian_Markov_Random_Field.R
# Description: This script simulates a Gaussian Markov Random Field (GMRF)
# and visualizes its evolution.

# Flag to control PDF generation
generate_pdf <- FALSE  # Set to TRUE to save plots

# Define function for transformation
ftrans <- function(x) {
  return((x <= 0.5) * 0.5 * ((2 * x)**(1/2)) +
           (x > 0.5) * (1 - 0.5 * (2 * (1 - x))**(1/2)))
}

# Grid size and parameters
Nt <- 500
N <- 100

# Generate transformed grid
Y <- seq(0, 1, by = 1 / (Nt - 1))
Y1 <- kronecker(Y, matrix(1, Nt, 1))
Y2 <- kronecker(matrix(1, Nt, 1), Y)
Xt <- matrix(ftrans((exp(-1/2 * (Y1 - 0.5)^2 * 1) * 
                       exp(-((Y1 - 0.5)^2) * ((Y2 - 0.5)^2) * 100))**(1/4)), nrow = Nt)

# Generate smaller transformed grid
Y <- seq(0, 1, by = 1 / (N - 1))
Y1 <- kronecker(Y, matrix(1, N, 1))
Y2 <- kronecker(matrix(1, N, 1), Y)
X <- matrix(ftrans(exp(-1/2 * (Y1 - 0.5)^2 * 1) * 
                     exp(-((Y1 - 0.5)^2) * ((Y2 - 0.5)^2) * 100)), nrow = N)

# Plot 1: Initial transformed grid
if (generate_pdf) pdf("plot-gmrf-1.pdf", height = 5, width = 5)
image(Xt)
```

![](GMRF_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
if (generate_pdf) dev.off()

# Plot 2: Contour of transformed grid
if (generate_pdf) pdf("plot-gmrf-2.pdf", height = 5, width = 5)
contour(Xt)
```

![](GMRF_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
if (generate_pdf) dev.off()

# Define neighborhood points
ns <- 5
Neig.fix <- cbind(N * kronecker(matrix(seq(1 / (2 * ns), (1 - 1 / (2 * ns)), by = 1 / ns)), 
                                matrix(1, ns, 1)),
                  N * kronecker(matrix(1, ns, 1),
                                seq(1 / (2 * ns), (1 - 1 / (2 * ns)), by = 1 / ns)))
Neig.fix <- round(Neig.fix, digits = 0)

# Plot 3: Neighborhood points
if (generate_pdf) pdf("plot-gmrf-3.pdf", height = 5, width = 5)
image(Xt)
points(Neig.fix[,1] / N, Neig.fix[,2] / N, pch = 16, cex = 2)
```

![](GMRF_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
if (generate_pdf) dev.off()

# Function to filter valid neighbors
all.pos <- function(Neig, N) {
  for (i in length(Neig):1) {
    if ((min(Neig[[i]]) == 0) | (max(Neig[[i]]) == (N + 1))) {
      Neig <- Neig[-i]
    }
  }
  return(Neig)
}

# Simulation parameters
T <- 1000
Xsim <- array(NA, c(N, N, T))

# GMRF simulation loop
for (t in 1:T) {
  for (i in 1:N) {
    for (j in 1:N) {
      if (!any((Neig.fix[,1] == i) & (Neig.fix[,2] == j))) {
        Neig <- list(c(i, j - 1), c(i + 1, j - 1), c(i + 1, j), c(i + 1, j + 1),
                     c(i, j + 1), c(i - 1, j + 1), c(i - 1, j), c(i - 1, j - 1))
        Neig <- all.pos(Neig, N)
        x <- sapply(Neig, function(n) X[n[1], n[2]])
        X[i, j] <- if (t == 1) 0.75 + rnorm(1, sd = 0.2) else mean(x) + rnorm(1, sd = 0.1 / log(t))
      }
    }
  }
  Xsim[,,t] <- X
  if (t %% 10 == 0) {
    print(t)
    image(X, zlim = c(-0.2, 1.2))
  }
}
```

    ## [1] 10

![](GMRF_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

    ## [1] 20

![](GMRF_files/figure-gfm/unnamed-chunk-1-5.png)<!-- -->

    ## [1] 30

![](GMRF_files/figure-gfm/unnamed-chunk-1-6.png)<!-- -->

    ## [1] 40

![](GMRF_files/figure-gfm/unnamed-chunk-1-7.png)<!-- -->

    ## [1] 50

![](GMRF_files/figure-gfm/unnamed-chunk-1-8.png)<!-- -->

    ## [1] 60

![](GMRF_files/figure-gfm/unnamed-chunk-1-9.png)<!-- -->

    ## [1] 70

![](GMRF_files/figure-gfm/unnamed-chunk-1-10.png)<!-- -->

    ## [1] 80

![](GMRF_files/figure-gfm/unnamed-chunk-1-11.png)<!-- -->

    ## [1] 90

![](GMRF_files/figure-gfm/unnamed-chunk-1-12.png)<!-- -->

    ## [1] 100

![](GMRF_files/figure-gfm/unnamed-chunk-1-13.png)<!-- -->

    ## [1] 110

![](GMRF_files/figure-gfm/unnamed-chunk-1-14.png)<!-- -->

    ## [1] 120

![](GMRF_files/figure-gfm/unnamed-chunk-1-15.png)<!-- -->

    ## [1] 130

![](GMRF_files/figure-gfm/unnamed-chunk-1-16.png)<!-- -->

    ## [1] 140

![](GMRF_files/figure-gfm/unnamed-chunk-1-17.png)<!-- -->

    ## [1] 150

![](GMRF_files/figure-gfm/unnamed-chunk-1-18.png)<!-- -->

    ## [1] 160

![](GMRF_files/figure-gfm/unnamed-chunk-1-19.png)<!-- -->

    ## [1] 170

![](GMRF_files/figure-gfm/unnamed-chunk-1-20.png)<!-- -->

    ## [1] 180

![](GMRF_files/figure-gfm/unnamed-chunk-1-21.png)<!-- -->

    ## [1] 190

![](GMRF_files/figure-gfm/unnamed-chunk-1-22.png)<!-- -->

    ## [1] 200

![](GMRF_files/figure-gfm/unnamed-chunk-1-23.png)<!-- -->

    ## [1] 210

![](GMRF_files/figure-gfm/unnamed-chunk-1-24.png)<!-- -->

    ## [1] 220

![](GMRF_files/figure-gfm/unnamed-chunk-1-25.png)<!-- -->

    ## [1] 230

![](GMRF_files/figure-gfm/unnamed-chunk-1-26.png)<!-- -->

    ## [1] 240

![](GMRF_files/figure-gfm/unnamed-chunk-1-27.png)<!-- -->

    ## [1] 250

![](GMRF_files/figure-gfm/unnamed-chunk-1-28.png)<!-- -->

    ## [1] 260

![](GMRF_files/figure-gfm/unnamed-chunk-1-29.png)<!-- -->

    ## [1] 270

![](GMRF_files/figure-gfm/unnamed-chunk-1-30.png)<!-- -->

    ## [1] 280

![](GMRF_files/figure-gfm/unnamed-chunk-1-31.png)<!-- -->

    ## [1] 290

![](GMRF_files/figure-gfm/unnamed-chunk-1-32.png)<!-- -->

    ## [1] 300

![](GMRF_files/figure-gfm/unnamed-chunk-1-33.png)<!-- -->

    ## [1] 310

![](GMRF_files/figure-gfm/unnamed-chunk-1-34.png)<!-- -->

    ## [1] 320

![](GMRF_files/figure-gfm/unnamed-chunk-1-35.png)<!-- -->

    ## [1] 330

![](GMRF_files/figure-gfm/unnamed-chunk-1-36.png)<!-- -->

    ## [1] 340

![](GMRF_files/figure-gfm/unnamed-chunk-1-37.png)<!-- -->

    ## [1] 350

![](GMRF_files/figure-gfm/unnamed-chunk-1-38.png)<!-- -->

    ## [1] 360

![](GMRF_files/figure-gfm/unnamed-chunk-1-39.png)<!-- -->

    ## [1] 370

![](GMRF_files/figure-gfm/unnamed-chunk-1-40.png)<!-- -->

    ## [1] 380

![](GMRF_files/figure-gfm/unnamed-chunk-1-41.png)<!-- -->

    ## [1] 390

![](GMRF_files/figure-gfm/unnamed-chunk-1-42.png)<!-- -->

    ## [1] 400

![](GMRF_files/figure-gfm/unnamed-chunk-1-43.png)<!-- -->

    ## [1] 410

![](GMRF_files/figure-gfm/unnamed-chunk-1-44.png)<!-- -->

    ## [1] 420

![](GMRF_files/figure-gfm/unnamed-chunk-1-45.png)<!-- -->

    ## [1] 430

![](GMRF_files/figure-gfm/unnamed-chunk-1-46.png)<!-- -->

    ## [1] 440

![](GMRF_files/figure-gfm/unnamed-chunk-1-47.png)<!-- -->

    ## [1] 450

![](GMRF_files/figure-gfm/unnamed-chunk-1-48.png)<!-- -->

    ## [1] 460

![](GMRF_files/figure-gfm/unnamed-chunk-1-49.png)<!-- -->

    ## [1] 470

![](GMRF_files/figure-gfm/unnamed-chunk-1-50.png)<!-- -->

    ## [1] 480

![](GMRF_files/figure-gfm/unnamed-chunk-1-51.png)<!-- -->

    ## [1] 490

![](GMRF_files/figure-gfm/unnamed-chunk-1-52.png)<!-- -->

    ## [1] 500

![](GMRF_files/figure-gfm/unnamed-chunk-1-53.png)<!-- -->

    ## [1] 510

![](GMRF_files/figure-gfm/unnamed-chunk-1-54.png)<!-- -->

    ## [1] 520

![](GMRF_files/figure-gfm/unnamed-chunk-1-55.png)<!-- -->

    ## [1] 530

![](GMRF_files/figure-gfm/unnamed-chunk-1-56.png)<!-- -->

    ## [1] 540

![](GMRF_files/figure-gfm/unnamed-chunk-1-57.png)<!-- -->

    ## [1] 550

![](GMRF_files/figure-gfm/unnamed-chunk-1-58.png)<!-- -->

    ## [1] 560

![](GMRF_files/figure-gfm/unnamed-chunk-1-59.png)<!-- -->

    ## [1] 570

![](GMRF_files/figure-gfm/unnamed-chunk-1-60.png)<!-- -->

    ## [1] 580

![](GMRF_files/figure-gfm/unnamed-chunk-1-61.png)<!-- -->

    ## [1] 590

![](GMRF_files/figure-gfm/unnamed-chunk-1-62.png)<!-- -->

    ## [1] 600

![](GMRF_files/figure-gfm/unnamed-chunk-1-63.png)<!-- -->

    ## [1] 610

![](GMRF_files/figure-gfm/unnamed-chunk-1-64.png)<!-- -->

    ## [1] 620

![](GMRF_files/figure-gfm/unnamed-chunk-1-65.png)<!-- -->

    ## [1] 630

![](GMRF_files/figure-gfm/unnamed-chunk-1-66.png)<!-- -->

    ## [1] 640

![](GMRF_files/figure-gfm/unnamed-chunk-1-67.png)<!-- -->

    ## [1] 650

![](GMRF_files/figure-gfm/unnamed-chunk-1-68.png)<!-- -->

    ## [1] 660

![](GMRF_files/figure-gfm/unnamed-chunk-1-69.png)<!-- -->

    ## [1] 670

![](GMRF_files/figure-gfm/unnamed-chunk-1-70.png)<!-- -->

    ## [1] 680

![](GMRF_files/figure-gfm/unnamed-chunk-1-71.png)<!-- -->

    ## [1] 690

![](GMRF_files/figure-gfm/unnamed-chunk-1-72.png)<!-- -->

    ## [1] 700

![](GMRF_files/figure-gfm/unnamed-chunk-1-73.png)<!-- -->

    ## [1] 710

![](GMRF_files/figure-gfm/unnamed-chunk-1-74.png)<!-- -->

    ## [1] 720

![](GMRF_files/figure-gfm/unnamed-chunk-1-75.png)<!-- -->

    ## [1] 730

![](GMRF_files/figure-gfm/unnamed-chunk-1-76.png)<!-- -->

    ## [1] 740

![](GMRF_files/figure-gfm/unnamed-chunk-1-77.png)<!-- -->

    ## [1] 750

![](GMRF_files/figure-gfm/unnamed-chunk-1-78.png)<!-- -->

    ## [1] 760

![](GMRF_files/figure-gfm/unnamed-chunk-1-79.png)<!-- -->

    ## [1] 770

![](GMRF_files/figure-gfm/unnamed-chunk-1-80.png)<!-- -->

    ## [1] 780

![](GMRF_files/figure-gfm/unnamed-chunk-1-81.png)<!-- -->

    ## [1] 790

![](GMRF_files/figure-gfm/unnamed-chunk-1-82.png)<!-- -->

    ## [1] 800

![](GMRF_files/figure-gfm/unnamed-chunk-1-83.png)<!-- -->

    ## [1] 810

![](GMRF_files/figure-gfm/unnamed-chunk-1-84.png)<!-- -->

    ## [1] 820

![](GMRF_files/figure-gfm/unnamed-chunk-1-85.png)<!-- -->

    ## [1] 830

![](GMRF_files/figure-gfm/unnamed-chunk-1-86.png)<!-- -->

    ## [1] 840

![](GMRF_files/figure-gfm/unnamed-chunk-1-87.png)<!-- -->

    ## [1] 850

![](GMRF_files/figure-gfm/unnamed-chunk-1-88.png)<!-- -->

    ## [1] 860

![](GMRF_files/figure-gfm/unnamed-chunk-1-89.png)<!-- -->

    ## [1] 870

![](GMRF_files/figure-gfm/unnamed-chunk-1-90.png)<!-- -->

    ## [1] 880

![](GMRF_files/figure-gfm/unnamed-chunk-1-91.png)<!-- -->

    ## [1] 890

![](GMRF_files/figure-gfm/unnamed-chunk-1-92.png)<!-- -->

    ## [1] 900

![](GMRF_files/figure-gfm/unnamed-chunk-1-93.png)<!-- -->

    ## [1] 910

![](GMRF_files/figure-gfm/unnamed-chunk-1-94.png)<!-- -->

    ## [1] 920

![](GMRF_files/figure-gfm/unnamed-chunk-1-95.png)<!-- -->

    ## [1] 930

![](GMRF_files/figure-gfm/unnamed-chunk-1-96.png)<!-- -->

    ## [1] 940

![](GMRF_files/figure-gfm/unnamed-chunk-1-97.png)<!-- -->

    ## [1] 950

![](GMRF_files/figure-gfm/unnamed-chunk-1-98.png)<!-- -->

    ## [1] 960

![](GMRF_files/figure-gfm/unnamed-chunk-1-99.png)<!-- -->

    ## [1] 970

![](GMRF_files/figure-gfm/unnamed-chunk-1-100.png)<!-- -->

    ## [1] 980

![](GMRF_files/figure-gfm/unnamed-chunk-1-101.png)<!-- -->

    ## [1] 990

![](GMRF_files/figure-gfm/unnamed-chunk-1-102.png)<!-- -->

    ## [1] 1000

![](GMRF_files/figure-gfm/unnamed-chunk-1-103.png)<!-- -->

``` r
# Plot 4: First simulation frame
if (generate_pdf) pdf("plot-gmrf-4.pdf", height = 5, width = 5)
image(Xsim[,,1])
```

![](GMRF_files/figure-gfm/unnamed-chunk-1-104.png)<!-- -->

``` r
if (generate_pdf) dev.off()

# Plot 5: Last simulation frame
if (generate_pdf) pdf("plot-gmrf-5.pdf", height = 5, width = 5)
image(Xsim[,,T])
```

![](GMRF_files/figure-gfm/unnamed-chunk-1-105.png)<!-- -->

``` r
if (generate_pdf) dev.off()

# Plot 6: Maximum over last 100 frames
if (generate_pdf) pdf("plot-gmrf-6.pdf", height = 5, width = 5)
Xmax <- apply(Xsim[,,(T - 100):T], MARGIN = c(1,2), FUN = max)
image(Xmax)
```

![](GMRF_files/figure-gfm/unnamed-chunk-1-106.png)<!-- -->

``` r
if (generate_pdf) dev.off()

# Plot 7: Mean over last 100 frames
if (generate_pdf) pdf("plot-gmrf-7.pdf", height = 5, width = 5)
Xmean <- apply(Xsim[,,(T - 100):T], MARGIN = c(1,2), FUN = mean)
image(Xmean)
```

![](GMRF_files/figure-gfm/unnamed-chunk-1-107.png)<!-- -->

``` r
if (generate_pdf) dev.off()

# Plot 8: Standard deviation over last 100 frames
if (generate_pdf) pdf("plot-gmrf-8.pdf", height = 5, width = 5)
Xvar <- apply(Xsim[,,(T - 100):T], MARGIN = c(1,2), FUN = var)
image(sqrt(Xvar), zlim = c(0, max(sqrt(Xvar))))
```

![](GMRF_files/figure-gfm/unnamed-chunk-1-108.png)<!-- -->

``` r
if (generate_pdf) dev.off()
```
