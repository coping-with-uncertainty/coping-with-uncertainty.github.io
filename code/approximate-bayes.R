generate_pdf <- TRUE
if (generate_pdf && !dir.exists("plots")) dir.create("plots")

x = seq(0,10,by=0.1)
n = length(x)
y = rnorm(n, mean=x , sd=1 )
T = 1000
rho1 = 0.6
rho2 = 0.2
y_ts = (c( 0 , 0))
for ( i in 3:T)
{
  y_ts = c(y_ts, rho1 * y_ts[i-1] + rho2 * y_ts[i-2] + rnorm(1,sd=0.2))
}
if (generate_pdf) pdf("plots/plot-bayes-1.pdf", width = 6, height = 4)
plot(y_ts , type="l", las = 1, bty = "l", cex.lab = 1, cex.axis = 1, cex.main = 1, xlab = "Time", ylab = expression(y[t]))
if (generate_pdf) dev.off()
af = c(acf(y_ts, plot=FALSE)$acf)

dd = c()
theta = c()
YS = c()
for (j in 1:1000 )
{
  bool = FALSE
  while(bool == FALSE)
  {
      rho1s = -1+ 2 * runif(1)
      rho2s = -1 + 2* runif(1)
      bool = ( rho1s^2 + rho2s^2 < 1)
  }
  sigs = 0.2 #runif(1)
  ys = c(0,0)
  for ( i in 3:T)
  {
    ys = c(ys, rho1s * ys[i-1] + rho2s * ys[i-2] + rnorm(1,sd=sigs))
  }
  afs = c(acf(ys, plot=FALSE)$acf)
  d = max((afs[1:15]-af[1:15])^2)
#  d = mean( ( y_ts - ys)^2)
  if (d < 0.05)
  {
    print(j)
    YS = rbind(YS, t(ys))
    theta = rbind(theta, c(rho1s, rho2s, sigs, d))
  }
  dd = c(dd,d)
}


if (generate_pdf) pdf("plots/plot-bayes-2.pdf", width = 5, height = 5)
plot(theta[,1], theta[,2], ylim=c(-1,1), xlim=c(-1,1), las = 1, bty = "l", cex.lab = 1, cex.axis = 1, cex.main = 1, xlab = expression(rho[1]), ylab = expression(rho[2]))
px = sin(seq(0,2,by=0.01)*2*pi)
py = cos(seq(0,2,by=0.01)*2*pi)
polygon(px,py, col="lightblue")
points(theta[,1], theta[,2], las = 1, bty = "l")
abline(v=rho1)
abline(rho2,0)
if (generate_pdf) dev.off()

if (generate_pdf) pdf("plots/plot-bayes-3.pdf", width = 5, height = 4)
plot(x,y, las = 1, bty = "l", cex.lab = 1, cex.axis = 1, cex.main = 1, xlab = "x", ylab = "y")
if (generate_pdf) dev.off()
sig.s = var(lm(y~x)$residual)

dd =c()
YS = c()
theta = c()
for( i in 1:10000)
{
  sigs = 1 # exp(rnorm(1,sd=1))
  beta0s = rnorm(1, sd=3)
  beta1s = rnorm(1, sd=3)
  ys = rnorm( n, mean= beta0s + x * beta1s, sd=sigs )
  mods = fitted(lm(ys ~ x))
  d = abs( mean( (y - mods)^2) - sig.s )
  if (d < 2)
  {
    YS = rbind(YS, t(ys))
    theta = rbind(theta, c(beta0s, beta1s, sigs, d))
  }
  dd = c(dd,d)
}

if (generate_pdf) pdf("plots/plot-bayes-4.pdf", width = 5, height = 4)
plot(x,YS[7,], las = 1, bty = "l", cex.lab = 1, cex.axis = 1, cex.main = 1, xlab = "x", ylab = "y")   
points(x,y,col=2, las = 1, bty = "l")
if (generate_pdf) dev.off()
