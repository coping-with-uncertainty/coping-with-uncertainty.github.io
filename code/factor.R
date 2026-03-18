data = read.csv("../data/btw2021kreis.csv", sep=";",skip=4)
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


p = dim(data)[2]
for (j in 2:p)
{
  data[,j] = as.numeric(data[,j])
}
data$CDU = data$CDU + data$CSU
data[,3:10] = diag(1/data[,2]) %*% as.matrix(data[,3:10])


mod = factanal(data[,c(3:8,10)], factors =3)
mod$loadings


