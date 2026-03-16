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
dat = as.data.frame(tab)
colnames(dat) = c("floor.space","center","new.contract","district","Freq" )

ind2 = loglm(Freq ~ floor.space + center + new.contract + district, 
             data=dat)
full =  loglm(Freq ~ floor.space* center * new.contract * district, 
              data=dat)
mod = step(ind2, scope=list(lower=formula(ind2),upper=formula(full)),
           direction="forward", k = log(n))
summary(mod)

loglm(~)