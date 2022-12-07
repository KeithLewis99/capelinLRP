#### this seems to wrok
svBH <- srStarts(age2 ~ biomass, data = na.omit(srBH), type = "BevertonHolt", na.rm=T)


svBH <- srStarts(age2~biomass,data=na.omit(sr),type="BevertonHolt",2)
srFuns("BevertonHolt",2)
bhr <- srFuns("BevertonHolt" ,2)
bhr(S=135, a = svBH$a, svBH$Rp)

srBH1 <- nls(age2 ~ bhr(biomass, a, Rp), data = na.omit(sr), start=svBH)

cbind(estimates=coef(srBH1), confint(srBH1))
coef_srBH <- coef(srBH1)
bhr(S=135, a = coef(srBH1))
a <- coef_srBH[[1]]
b <- coef_srBH[[2]]

# my code
#plot(sr$biomass, log(sr$R), ylim=c(0,12))
plot(sr$biomass, sr$age2)

# bootstrap from book
## note that this works for the figure!!!
x <- seq(0, max(srBH$biomass, na.rm = T), 1)
pBH <- bhr(x, a = coef(srBH1))
lines(pBH~x, col="red")

