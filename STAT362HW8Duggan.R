#HW8
#1a
w = c(Copy_of_Cauchy_Ex7_7_1$w)
samplemedian=  function(x, i) {
  
  return(median(x[i]))
  
} 
bootmedian= boot(w, samplemedian, 40)
bootmedian
plot(bootmedian)
boot.ci(bootmedian)
#b
samplemean=  function(x, i) {
  
  return(mean(x[i]))
  
} 
bootmean= boot(w, samplemean, 40)
plot(bootmean)
bootmean
boot.ci(bootmean)
#c
samplemeantrim=  function(x, i) {
  
  return(mean(x[i], trim = .375))
  
} 
bootmeantrim= boot(w, samplemeantrim, 40)
boot.ci(bootmeantrim)
bootmeantrim
plot(bootmeantrim)
#d
hist(w)
boxplot(w)

#2
samplevar=  function(x, i) {
  
  return(var(x[i]))
  
} 
samplevaradj=  function(x, i) {
  
  return(var(x[i])*((10000-1)/10000))
  
} 
bootvarw = boot(w, samplevar, 10000)
print(bootvarw)
plot(bootvarw)
bootvarwadj = boot(w, samplevaradj, 10000)
print(bootvarwadj)
plot(bootvarwadj)

#3
cropyield$moisture2 = cropyield$moisture^2

cropRAS.lm = lm(yield ~ moisture + temp + moisture2, data = cropyield)
cropRAS.lm
summary(cropRAS.lm)
#b
cropRAS.lo = loess(yield ~ moisture + temp, data = cropyield, degree = 2)
cropRAS.lo
cropRAS2.lo = loess(yield ~ moisture + temp, data = cropyield, degree = 1, span = 9/25)
cropRAS2.lo
summary(cropRAS.lm)
#c
pred1=data.frame(moisture=6, temp=20)
pred2=data.frame(moisture=14, temp=20)
pred3=data.frame(moisture=6, temp=24)
pred4=data.frame(moisture=14, temp=24)
predict(cropRAS.lo, pred1)
predict(cropRAS.lo, pred2)
predict(cropRAS.lo, pred3)
predict(cropRAS.lo, pred4)

lmpred1=data.frame(moisture=6, moisture2=36, temp=20)
lmpred2=data.frame(moisture=14, moisture2=196, temp=20)
lmpred3=data.frame(moisture=6, moisture2=36, temp=24)
lmpred4=data.frame(moisture=14, moisture2=196, temp=24)
predict(cropRAS.lm, lmpred1)
predict(cropRAS.lm, lmpred2)
predict(cropRAS.lm, lmpred3)
predict(cropRAS.lm, lmpred4)

#4
muscle1=rpart(mass ~ age, data = Muscle)   # default regression tree (we dont usually want just the default)
rpart.plot(muscle1)
rsq.rpart(muscle1)
plot(Muscle$age, Muscle$mass)
xmus = seq(40, 80, .1)         # create a sequence of x values to predict
musclepred = predict(muscle1, data.frame(age=xmus) )  # obtain predictions
lines(xmus, musclepred)
#b
muscle1b = rpart(mass ~ age, data = Muscle, minsplit = 7)  # grow a larger tree, "want to grow it all the way out and then prune it"
rpart.plot(muscle1b)
rsq.rpart(muscle1b)
plotcp(muscle1b)
prune1b = prune(muscle1b, cp = .045)# prune tree
rsq.rpart(prune1b)
rpart.plot(prune1b)
#c
xmus = seq(40, 80, .1)         # create a sequence of x values to predict
musclepred = predict(muscle1, data.frame(age=xmus) )  # obtain predictions
lines(xmus, musclepred)

plot(Muscle$age, Muscle$mass)
xmus = seq(40, 80, .1)         # create a sequence of x values to predict
musclepred2 = predict(prune1b, data.frame(age=xmus) )  # obtain predictions
lines(xmus, musclepred2)
