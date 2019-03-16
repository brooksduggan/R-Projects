#9.1-5a
dbinom(0, 3, .5)
dbinom(1, 3, .5)
dbinom(2, 3, .5)
dbinom(3, 3, .5)
qchisq(.975, df=3) 
#9.1-5b
exp = c(.125*52, .375*52, .375*52, .125*52)
obs = c(5, 17, 24, 6)
chisqu = sum((obs - exp)^2 / exp)
chisqu
qchisq(.95, df=3) 
pchisq(7.185, 3, lower.tail = FALSE)
#9.1-10a
infect = c(263, 238, 226, 220, 170, 155, 139, 123, 119, 107, 107, 97, 90, 90, 90, 79, 75, 74, 71, 66, 60, 55, 47, 47, 47, 45, 43, 41, 40, 39, 38, 38, 35, 32, 32, 28, 19, 10, 10)
control = c(314, 300, 274, 246, 190, 186, 185, 182, 180, 141, 132, 129, 110, 100, 95, 95, 93, 83, 55, 52, 50, 48, 48, 44, 40, 32, 30, 25, 24, 18, 7)
mean(infect)
mean(control)
sd(infect)
sd(control)
#9.1-10b
boxplot(infect, control)
#9.1-10c
qexp(.2, 1/mean(control))
qexp(.4, 1/mean(control))
qexp(.6, 1/mean(control))
qexp(.8, 1/mean(control))
obs2 = c(4, 9, 5, 6, 7)
exp2 = c(6.2, 6.2, 6.2, 6.2, 6.2)
chisqu2 = sum((obs2 - exp2)^2 / exp2)
chisqu2
qchisq(.95, df=1)
#9.1-11
p1 = pnorm(303.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts))
p2 = pnorm(307.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts))-pnorm(303.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts)) #50-100
p3 = pnorm(311.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts))-pnorm(307.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts))
p4 = pnorm(315.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts))-pnorm(311.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts))
p5 = pnorm(319.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts))-pnorm(315.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts))
p6 = pnorm(323.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts))-pnorm(319.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts))
p7 = pnorm(327.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts))-pnorm(323.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts))
p8 = pnorm(331.5, mean(meltingpoint$meltpts), sd = sd(meltingpoint$meltpts))
obs5 = c(p1, p2, p3, p4, p5, p6, p7, p8)
exp5 = c(1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8)
chisqu5 = sum((obs5 - exp5)^2 / exp5)
chisqu5
qchisq(.95, df=5)
#9.2-1
obs3 = c(95, 53, 36, 26, 71,  43, 21, 18, 45, 32, 32, 28)
exp3 = c(88.8, 59.2, 37.2, 24.8, 68.4,  45.6, 23.4, 15.6, 46.2, 30.8, 36, 24)
chisqu3 = sum((obs3 - exp3)^2 / exp3)
chisqu3
qchisq(.95, df=5)
#9.2-2
obs4 = c(95, 53, 130, 36, 26, 75, 71,  43, 136, 21, 18, 33, 45, 32, 61, 32, 28, 65)
exp4 = c(83.4, 55.6, 139, 41.1, 27.4, 68.5, 75, 50, 125, 21.6, 14.4, 36, 41.4, 27.6, 69, 37.5, 25, 62.5)
chisqu4 = sum((obs4 - exp4)^2 / exp4)
chisqu4
qchisq(.95, df=10)
#9.2-11
chisq.test(gpa)
