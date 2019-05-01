#8.4-3
1-pbinom(16, 25, .5)
SIGN.test(jollyrancher$X, md = 5.9, alternative = "greater")
#Wilcoxon
wilcox.test(jollyrancher$X, mu = 5.9, alternative = "greater", exact = FALSE, correct = FALSE)
#t-test
t.test(jollyrancher$X, mu = 5.9, alternative = "greater")


#8.4-9
man1 = c(5.6, 4.6, 6.8, 4.9, 6.1, 5.3, 4.5, 5.8, 5.4, 4.7)
man2 = c(7.2, 8.1, 5.1, 7.3, 6.9, 7.8, 5.9, 6.7, 6.5, 7.1)
boxplot(man1, man2, horizontal = FALSE)
qqplot(man1, man2)
wilcox.test(man1, man2, correct = FALSE)

#Problem 3
stripchart(hw7data, vertical = TRUE)
kruskal.test(hw7data)
pchisq(5.99, 2, lower.tail = FALSE)
