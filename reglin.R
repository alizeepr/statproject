# régression linéaire
lm(confood2$apriceb1 ~ confood2$saleb1)
m<- lm(confood2$apriceb1 ~ confood2$saleb1)
summary(m)
plot(confood2$saleb1, confood2$apriceb1)
a<- m$coefficients[1]
b<- m$coefficients[2]
abline(a, b)