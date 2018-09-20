confoodDF <- as.data.frame(confood2)

#group of brand 2 to 5
#sales
colnames <- c("saleb2", "saleb3", "saleb4", "saleb5")
confoodDF[colnames]
confoodDF$salesO <- apply(confoodDF[colnames], 1, sum)
confoodDF$salesOmean <- apply(confoodDF[colnames], 1, mean)
#rprice
colnames <- c("rpriceb2", "rpriceb3", "rpriceb4", "rpriceb5")
confoodDF$rpriceO <- apply(confoodDF[colnames], 1, sum)
confoodDF$rpriceOmean <- apply(confoodDF[colnames], 1, mean)
#aprice
colnames <- c("apriceb2", "apriceb3", "apriceb4", "apriceb5")
confoodDF$apriceO <- apply(confoodDF[colnames], 1, sum)
confoodDF$apriceOmean <- apply(confoodDF[colnames], 1, mean)

#confood 1 vs other
colnames <- c("period", "storenum", "weeknum", "saleb1", "apriceb1", "rpriceb1",
              "promotb1", "salesOmean", "apriceOmean", "rpriceOmean")
confood1 <- confoodDF[colnames]

# régression linéaire
lm(confood2$apriceb1 ~ confood2$saleb1)
m<- lm(confood2$apriceb1 ~ confood2$saleb1)
summary(m)
plot(confood2$saleb1, confood2$apriceb1)
a<- m$coefficients[1]
b<- m$coefficients[2]
abline(a, b)

#change promotion variable to dummy


#corrélation
M <- cor(confood1[[storenum==1]])
corrplot(M, method='color')