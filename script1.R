library(readr)
confood2 <- read_csv("~/MSc DSBA/Stats/refresh/confood2.csv")
summary(confood2)

common_vars <- c("period", "storenum", "weeknum")
common <- confood2[common_vars]

brand1_vars <- c("saleb1", "apriceb1", "rpriceb1", "promotb1")
brand1 <- confood2[brand1_vars]

brand2_vars <- c("saleb2", "apriceb2", "rpriceb2", "promotb2")
brand2 <- confood2[brand2_vars]

brand3_vars <- c("saleb3", "apriceb3", "rpriceb3", "promotb3")
brand3 <- confood2[brand3_vars]

brand4_vars <- c("saleb4", "apriceb4", "rpriceb4", "promotb4")
brand4 <- confood2[brand4_vars]

brand5_vars <- c("saleb5", "apriceb5", "rpriceb5", "promotb5")
brand5 <- confood2[brand5_vars]

# combine dataframes
total1 <- cbind(brand1,common)

# informations
summary(confood2["saleb1"])

# plots
plot(confood2$apriceb1, confood2$saleb1)
plot(confood2$apriceb2, confood2$saleb2)
plot(confood2$apriceb3, confood2$saleb3)
plot(confood2$apriceb4, confood2$saleb4)
plot(confood2$apriceb5, confood2$saleb5)

# régression linéaire
lm(confood2$apriceb1 ~ confood2$saleb1)

# coefficient de corrélation
cor(confood2$apriceb1, confood2$saleb1)

# sort by period
confood2[confood2['period'] == 1]

# sales Brand 1 en fonction de la période
boxplot(confood2$saleb1 ~ confood2$period)
