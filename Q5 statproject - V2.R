install.packages("readr")
install.packages("dplyr")
install.packages("PASWR")
install.packages("purrr")
install.packages("gridExtra")
install.packages("reshape2")
library(PASWR)
library(BSDA)
library(readr)
library(ggplot2)
library(purrr)
library(dplyr)
library(gridExtra)
library(reshape2)

cf <- read_csv("~/statproject/confood2.csv")


 cf <- mutate(cf, marketshare = revenue1 / sum(revenue1, revenue2, revenue3, revenue4, revenue5))
 cf <- mutate(cf, revenue1 = saleb1 * apriceb1)
 cf <- mutate(cf, revenue2 = saleb2 * apriceb2)
 cf <- mutate(cf, revenue4 = saleb4 * apriceb4)
 cf <- mutate(cf, revenue3 = saleb3 * apriceb3)
 cf <- mutate(cf, revenue5 = saleb5 * apriceb5)
 
cf_agg <- cf %>% group_by(weeknum) 

sum = summarise(cf_agg, saleb1 = sum(saleb1), saleb2 = sum(saleb2), saleb4 = sum(saleb4), saleb3 = sum(saleb3),
                saleb5 = sum(saleb5), revenue1 = sum(revenue1), revenue2 = sum(revenue2), revenue4 = sum(revenue4),
                revenue3 = sum(revenue3), revenue5 = sum(revenue5))

Z_test_data <- summarise(sum, mean(saleb1), mean(saleb2),mean(saleb4), sd(saleb1), sd(saleb2), sd(saleb4))
 
# Test to validate or reject the claim that Brand2 is outperforming Brand1

z.test(sum$saleb2, sum$saleb1, sigma.x = sd(sum$saleb2), sigma.y = sd(sum$saleb1), alternative = "greater")

# Test to validate or reject the claim that Brand1 is outperforming Brand4

z.test(sum$saleb1, sum$saleb4, sigma.x = sd(sum$saleb1), sigma.y = sd(sum$saleb4), alternative = "greater") 

# Summary table of brand1 performance indicators

Summary_Brand1 <- modify_at(sum, c("saleb2","saleb4", "revenue2", "revenue4"), ~NULL)
Summary_Brand1 <- mutate(Summary_Brand1, A_price = revenue1 / saleb1)
Summary_Brand1 <- modify_at(Summary_Brand1, "M_share",~NULL)
Summary_Brand1 <- mutate(Summary_Brand1, M_share = (sum$revenue1 / (sum$revenue1+ sum$revenue2+ sum$revenue3+ sum$revenue4+ sum$revenue5)* 100))

s <- qplot(y = Summary_Brand1$saleb1, x=1, geom = "boxplot", main = "Variations in weekly sales",
           xlab = "2018", ylab = " Weekly sales", fill = I("blue")) 
r <- qplot(y = Summary_Brand1$revenue1, x=1, geom = "boxplot", main = "Variations in weekly revenue",
           xlab = "2018", ylab = " Weekly revenue", fill = I("red")) 
p <- qplot(y = Summary_Brand1$A_price, x=1, geom = "boxplot", main = "Variations in weekly average price",
           xlab = "2018", ylab = " Weekly average price", fill = I("green")) 
m <- qplot(y = Summary_Brand1$M_share, x=1, geom = "boxplot", main = "Variations in weekly market share",
           xlab = "2018", ylab = " Weekly market share", fill = I("grey"), xtick = element_blank())

grid.arrange(s,r,p,m, ncol = 2, nrow = 2)

sh <- ggplot(Summary_Brand1, aes(x=saleb1)) +
          geom_density(fill="blue", alpha = 0.5) +
          ggtitle("Variation in weekly sales") +
          xlab("Sales") +
          theme(plot.title = element_text(hjust = 0.5)) 
          
rh <- ggplot(Summary_Brand1, aes(x=revenue1)) +
  geom_density(fill="red", alpha = 0.5) +
  ggtitle("Variation in weekly revenue") +
  xlab("Revenue") +
  theme(plot.title = element_text(hjust = 0.5))

ph <- ggplot(Summary_Brand1, aes(x=A_price)) +
  geom_density(fill="green", alpha = 0.5) +
  ggtitle("Variation in weekly average price") +
  xlab("Average price") +
  theme(plot.title = element_text(hjust = 0.5))

mh <- ggplot(Summary_Brand1, aes(x=M_share)) +
  geom_density(fill="grey", alpha = 0.5) +
  ggtitle("Variation in weekly market share") +
  xlab("Market share") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(sh,rh,ph,mh, ncol = 2, nrow = 2)

# Comparison of sales 

comp <- summarise(sum, mean(saleb1), mean(saleb2), mean(saleb3), mean(saleb4), 
                  mean(revenue1), mean(revenue2), mean(revenue3), mean(revenue4),
                  mean(saleb5), mean(revenue5))

df= data.frame(Brand = c("Brand1", "Brand2", "Brand3", "Brand4", "Brand5"), sales = c(comp$`mean(saleb1)`, comp$`mean(saleb2)`, comp$`mean(saleb3)`
                                                       , comp$`mean(saleb4)`, comp$`mean(saleb5)`), revenue = c(comp$`mean(revenue1)`,
                                                              comp$`mean(revenue2)`,comp$`mean(revenue3)`,comp$`mean(revenue4)`,
                                                              comp$`mean(revenue5)`))
               
dfm <- melt(df,id.vars = 1)

ggplot(df, aes(x = reorder (Brand, -sales), y = sales)) + 
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  ggtitle("Average weekly sales per brand (in units)") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("") + ylab("sales in units")

)
