library(dplyr)
library(magrittr)
library(ggplot2)

confood_df <- data.frame(confood)
confood_df <- mutate(confood_df, 
                     revenuesb1 = saleb1 * apriceb1,
                     revenuesb2 = saleb2 * apriceb2,
                     revenuesb3 = saleb3 * apriceb3,
                     revenuesb4 = saleb4 * apriceb4,
                     revenuesb5 = saleb5 * apriceb5,
                     )
test <- summarise(confood_df, 
                  total_saleb1 = sum(saleb1), 
                  total_saleb2 = sum(saleb2), 
                  total_saleb3 = sum(saleb3),
                  total_saleb4 = sum(saleb4), 
                  total_saleb5 = sum(saleb5), 
                  sum_sales = sum(saleb1, saleb2, saleb3, saleb4, saleb5),
                  total_sales_2to5 = (sum_sales - total_saleb1),
                  average_price2to5 = mean(mean(apriceb1), mean(apriceb2), mean(apriceb3), mean(apriceb4)),
                  total_revenues1 = sum(revenuesb1),
                  total_revenues2 = sum(revenuesb2),
                  total_revenues3 = sum(revenuesb3),
                  total_revenues4 = sum(revenuesb4),
                  total_revenues5 = sum(revenuesb5),
                  market_shareb1 = total_revenues1 / sum(total_revenues1, total_revenues2, total_revenues3, total_revenues4, total_revenues5),
                  market_shareb2 = total_revenues2 / sum(total_revenues1, total_revenues2, total_revenues3, total_revenues4, total_revenues5),
                  market_shareb3 = total_revenues3 / sum(total_revenues1, total_revenues2, total_revenues3, total_revenues4, total_revenues5),
                  market_shareb4 = total_revenues4 / sum(total_revenues1, total_revenues2, total_revenues3, total_revenues4, total_revenues5),
                  market_shareb5 = total_revenues5 / sum(total_revenues1, total_revenues2, total_revenues3, total_revenues4, total_revenues5)
                  )

histogram_vector <- c(test$total_saleb1, test$total_sales_2to5)
histogram_df <- data.frame(histogram_vector)
geom_histogram(aes(y = histogram_df))


