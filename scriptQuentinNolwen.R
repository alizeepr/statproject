library(dplyr)
library(magrittr)
library(ggplot2)

# create dataframe from confood

confood_df <- data.frame(confood)

# add columns for revenues & total sales for brand 1 to 5

confood_df <- mutate(confood_df, 
       revenues_b1 = saleb1 * apriceb1,
       revenues_b2 = saleb2 * apriceb2,
       revenues_b3 = saleb3 * apriceb3,
       revenues_b4 = saleb4 * apriceb4,
       revenues_b5 = saleb5 * apriceb5)

confood_df <- mutate(confood_df,
       total_sale_b2to5 = saleb2 + saleb3 + saleb4 + saleb5)

# create descriptive stat dataframe from confood_df

descriptive_stat <- summarise(confood_df, 
          total_sales_b1 = sum(saleb1), 
          total_sales_b2 = sum(saleb2), 
          total_sales_b3 = sum(saleb3), 
          total_sales_b4 = sum(saleb4),
          total_sales_b5 = sum(saleb5),
          total_sales_market = sum(saleb1, saleb2, saleb3, saleb4, saleb5),
          total_sales_2to5 = sum(saleb2, saleb3, saleb4, saleb5),
          average_price_2to5 = mean(mean(apriceb1), mean(apriceb2), mean(apriceb3), mean(apriceb4), mean(apriceb5)),
          total_revenue_b1 = sum(revenues_b1),
          total_revenue_b2 = sum(revenues_b2),
          total_revenue_b3 = sum(revenues_b3),
          total_revenue_b4 = sum(revenues_b4),
          total_revenue_b5 = sum(revenues_b5),
          total_revenue_market = sum(total_revenue_b1, total_revenue_b2, total_revenue_b3, total_revenue_b4, total_revenue_b5),
          market_share_b1 = total_revenue_b1 / sum(total_revenue_market),
          market_share_b2 = total_revenue_b2 / sum(total_revenue_market),
          market_share_b3 = total_revenue_b3 / sum(total_revenue_market),
          market_share_b4 = total_revenue_b4 / sum(total_revenue_market),
          market_share_b5 = total_revenue_b5 / sum(total_revenue_market))

# create a 2d vector to print an histogram

histogram_vector_df <- data.frame(c(descriptive_stat$total_sales_b1, descriptive_stat$total_sales_2to5))
histogram_vector_df <- mutate(histogram_vector_df, name = c("Brand 1", "Brand 2 through 5"))

# print a histogram to see market share of brand 1 versus all other brands

ggplot(histogram_vector_df, aes(x = name, y = histogram_vector)) + 
  geom_bar(stat = "identity")

# new array grouped by week number

confood_grouped <- confood_df %>%
  group_by(weeknum) %>%
  summarise(sum_sales_b1 = sum(saleb1), total_sales_b2to5 = sum(total_sale_b2to5))

# plot the time series of sales for brand 1

ggplot(confood_grouped, aes(weeknum)) +
  geom_line(aes(y = sum_sales_b1, colour = "sales_b1")) +
  geom_line(aes(y = total_sales_b2to5, colour = "sales_b2to5"))

# new array with sales price weighted by storenum

confood_sales_weighted <- confood_df %>%
  group_by(weeknum) %>%
  summarise(avg_price_weekly_b1 = mean(apriceb1), 
            avg_price_weekly_b2 = mean(apriceb2),
            avg_price_weekly_b3 = mean(apriceb3),
            avg_price_weekly_b4 = mean(apriceb4),
            avg_price_weekly_b5 = mean(apriceb5),
            avg_price_weekly_b2to4 = (avg_price_weekly_b2 + 
                                          avg_price_weekly_b3 + 
                                          avg_price_weekly_b4) / 3)

# plot the average sales price of brand 1 and brand 2 to 4

ggplot(confood_sales_weighted, aes(weeknum)) +
  geom_line(aes(y = avg_price_weekly_b1, colour = "avg price b1")) +
  geom_line(aes(y = avg_price_weekly_b2to4, colour = "avg price b2 to 4"))

# group 52 weeks into 4 weeks periods 

confood_period_grouped <- confood_df %>%
  group_by(period) %>%
  summarise(sum_sales_b1 = sum(saleb1), total_sales_b2to5 = sum(total_sale_b2to5))

# plot the time series of sales for brand 1

ggplot(confood_period_grouped, aes(period)) +
  geom_line(aes(y = sum_sales_b1, colour = "sales_b1")) +
  geom_line(aes(y = total_sales_b2to5, colour = "sales_b2to5"))

# new array with period sales price weighted by storenum

confood_sales_weighted <- confood_df %>%
  group_by(period) %>%
  summarise(avg_price_weekly_b1 = mean(apriceb1), 
            avg_price_weekly_b2 = mean(apriceb2),
            avg_price_weekly_b3 = mean(apriceb3),
            avg_price_weekly_b4 = mean(apriceb4),
            avg_price_weekly_b5 = mean(apriceb5),
            avg_price_weekly_b2to4 = (avg_price_weekly_b2 + 
                                        avg_price_weekly_b3 + 
                                        avg_price_weekly_b4) / 3)

# plot the average sales price of brand 1 and brand 2 to 4 for the periods

ggplot(confood_sales_weighted, aes(period)) +
  geom_line(aes(y = avg_price_weekly_b1, colour = "avg price b1")) +
  geom_line(aes(y = avg_price_weekly_b2to4, colour = "avg price b2 to 4"))