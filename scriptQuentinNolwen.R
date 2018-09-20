library(dplyr)
library(magrittr)
library(ggplot2)

# create dataframe from confood

confood_df <- data.frame(confood)

# add columns for revenues & total sales for brand 1 to 5 & 

confood_df <- mutate(confood_df, 
       revenues_b1 = saleb1 * apriceb1,
       revenues_b2 = saleb2 * apriceb2,
       revenues_b3 = saleb3 * apriceb3,
       revenues_b4 = saleb4 * apriceb4,
       revenues_b5 = saleb5 * apriceb5,
       avg_price_2to4 = (apriceb2 + apriceb3 + apriceb4) / 3)

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

# B.2 Compute correlation coefficients to describe the linear relationship between :
# a. Number of units sold / price for each of the five brands (Note that b5 is biased)

correlations_sales_price <- confood_df %>%
  summarise(N = n(),
            cor_b1 = cor(saleb1, apriceb1, use ="pairwise.complete.obs"),
            cor_b2 = cor(saleb2, apriceb2, use ="pairwise.complete.obs"),
            cor_b3 = cor(saleb3, apriceb3, use ="pairwise.complete.obs"),
            cor_b4 = cor(saleb4, apriceb4, use ="pairwise.complete.obs"),
            cor_b5 = cor(saleb5, apriceb5, use ="pairwise.complete.obs"))


# b. Brand 1 sales and weighted average price for Brands 2 through 5.

correlations_sales_avgprice <- confood_df %>%
  summarise(N = n(),
            cor_sales_avgprice2to4 = cor(saleb1, avg_price_2to4, use ="pairwise.complete.obs"))

# c. All combinations of prices for the five brands.

correlations_combination_prices_b1to4 <- confood_df %>%
  summarise(N = n(),
            cor_b12 = cor(apriceb1, apriceb2, use ="pairwise.complete.obs"),
            cor_b13 = cor(apriceb1, apriceb3, use ="pairwise.complete.obs"),
            cor_b14 = cor(apriceb1, apriceb4, use ="pairwise.complete.obs"),
            cor_b23 = cor(apriceb2, apriceb3, use ="pairwise.complete.obs"),
            cor_b24 = cor(apriceb2, apriceb4, use ="pairwise.complete.obs"),
            cor_b34 = cor(apriceb3, apriceb4, use ="pairwise.complete.obs"))

# d. All combinations of quantity sold for the five brands.

correlations_combination_sales_b1to4 <- confood_df %>%
  summarise(N = n(),
            cor_b12 = cor(saleb1, saleb2, use ="pairwise.complete.obs"),
            cor_b13 = cor(saleb1, saleb3, use ="pairwise.complete.obs"),
            cor_b14 = cor(saleb1, saleb4, use ="pairwise.complete.obs"),
            cor_b23 = cor(saleb2, saleb3, use ="pairwise.complete.obs"),
            cor_b24 = cor(saleb2, saleb4, use ="pairwise.complete.obs"),
            cor_b34 = cor(saleb3, saleb4, use ="pairwise.complete.obs"))

# B.3 Scatter plot of the five sales versus price

# Brand 1

ggplot(confood_df, aes(x = apriceb1, y = saleb1)) + 
  geom_point()

# Brand 2

ggplot(confood_df, aes(x = apriceb2, y = saleb2)) + 
  geom_point()

# Brand 3

ggplot(confood_df, aes(x = apriceb3, y = saleb3)) + 
  geom_point()

# Brand 4

ggplot(confood_df, aes(x = apriceb4, y = saleb4)) + 
  geom_point()


# Brand 5

ggplot(confood_df, aes(x = apriceb5, y = saleb5)) + 
  geom_point()

# B.4 Scatter plot for Brand 1 sales versus the weighted average price of all competing brands.

ggplot(confood_df, aes(x=avg_price_2to4, y=saleb1)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE)

# B.5 Simple linear regression for Brand 1

ggplot(confood_df, aes(x = apriceb1 , y = saleb1)) + 
  geom_point() +
  geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95)

# B.5 Simple linear regression for sales of brand 1 vs weighted average price of brand 1 to 4

ggplot(confood_df, aes(x=saleb1, y=avg_price_2to4)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, level=0.95)

# B.6 Preparing a new array with sales grouped by promotions to indicate the relationship
# between quantity sold and type of promotion used.

confood_sales_promotion1 <- confood_df %>%
  group_by(promotb1) %>%
  summarise(mean(saleb1))

confood_sales_promotion2 <- confood_df %>%
  group_by(promotb2) %>%
  summarise(mean(saleb2))

confood_sales_promotion3 <- confood_df %>%
  group_by(promotb3) %>%
  summarise(mean(saleb3))

confood_sales_promotion4 <- confood_df %>%
  group_by(promotb4) %>%
  summarise(mean(saleb4))

confood_sales_promotion5 <- confood_df %>%
  group_by(promotb5) %>%
  summarise(mean(saleb5))


