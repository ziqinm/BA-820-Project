options(stringsAsFactors = FALSE)

library(tidyverse)
library(zoo)

train_orig = read.csv("Original Data/train.csv")
glimpse(train_orig)

train_orig %>%
  mutate(date = paste0(YrSold, "-", MoSold)) -> ds
ds$date = as.Date(as.yearmon(ds$date))


ds %>% 
  group_by(date) %>%
  summarise(meanpriced = mean(SalePrice)) %>%
  ggplot(aes(x = date, y = meanpriced)) +
  geom_point() +
  geom_line(col = "#ff8700") +
  labs(y = "Average price",
       title = "Average price by year sold") +
  theme_bw()

age = read.csv("Subsets/train_dbl.csv")
glimpse(age)
age = cbind(age, ds$date, ds$SalePrice)
  
ggplot() + 
  geom_boxplot(age, mapping = aes(x = ds$date, y = ds$SalePrice, group = Age))

  
ggplot(age) + 
  geom_point(aes(x = Age, y = `ds$SalePrice`), alpha = 0.3) +
  labs(y = "Sale Price",
       title = "Age ~ Price") +
  theme_bw()
