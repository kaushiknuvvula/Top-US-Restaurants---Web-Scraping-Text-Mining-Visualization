library(dplyr)
library(ggplot2)
library(xlsx)

data <- read.csv('Yelp_New_Data.csv')

summary(data)
str(data)

quantile(data$Review_Count, c(1:10/10))
# finde the quantile of 100 in reviewcount
ecdf(data$Review_Count)(100)
#find the quantile of 4.5
ecdf(data$Rating)(4)

# since the dataset is too large, here we random sample the data
data10 <- sample_n(data, nrow(data)/10) %>% arrange(Review_Count)

p1 <- ggplot(data) + geom_point(aes(Rating, Review_Count), color = 'blue') + labs(title="Distribution", x="Rating", y="Review_Count")

p2 <- ggplot(data10) + geom_point(aes(Name, Review_Count), color = 'blue') + labs(title="Distribution of review count", x="Restaurant", y="Review count")

p3 <- ggplot(data10) + geom_point(aes(Name, Rating), color = 'blue') + labs(title="Distribution of review count", x="Restaurant", y="Rating")

median(data$Review_Count)
median(data$Rating)

"
data_restaurant <- data %>% filter(Rating > 4, Review_Count > 100)

data_groupby_city <- data %>% filter(Rating > 4, Review_Count > 100) %>%
        group_by(City) %>%
        summarise(count = n())

data_groupby_state <- data %>% filter(Rating > 4, Review_Count > 100) %>%
        group_by(State) %>%
        summarise(count = n())
"
#write.xlsx(data_groupby_city, file = 'data_groupby_city.xlsx')
#write.xlsx(data_groupby_state, file = 'data_groupby_state.xlsx')
#write.xlsx(data_restaurant, file = 'data_restaurant.xlsx')
