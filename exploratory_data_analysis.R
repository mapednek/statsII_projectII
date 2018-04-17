library(ggplot2)

# Data cleaning resulted in numerous dervied columns that will be utilized instead of the original columns.
train <- read.csv(file="train.csv",head=TRUE,sep=",")
test <- read.csv(file="test.csv",head=TRUE,sep=",")

# Remove columns
train <- train[, -which(names(train) %in% c("workclass", "hours_per_week", "native_country", "capital_gain", "capital_loss"))]
test <- test[, -which(names(test) %in% c("workclass", "hours_per_week", "native_country", "capital_gain", "capital_loss"))]

# Look at Income Counts
summary(train$income)
ggplot(train, aes(x= income)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(x= "Income", y = "Percent") +
  scale_y_continuous(labels = scales::percent) + guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle=75,hjust=1))

ggplot(train, aes(x = income, fill = income)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y = (..count..)/sum(..count..) ), stat = "count", vjust = -.1) +
  labs(x = "Income", y = "", fill = "Income") + scale_y_continuous(labels = percent)


ggplot(train, aes(x= marital_status,  group=income)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(x= "Marital Status", y = "Percent", fill="Marital Status") + facet_grid(~income) +
  scale_y_continuous(labels = scales::percent) + guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle=75,hjust=1))
