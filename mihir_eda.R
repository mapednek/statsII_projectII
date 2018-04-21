library(ggplot2)
library(plyr)
library(gridExtra)
library(gmodels)
library(grid)
library(vcd)
library(scales)
library(ggthemes)

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# percent 50K
ggplot(data = train, mapping = aes(x = train$income, fill = train$income)) + 
  geom_bar(mapping = aes(y = (..count..)/sum(..count..))) +
  geom_text(mapping = aes(label = scales::percent((..count..)/sum(..count..)),
  y = (..count..)/sum(..count..) ), stat = "count", vjust = -.1) +
  theme(legend.position = 'none', axis.text.x=element_text(angle=75,hjust=1)) +
  labs(title="Percentage of 50K", x = "Income", y = "", fill = "Income") + scale_y_continuous(labels = percent)

# capital_gain_category
plot_gain <- lapply(X = levels(train$income), FUN = function(v){
  df <- subset(train, train$income == v)    
  df <- within(df, capital_gain_category <- factor(capital_gain_category, 
    levels = names(sort(table(capital_gain_category), decreasing = TRUE))))
  
  ggplot(data = df, aes(x = capital_gain_category, fill = capital_gain_category)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y = (..count..)/sum(..count..) ), 
      stat = "count", vjust = -.1) + labs(x = "Capital Gain", y = "", fill = "Capital Gain") +
    theme(legend.position = 'none', axis.text.x=element_text(angle=75,hjust=1)) +
    ggtitle(paste("Income", v, sep = "")) +  
    scale_y_continuous(labels = percent)
})
grid.arrange(grobs = plot_gain, ncol = 2)

# capital_loss_category
plot_loss <- lapply(X = levels(train$income), FUN = function(v){
  df <- subset(train, train$income == v)    
  df <- within(df, capital_loss_category <- factor(capital_loss_category, 
                                                   levels = names(sort(table(capital_loss_category), decreasing = TRUE))))
  
  ggplot(data = df, aes(x = capital_loss_category, fill = capital_loss_category)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y = (..count..)/sum(..count..) ), 
      stat = "count", vjust = -.1) + labs(x = "Capital Loss", y = "", fill = "Capital Loss") +
    theme(legend.position = 'none', axis.text.x=element_text(angle=75,hjust=1)) +
    ggtitle(paste("Income", v, sep = "")) +  
    scale_y_continuous(labels = percent)
})
grid.arrange(grobs = plot_loss, ncol = 2)

# workclass_category
plot_workclass_category <- lapply(X = levels(train$income), FUN = function(v){
  df <- subset(train, train$income == v)    
  df <- within(df, workclass_category <- factor(workclass_category, 
                                                levels = names(sort(table(workclass_category), decreasing = TRUE))))
  
  ggplot(data = df, aes(x = workclass_category, fill = workclass_category)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y = (..count..)/sum(..count..) ), 
              stat = "count", vjust = -.1) + labs(x = "Workclass", y = "", fill = "Workclass") +
    theme(legend.position = 'none', axis.text.x=element_text(angle=75,hjust=1)) +
    ggtitle(paste("Income", v, sep = "")) +  
    scale_y_continuous(labels = percent)
})
grid.arrange(grobs = plot_workclass_category, ncol = 2)

# hours_per_week_category
plot_hours_per_week_category <- lapply(X = levels(train$income), FUN = function(v){
  df <- subset(train, train$income == v)    
  df <- within(df, hours_per_week_category <- factor(hours_per_week_category, 
    levels = names(sort(table(hours_per_week_category), decreasing = TRUE))))
  
  ggplot(data = df, aes(x = hours_per_week_category, fill = hours_per_week_category)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y = (..count..)/sum(..count..) ), 
              stat = "count", vjust = -.1) + labs(x = "Work Hours Category", y = "", fill = "Work Hours Category") +
    theme(legend.position = 'none', axis.text.x=element_text(angle=75,hjust=1)) +
    ggtitle(paste("Income", v, sep = "")) +  
    scale_y_continuous(labels = percent)
})
grid.arrange(grobs = plot_hours_per_week_category, ncol = 2)

# global_region
plot_global_region <- lapply(X = levels(train$income), FUN = function(v){
  df <- subset(train, train$income == v)    
  df <- within(df, global_region <- factor(global_region, 
    levels = names(sort(table(global_region), decreasing = TRUE))))
  
  ggplot(data = df, aes(x = global_region, fill = global_region)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y = (..count..)/sum(..count..) ), 
      stat = "count", vjust = -.1) + labs(x = "Global Region", y = "", fill = "Global Region") +
    theme(legend.position = 'none', axis.text.x=element_text(angle=75,hjust=1)) +
    ggtitle(paste("Income", v, sep = "")) +  
    scale_y_continuous(labels = percent)
})
grid.arrange(grobs = plot_global_region, ncol = 2)