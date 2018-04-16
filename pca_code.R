#Principal Component Analysis

# Change categorical variables into numeric using one hot encoding
library(dummies)

new_train <- dummy.data.frame(train, names = c("workclass", "education", "marital_status",  "occupation", "relationship", "race", "sex", "native_country", "income", "workclass_category", "hours_per_week_category", "global_region", "capital_gain_category", "capital_loss_category"))
head(new_train)
prin_comp <- prcomp(new_train, scale. = T)
names(prin_comp)
prin_comp$center
prin_comp$rotation

#Principal components for all the 19 variables
prin_comp$rotation[1:19,1:4]
#The first four PC and the top five variables
prin_comp$rotation[1:5,1:4]


dim(prin_comp$x)
biplot(prin_comp, scale = 0)
biplot(prin_comp, expand=50, xlim=c(-0.2, 0.6), ylim=c(-0.5, 0.5))
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
# variance of the first ten principal compo
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]
screeplot(prin_comp,  type = "lines")
