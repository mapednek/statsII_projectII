library("ggplot2")
library("dplyr")

#read file, NA values in file are denoted by question marks
adult <- read.table("adult.data", sep = ",", header = FALSE, na.strings = " ?")

#set column names
colnames(adult) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")
str(adult)

#remove unnecessary columns, there is only one value thus is unncessary
adult$fnlwgt <- NULL

#Remove NAs
#based off visual inspection of the data, there are a number of NA values. Lets first find the number of rows with atleast one NA value
subsetOfNAData <- adult[!complete.cases(adult),]
summary(subsetOfNAData)
numberNARows <- nrow(adult[!complete.cases(adult),])
percentNARows <- (numberNARows / nrow(adult)) * 100
percentNARows
# Based off the small percentage of NA data and the overall number of rows in the dataset (32561), I am going to make the decisison that removing the rows with NA values will be more beneficial then making assumptions on filling those values.
#remove NA rows
adult <- na.omit(adult)
#re-number row names of dataframe
row.names(adult) <- 1:nrow(adult)


# for each categorical variable we want to analyze if categories can be combined or if levels need to be recoded or dropped:

# I. for workclass:
summary(adult$workclass)
adult$workclass <- droplevels(adult$workclass)
levels(adult$workclass)

adult$workclass_category <- adult$workclass

# combine into Government job
adult$workclass_category <- gsub(' Federal-gov', 'Government', adult$workclass_category)
adult$workclass_category <- gsub(' Local-gov', 'Government', adult$workclass_category)
adult$workclass_category <- gsub(' State-gov', 'Government', adult$workclass_category) 

# combine into Sele-Employed job
adult$workclass_category <- gsub(' Self-emp-inc', 'Self-Employed', adult$workclass_category)
adult$workclass_category <- gsub(' Self-emp-not-inc', 'Self-Employed', adult$workclass_category)

adult$workclass_category <- as.factor(adult$workclass_category)


# II. for hours per week
summary(adult$hours_per_week)
ggplot(adult) + aes(x=hours_per_week, group=income, fill=income) + geom_histogram(binwidth = 5)

adult$hours_per_week_category[adult$hours_per_week < 30] <- "part_time"
adult$hours_per_week_category[adult$hours_per_week >= 30 & adult$hours_per_week <= 37] <- "fringe_fulltime"
adult$hours_per_week_category[adult$hours_per_week > 37 & adult$hours_per_week <= 45  ] <- "regular_fulltime"
adult$hours_per_week_category[adult$hours_per_week > 45 & adult$hours_per_week <= 60  ] <- "overtime"
adult$hours_per_week_category[adult$hours_per_week > 60] <- "extreme_overtime"
adult$hours_per_week_category <- as.factor(adult$hours_per_week_category)

# III. separate native region into column by global regions
east_asia <- c(" Cambodia", " China", " Hong", " Laos", " Thailand", " Japan", " Taiwan", " Vietnam", " Philippines")
central_subcontinent_asia <- c(" India", " Iran")
central_carribean_america <- c(" Cuba", " Guatemala", " Jamaica", " Nicaragua", " Puerto-Rico",  " Dominican-Republic", " El-Salvador", " Haiti", " Honduras", " Mexico", " Trinadad&Tobago")
south_america <- c(" Ecuador", " Peru", " Columbia")
western_europe <- c(" England", " Germany", " Holand-Netherlands", " Ireland", " France", " Greece", " Italy", " Portugal", " Scotland")
eastern_europe <- c(" Poland", " Yugoslavia", " Hungary")
united_states <- c(" United-States", " Outlying-US(Guam-USVI-etc)")

adult$global_region[adult$native_country %in% east_asia] <- "east_asia"
adult$global_region[adult$native_country %in% central_subcontinent_asia] <- "central_subcontinent_asia"
adult$global_region[adult$native_country %in% central_carribean_america] <- "central_carribean_america"
adult$global_region[adult$native_country %in% south_america] <- "south_america"
adult$global_region[adult$native_country %in% western_europe] <- "western_europe"
adult$global_region[adult$native_country %in% eastern_europe] <- "eastern_europe"
adult$global_region[adult$native_country %in% united_states] <- "united_states"
adult$global_region[adult$native_country == " Canada"] <- "canada"
adult$global_region[adult$native_country == " South"] <- "country_labeled_as_south"
adult$global_region <- as.factor(adult$global_region)
