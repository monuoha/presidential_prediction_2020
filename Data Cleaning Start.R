require(readr)  # for reading data into R
library(dplyr)  # for a set of really useful data selection tools
data <- read_csv('president_polls.csv')
attach(data)  # eliminates the need for data$....
data = select(data, state, pollster, sponsors, fte_grade, sample_size, methodology, start_date, end_date, answer, pct)  # select the useful data
data$state = as.factor(data$state)  # make into factor
data$pollster = as.factor(data$pollster)  # make into factor
data$sponsors = as.factor(data$sponsors)  # make into factor
data$fte_grade = as.factor(data$fte_grade)  # make into factor
data$methodology = as.factor(data$methodology)  # make into factor
data$start_date = as.Date(data$start_date, format = "%m/%d/%y")  # format tells you how to read the dates. In our case is month/day/two-number-year or %m/%d/%y 
data$end_date = as.Date(data$end_date, format = "%m/%d/%y")  # format tells you how to read the dates. In our case is month/day/two-number-year or %m/%d/%y
data$answer = as.factor(data$answer)  # make into factor

summary(data)  # lets summarize


##### What needs to be done to the data next? #####
# delete NAs for state, fte_grade, methodology! 
data_1 = data[complete.cases(data$state),] #complete.cases revoves rows containing NAs
summary(data_1)
data_2 = data_1[complete.cases(data_1$fte_grade),]
summary(data_2)
clean_data = data_2[complete.cases(data_2$methodology),]
summary(clean_data) #clean_data is the modified datasets containing no NAs for the three variables


# minimum grade allowance (what grade is too low?).
print(clean_data$fte_grade)
A_1 = clean_data[(clean_data$fte_grade == "A+"),]
nrow(A_1) #376 rows have A+
A_2 = clean_data[(clean_data$fte_grade == "A"),]
nrow(A_2) #267 rows have A
A_3 = clean_data[(clean_data$fte_grade == "A-"),]
nrow(A_3) #438 rows have A-
A_B = clean_data[(clean_data$fte_grade == "A/B"),]
nrow(A_B) #241 rows have A/B
B_1 = clean_data[(clean_data$fte_grade == "B+"),]
nrow(B_1) #253 rows have B+
B_2 = clean_data[(clean_data$fte_grade == "B"),]
nrow(B_2) #432 rows have B
B_3 = clean_data[(clean_data$fte_grade == "B-"),]
nrow(B_3) #205 rows have B-
B_C = clean_data[(clean_data$fte_grade == "B/C"),]
nrow(B_C) #1576 rows have B/C
C_1 = clean_data[(clean_data$fte_grade == "C+"),]
nrow(C_1) #82 rows have C+
C_2 = clean_data[(clean_data$fte_grade == "C"),]
nrow(C_2) #102 rows have C
C_3 = clean_data[(clean_data$fte_grade == "C-"),]
nrow(C_3) #251 rows have C-
C_D = clean_data[(clean_data$fte_grade == "C/D"),]
nrow(C_D) #142 rows have C/D
D_grade = clean_data[(clean_data$fte_grade == "D-"),]
nrow(D_grade) #1228 rows have D-

clean_data1 = clean_data[!(clean_data$fte_grade == "D-" | clean_data$fte_grade == "C/D"),]
summary(clean_data1) #clean_data1 is the modified dataset without the relatively unreliable pollsters



     

# start dates start too soon.
# end_dates end too soon.
print(clean_data1$start_date)
clean_data2 = clean_data1[(clean_data1$start_date > "2019-12-30"),]
summary(clean_data2) #clean_data2 is the modified dataset without the dates that start/end before 2020



# sample size is really variable, and some sameples are too small to decide. (how small is too small? Depends on state/numbner of polls/quality of polls/etc)
d = density(clean_data2$sample_size)
plot(d) 

#density curve is skewed to the left, so we must increase the minimum sample size number
clean_data3 = clean_data2[(clean_data2$sample_size > 1500),]
summary(clean_data3) #clean_data3 is the modified dataset whose minimum sample size number is more than 1500

d_2 = density(clean_data3$sample_size)
plot(d_2) #It's still skewed to the left, but we do not want to increase the minimum sample size number too much otherwise we would not have sufficient data to work with


