require(readr)  # for reading data into R
library(dplyr)  # for a set of really useful data selection tools
data <- read_csv('president_polls.csv')
attach(data)  # eliminates the need for data$....
data = select(data, state, pollster, sponsors, fte_grade, sample_size, methodology, start_date, end_date, answer, question_id, poll_id, pct)  # select the useful data
data$state = as.factor(data$state)  # make into factor
data$question_id = as.factor(data$question_id)
data$poll_id = as.factor(data$poll_id)
data$pollster = as.factor(data$pollster)  # make into factor
data$sponsors = as.factor(data$sponsors)  # make into factor
data$fte_grade = as.factor(data$fte_grade)  # make into factor
data$methodology = as.factor(data$methodology)  # make into factor
data$start_date = as.Date(data$start_date, format = "%m/%d/%y")  # format tells you how to read the dates. In our case is month/day/two-number-year or %m/%d/%y 
data$end_date = as.Date(data$end_date, format = "%m/%d/%y")  # format tells you how to read the dates. In our case is month/day/two-number-year or %m/%d/%y
data$answer = as.factor(data$answer)  # make into factor

summary(data)# lets summarize



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

clean_data1 = clean_data[!(clean_data$fte_grade %in% c("C/D", "D-")),]
clean_data1$fte_grade = as.factor(as.character(clean_data1$fte_grade))
summary(clean_data1)#clean_data1 is the modified dataset without the relatively unreliable pollsters




# start dates start too soon.
# end_dates end too soon.
print(clean_data1$start_date)
clean_data2 = clean_data1[(clean_data1$start_date > "2019-12-30"),]
summary(clean_data2) #clean_data2 is the modified dataset without the dates that start/end before 2020
date_cleaner = function(x){
  if((is.na(x['start_date']) == TRUE) || (is.na(x['end_date']) == TRUE)){
    return(NA)
  }
  time_diff = x["end_date"] - x["start_date"]
  return(time_diff)
}
answer = c()  # something to hold the answer
for(i in 1:nrow(clean_data2)){  # loop through every row
  answer = c(answer, date_cleaner(clean_data2[i,]))  # add the output of date_cleaner to answer for every row
}
den = density(as.numeric(answer))
plot(den)

#number of days should by more than two but less than ten
clean_data3 = clean_data2[!(date_cleaner(clean_data2) < 2 | date_cleaner(clean_data2) > 10),]
summary(clean_data3) #clean_data3 is the dataset in which the dates are neither too long not too short
#View(clean_data3)




#we only want to focus on Trump and Biden
clean_data4 = clean_data3[clean_data3$answer %in% c("Trump", "Biden"),]
clean_data4$answer = as.factor(as.character(clean_data4$answer))
summary(clean_data4) #clean_data4 focuses only on Trump and Biden
summary(clean_data4$answer) 
#View(clean_data4)

#the number of rows for Trump and Biden are uneven
clean_data5 = clean_data4[clean_data4$question_id %in% clean_data4$question_id[duplicated(clean_data4$question_id)],]
summary(clean_data5) #this dataset ensures that the number of rows for the two candidates are even 
#View(clean_data5)

# sample size is really variable, and some sameples are too small to decide. (how small is too small? Depends on state/numbner of polls/quality of polls/etc)
d = density(clean_data5$sample_size)
plot(d) 

#maximize sufficiency of data
clean_data6 = clean_data5[(clean_data5$sample_size > 150),]
summary(clean_data6) 
clean_data6$state = as.factor(as.character(clean_data6$state))
levels(clean_data6$state)


d_2 = density(clean_data6$sample_size)
plot(d_2) 
#View(clean_data6)

#Let's add some rows so that ALL states are accounted for
da_1 = clean_data[clean_data$state %in% c("Arkansas", "District of Columbia","Delaware", "Hawaii", "Idaho", "Illinois", "Nebraska", "Rhode Island", "South Dakota", "Vermont", "Wyoming"),]
da_2 = da_1[c(275, 276, 33, 34, 41, 42, 29, 30, 31, 32, 25, 26, 21, 22, 17, 18, 13, 14, 15, 16, 9, 10, 11, 12, 5, 6, 7, 8, 37, 38, 135, 136, 1, 2, 3, 4),]
clean_data7 = rbind(clean_data6, da_2)
summary(clean_data7) 
clean_data8 = clean_data7[clean_data7$answer %in% c("Trump", "Biden"),]
clean_data8$answer = as.factor(as.character(clean_data8$answer))
summary(clean_data8) #the dataset shows the rows added back for sufficiency sake

#increase the minimum sample size number to maximize sufficiency of data
clean_data9 = clean_data8[(clean_data8$sample_size >= 375),]
summary(clean_data9) #clean_data6 is the modified dataset whose minimum sample size number is more than 375
clean_data9$state = as.factor(as.character(clean_data9$state))
levels(clean_data9$state)


d_2 = density(clean_data9$sample_size)
plot(d_2) 
#View(clean_data9)


#Let's get to model building
states = unique(clean_data9$state)

levels(states)

state_dist = data.frame(matrix(ncol = 5, nrow = length(states)), row.names = states)
colnames(state_dist) = c('state', 'trump_mean', 'trump_var', 'biden_mean', 'biden_var')


for(s in states){
  state_dist[s, 'state'] = s
  #calculate stats for trump
  temp_mean = mean(clean_data9[(clean_data9$state == s & clean_data9$answer == 'Trump'),]$pct, na.rm = TRUE)
  temp_var = var(clean_data9[(clean_data9$state == s & clean_data9$answer == 'Trump'),]$pct, na.rm = TRUE)
  state_dist[s, "trump_mean"] = temp_mean
  state_dist[s, "trump_var"] = temp_var
  #calculate stats for biden
  temp_mean = mean(clean_data9[(clean_data9$state == s & clean_data9$answer == 'Biden'),]$pct, na.rm = TRUE)
  temp_var = var(clean_data9[(clean_data9$state == s & clean_data9$answer == 'Biden'),]$pct, na.rm = TRUE)
  state_dist[s, "biden_mean"] = temp_mean
  state_dist[s, "biden_var"] = temp_var
}

# first goal is to find the winning states!
ci_col = data.frame(matrix(ncol = 4, nrow = length(states)), row.names = states)
colnames(ci_col) = c('ci_trump_1', 'ci_trump_2', 'ci_biden_1', 'ci_biden_2')
state_dist1 = cbind(state_dist, ci_col)

#perhaps we can replace the NAs in the Nebraska CD-1 row with the means of each column
state_dist1[is.na(state_dist1$trump_var), "trump_var"] = mean(state_dist1$trump_var, na.rm = TRUE)
state_dist1[is.na(state_dist1$biden_var), "biden_var"] = mean(state_dist1$biden_var, na.rm = TRUE)
state_dist1[is.na(state_dist1$ci_trump_1), "ci_trump_1"] = mean(state_dist1$ci_trump_1, na.rm = TRUE)
state_dist1[is.na(state_dist1$ci_trump_2), "ci_trump_2"] = mean(state_dist1$ci_trump_2, na.rm = TRUE)
state_dist1[is.na(state_dist1$ci_biden_1), "ci_biden_1"] = mean(state_dist1$ci_biden_1, na.rm = TRUE)
state_dist1[is.na(state_dist1$ci_biden_2), "ci_biden_2"] = mean(state_dist1$ci_biden_2, na.rm = TRUE)

#we do not want any variances to be zero
state_dist1[(state_dist1$trump_var == 0), "trump_var"] = mean(state_dist1$trump_var[state_dist1$trump_var != 0], na.rm = TRUE)
state_dist1[(state_dist1$biden_var == 0), "biden_var"] = mean(state_dist1$biden_var[state_dist1$biden_var != 0], na.rm = TRUE)


#let's construct the confidence intervals  
for(v in 1:nrow(state_dist1)){
  ci_trump1 = state_dist1[v, 'trump_mean'] - 1.96*sqrt(state_dist1[v, 'trump_var'])
  ci_trump2 = state_dist1[v, 'trump_mean'] + 1.96*sqrt(state_dist1[v, 'trump_var'])
  ci_biden1 = state_dist1[v, 'biden_mean'] - 1.96*sqrt(state_dist1[v, 'biden_var'])
  ci_biden2 = state_dist1[v, 'biden_mean'] + 1.96*sqrt(state_dist1[v, 'biden_var'])
  state_dist1[v, 'ci_trump_1'] = ci_trump1
  state_dist1[v, 'ci_trump_2'] = ci_trump2
  state_dist1[v, 'ci_biden_1'] = ci_biden1
  state_dist1[v, 'ci_biden_2'] = ci_biden2
}




#now to gather the electoral votes for each state
elec_ = read_csv("electoral_col - Sheet1.csv")
elec_1 = select(elec_, electoral)
state_dist2 = cbind(state_dist1, elec_1)

#which confidence intervals overlap
bin_overlap = c()
for(i in 1:nrow(state_dist2)){
  temp_x = max(state_dist2[i, "ci_trump_1"], state_dist2[i, "ci_biden_1"]) 
  temp_y = min(state_dist2[i, "ci_trump_2"], state_dist2[i, "ci_biden_2"]) 
  if(temp_x < temp_y){  # the case in which there is an overlap 
    bin_overlap = c(bin_overlap, 1)  # 1 indicates yes
  }else{
    bin_overlap = c(bin_overlap, 0)  # 0 indicates no 
  }
}

state_dist2[,'bin_overlap'] = bin_overlap
state_dist_no_overlap = state_dist2[state_dist2$bin_overlap == 0,] #dataset for which the ci's don't overlap
#View(state_dist_no_overlap)


t = function(x){  #  function displays the sum of the electoral votes for each state that Trump is likely to win  
  sum_trump = sum(x[which(x[, 'trump_mean'] > x[, 'biden_mean']), 'electoral'])
  return(sum_trump)
}

b = function(x){  #  function displays the sum of the electoral votes for each state that Biden is likely to win  
  sum_biden = sum(x[which(x[, 'trump_mean'] < x[, 'biden_mean']), 'electoral'])
  return(sum_biden)
}

coin_toss_states = state_dist2[state_dist2$bin_overlap == 1,] #dataset for which the confidence intervals overlap
bin_win = data.frame(matrix(nrow = nrow(coin_toss_states)))
colnames(bin_win) = c("bin_winner")
coin_toss_states = cbind(coin_toss_states, bin_win)






election_result = data.frame(matrix(ncol = 3)) #table shows the predicted outcome of the election 
colnames(election_result) = c("Trump", "Biden", "winner")

#let's perform 1000 simulations for the "coin toss" states
for(j in 1:1000){
  for(i in 1:nrow(state_dist2)){
    #temp_trump = runif(1, state_dist2[i, 'ci_trump_1'], state_dist2[i, 'ci_trump_2'])
    #temp_biden = runif(1, state_dist2[i, 'ci_biden_1'], state_dist2[i, 'ci_biden_2'])
    prob_trump = state_dist2[i, 'trump_mean']/(state_dist2[i, 'trump_mean'] + state_dist2[i, 'biden_mean'])
    prob_biden = state_dist2[i, 'biden_mean']/(state_dist2[i, 'trump_mean'] + state_dist2[i, 'biden_mean'])
    temp_winner = sample(c(0, 1), 1, prob = c(prob_trump, prob_biden))
    #state_dist2[i, 'bin_winner'] = ifelse(temp_trump > temp_biden, 0, 1)
    state_dist2[i, 'bin_winner'] = temp_winner
  }
  state_dist_trump = state_dist2[(state_dist2$bin_winner == 0),] #dataset with the coin toss states that Trump is likely to win 
  state_dist_biden = state_dist2[(state_dist2$bin_winner == 1),] #dataset with the coin toss states that Biden is likely to win
  #add up the electoral values for each candidate
  trump_elec = sum(state_dist_trump$electoral) #+ t(state_dist_no_overlap) 
  biden_elec = sum(state_dist_biden$electoral) #+ b(state_dist_no_overlap)
  trump_elec1 = as.integer(trump_elec)
  biden_elec1 = as.integer(biden_elec)
  #election_result = election_result[which(((election_result[, 'Trump'] >= 270 & election_result[, 'Biden'] < 270) | (election_result[, 'Biden'] >= 270 & election_result[, 'Trump'] < 270)) & election_result[, 'Trump'] + election_result[, 'Biden'] <= 537),]       
  election_result[j, "Trump"] = trump_elec1
  election_result[j, "Biden"] = biden_elec1
  election_result[j, "winner"] = ifelse(biden_elec1 > trump_elec1, 'Biden', 'Trump')
  election_result1 = election_result[complete.cases(election_result),]
}
#View(election_result1)
table(election_result1['winner'])

#plot density of election results 
density_biden = density(election_result1$Biden)
density_trump = density(election_result1$Trump)
plot(density_biden, xlab = 'Electoral Votes', ylab = 'Frequency', main = '2020 Presidential Election Predictions', col = 'blue', xlim = c(min(min(election_result1$Trump), min(election_result1$Biden)), max(max(election_result1$Trump), max(election_result1$Biden))))
lines(density_trump, col = 'red')
legend('topright', legend = c('Trump', 'Biden'), col = c('red', 'blue'), lty = 1:1)

#BIDEN WINS!

