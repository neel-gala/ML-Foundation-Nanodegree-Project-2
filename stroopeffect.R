# reading data from the csv
data <- read.csv('D:/ML Foundation Nanodegree/ML Foundation Nanodegree Project-2/stroopdata.csv')

# getting difference
data$diff <- data$Incongruent - data$Congruent

# returns descriptive statistics data
descriptive_data <- adply(data, 2, function(x){
  # mean
  avg <- mean(x[,1])
  # median
  med <- median(x[,1])
  # standard deviation
  std <- sd(x[,1])
  # sample size
  n = length(x[,1])
  # degree of freedom
  df = n-1
  # sum of squared mean difference
  ss = sum((x[,1]-avg)^2)
  return(data.frame(mean = avg, median = med, std_deviation = std, ss = ss, n = n, df = df))
})

colnames(descriptive_data)[which(names(descriptive_data) == "X1")] = 'column'

diff_mean <- descriptive_data[descriptive_data$column == 'diff', 'mean']
diff_std_dev <- descriptive_data[descriptive_data$column == 'diff', 'std_deviation']
diff_n <- descriptive_data[descriptive_data$column == 'diff', 'n']
# getting standard error 
sem = diff_std_dev/sqrt(diff_n)

# getting t 
t = diff_mean/sem

# cohen's d 
d = diff_mean/diff_std_dev
  
# considering alpha = 0.01
a = 0.01

# getting t-critical
t_critical <- 2.807

# checking t wrt t_critical 
if(t>t_critical){
  print('Null hypothesis rejected')
} else{
  print('Null hypothesis accepted')
}

# plotting data 
ggplot(data, aes(c(1:nrow(data)), Congruent)) +
  geom_line(aes(y = Incongruent, colour = "red")) + 
  geom_line(aes(y = Congruent, colour = "blue")) +
  geom_line(aes(y = diff, colour = "black")) +
  labs(title = "Data",
       x = "n", y = "Time taken to read \n ink words in secs") +
  scale_color_manual(labels = c("diff", "Congruent", "Incongruent"), values = c("blue", "red", "black"))

