# Assignment: ASSIGNMENT 3
# Name: Shrestha, Saurabh
# Date: 2021-04-04

#load the ggplot library
library(ggplot2)

#i.What are the elements in your data (including the categories and data types)?
#The elements are Id,	Id2, Geography,	PopGroupID,	POPGROUP.display-label,	RacesReported,HSDegree,	BachDegree.
#Data types are integers, numeric and character.

#ii Please provide the output from the following functions: str(); nrow(); ncol()
week3 <- read.csv("C:/Users/Saurabh/Desktop/acs-14-1yr-s0201.csv")
str(week3)
nrow(week3)
ncol(week3)

#iii Create a Histogram of the HSDegree variable using the ggplot2 package.
# Set a bin size for the Histogram.
# Include a Title and appropriate X/Y axis labels on your Histogram Plot.
ggplot(week3, aes(x= week3$HSDegree)) + geom_histogram()
ggplot(week3, aes(x = week3$HSDegree)) +  geom_histogram(binwidth = 6)
ggplot(week3, aes (x = week3$HSDegree)) +geom_histogram(binwidth = 6, color = "blue", fill = "lightblue") + labs(title = "High School Degree Histogram", x = "HS Degree Average Population", y = "Frequency")

# Answer the following questions based on the Histogram produced:
# Based on what you see in this histogram, is the data distribution unimodal?
      "Yes,it can be seen that the data distribution is unimodal(has more than 1 mode), with the mode lying
between 87 -90 HS Degree."

#  Is it approximately symmetrical?
"     No, from the histogram, it does not seem to be symmetrical in nature(is not similarly divided).
"
#  Is it approximately bell-shaped?
"      No, the histogram is not bell-shaped.
"      
#  Is it approximately normal?
"        No, the histogram is not normal.
"
#  If not normal, is the distribution skewed? If so, in which direction?
        "Yes, the distribution is skewed. It is negatively skewed with more values titled towards the right side(tail)"

#  Include a normal curve to the Histogram that you plotted.
new_hist <- hist(week3$HSDegree)
new_multiplier <- new_hist$counts / new_hist$density
new_density <- density(week3$HSDegree)
new_density$y <- new_density$y * new_multiplier[1]

plot(new_hist)
lines(new_density)

new_x <- seq(min(week3$HSDegree), max(week3$HSDegree), length.out = 80)
new_mean <- mean(week3$HSDegree)
new_sd <- sd(week3$HSDegree)

normal <- dnorm (x = new_x, mean = new_mean, sd = new_sd)
lines(new_x, normal + new_multiplier[1], col = "red", lwd = 2)

sd_x <- seq(new_mean -2 * new_mean + 3 * new_sd, by = new_sd)
sd_y <- dnorm(x = sd_x, mean = new_mean, sd = new_sd) * new_multiplier[1]

segments(x0 =sd_x, y0 = 0, x1 = sd_x, y1 =sd_y, col = "red", lwd = 2)


# Explain whether a normal distribution can accurately be used as a model for this data.
"   No. Normal distribution cannot be used as a model this data because the data
seems to be a negatively skewed one. Also, as evident from the above graph, the
histogram is not of the same pattern as the normal curve."

#Create a Probability Plot of the HSDegree variable.
qqnorm(week3$HSDegree)

#Answer the following questions based on the Probability Plot:
"  Based on what you see in this probability plot, is the distribution approximately normal? Explain how you know.
"
#  If not normal, is the distribution skewed? If so, in which direction? Explain how you know.
      "No. Normal distribution cannot be used as a model for this data because the data
seems to be a negatively skewed one. Also, as evident from the above graph, the
histogram is not of the same pattern as the normal curve."

qqnorm(week3$HSDegree)
qqline(week3$HSDegree)

# Now that you have looked at this data visually for normality, you will now quantify normality with numbers using the stat.desc() function. Include a screen capture of the results produced.
library(pastecs)
stat.desc(week3$HSDegree, basic = FALSE, desc = TRUE, norm = TRUE, p=0.95)

#In several sentences provide an explanation of the result produced for skew, kurtosis, and z-scores. In addition, explain how a change in the sample size may change your explanation?
    "The value of skewness is -1.674767e+00 (<0) which implies that distribution is
negatively skewed.
The value of kurtosis is 4.352856e+00 (>3) which implies the distribution
is leptokurtic.
Also, Z-Score = 8.7736 and p-value is almost zero. Hence null hypothesis is rejected of data being normal distributed.s of data being normally distributed."

