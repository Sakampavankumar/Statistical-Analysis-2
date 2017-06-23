
#Mohammad Yasar Arshad

#*****************QUESTION 1***************

task1 <- runif(1000, min=5, max=9) #Task1 - Random Uniform Distribution
task2 <- rexp(1000, rate=0.1) #Task2 - Random Exponential Distribution
task3 <- rpois(1000,lambda = 4) #Task 3 - Random Posisson Distribution
task4  <- runif(1000,min=3, max=10) #Task 4 - Random Uniform Distribution

#1a What are the mean and median times to complete all the tasks

# As the task are parallel calculate the max of Task1,2,3
tot <- ifelse((task1>task2 & task1 > task3),task1, ifelse((task2>task3& task2>task1), task2,task3))

# Task 4 is in sequence 
sum <- tot + task4

c("Mean"=mean(sum), "Median"=median(sum))

#1b What is the probability that all the tasks are completed in 15 hours?

tot <- ifelse((task1>task2 & task1 > task3),task1, ifelse((task2>task3& task2>task1), task2,task3))
sum <- tot + task4
probab <- mean(sum<=15) #calcutae task that completed before 15 hours
c("Probability to complete the task in 15 hours=",probab)

#1c Create a plot of the density of the total completion time

plot(density(sum), main= "Total Completion Time Distribution")
polygon(density(sum), col = "green")



#*******************************************QUESTION 2*******************************

#Read the file diabetes.csv. There is a variable called Pregnancies, 
#which indicates the number of pregnancies. 
#Assuming this follows a poisson distribution, 
#test the hypothesis that the mean number of pregnancies is 3.7

diabetes <- read.csv("diabetes.csv")
head(diabetes)

# H0: Mean of Pregnancies = 3.7

tstat <- mean(diabetes$Pregnancies) # Test Statistic - Mean
# Considering random possion distribution and replicating 1000 times
# Population generation

sdist_mean<- replicate(1000,mean(rpois(768, lambda  = 3.7))) 
plot(density(sdist_mean))
polygon(density(sdist_mean),col="green")
abline(v=tstat, lwd=4)
abline(v=3.7+(3.7-tstat), lwd=4)


left_p = length(sdist_mean[sdist_mean<3.7+(3.7-tstat)]) 
right_p = length(sdist_mean[sdist_mean>tstat]) 
p = (left_p+right_p)/length(sdist_mean) 
p 

#We reject the null hypothesis


#**************QUESTION 3****************

#Read the file diabetes.csv. There is a variable called Insulin. 
#Conduct both a parametric and a non-parametric test for the median 
#value of 80. Are the results from both the tests similar? If not, 
#explain why and which test you would trust more


insul <- diabetes$Insulin

# Parametric Test
#H0 : Median = 80

tstat <- median(insul)
sdist<- replicate(1000,median(rnorm(length(insul), 
                            mean= 80, sd = sd(insul)))) #Population Generation

plot(density(sdist))


gap = abs(mean(sdist)-tstat) 
abline(v=mean(sdist)-gap) 
abline(v=mean(sdist)+gap) 
pvalue = length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist) 
pvalue




#Non-Parametric


tstat <- sum(ifelse(insul>80,1,0))

f2 <- function()
{
  v = c(1,0)
  p = c(0.5,0.5)
  x <- sample (x =v, replace =T, prob =p, size =length(insul) )
  return(sum(x))
}

sdist <- replicate(1000, f2())
plot(density(sdist)) 
gap = abs(mean(sdist)-tstat) 
abline(v=mean(sdist)-gap) 
abline(v=mean(sdist)+gap) 
pvalue = length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist) 
pvalue

#Answer:  In this case we have P value 0 from both the cases. In general we trust 
#Non-Parametric test because it not not based on any assumption
# about the distribution

#*************QUESTION 4*****************

#Read the file diabetes.csv. There are two variables called BMI and Outcome. 
#The variable Outcome takes on only two values: 0 and 1. 
#Conduct a non-parametric two sample test for the hypothesis 
#that the standard deviation of BMI is the same for both Outcome values

#H0 - Standard Deviation is same for both groups

Outcome1 <- diabetes[diabetes$Outcome==1, 6] # Having outcome 1
Outcome0 <- diabetes[diabetes$Outcome==0,6] # Having Outcome 0
tstat <- sd(Outcome0) - sd(Outcome1) # Defining test statistic
n=length(Outcome0)
f5 = function() # Generating population
{ 
  x = sample(c(Outcome0,Outcome1)) 
  m1 = sd(x[1:n]) 
  m2 = sd(x[(n+1):length(x)]) 
  abs(m1-m2) 
  return((m1-m2)) 
}
sdist <- replicate(10000, f5()) 
plot(density(sdist)) 
gap = abs(mean(sdist)-tstat) 
abline(v=tstat, lwd=4) 
abline(v=mean(sdist)-tstat, lwd=4)
lpvalue = mean(sdist>tstat)
rpvalue = mean(sdist<mean(sdist)-tstat)
pvalue = lpvalue+rpvalue
pvalue

#We fail to reject the hypothesis



#***************QUESTION 5*****************
#Read the file diabetes.csv. There are two variables called Glucose 
#and BloodPressure. Conduct a non-parametric test for the shapes of the 
#two distributions are identical


#H0 : There is no difference in the shape
attach(diabetes)
Glucose1 <- (Glucose-mean(Glucose))/sd(Glucose) # Standardizing Glucose
BloodPressure1 <- (BloodPressure-mean(BloodPressure))/sd(BloodPressure) # Standardizing Bloss Pressure
q = seq(0,1,by=0.1)
s1 = quantile(Glucose1, probs = q)
s2 = quantile(BloodPressure1, probs =q)
tstat <- sum(abs(s1-s2)) # Defining test statistic
shapetest = function() 
{ 
  sam1<- sample(Glucose1, 768, replace =T) # Taking out sample from Glucose
  sam2 <- sample(Glucose1, 768, replace =T) # Taking out sample from Blood Pressure
  quan1 <- quantile(sam1, probs = q) # Finding quantile values 
  quan2 <- quantile(sam2, probs = q)
  return(sum(abs(quan1-quan2)))
}
sdist = replicate(10000,shapetest()) # Population generation
plot(density(sdist)) 
abline(v=tstat, lwd=4) 
pvalue = mean(sdist>tstat) 
pvalue 

# Since P value is almost 0,we reject to the Hypothesis

