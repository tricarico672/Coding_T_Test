#loading the dataset
ds<-data.frame('gender' = c(2, 2, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 2, 1, 2, 2, 2, 1, 1, 2, 1, 1, 2, 2, 2, 1, 2, 2, 2, 2, 1, 1, 2, 2, 1, 2, 2, 2, 2, 1, 1, 2, 1, 2, 3, 1, 1, 1, 2),
               'age' = c(27, 30, 37, 39, 20, 30, 26, 22, 24, 19, 27, 33, 35, 36, 31, 21, 20, 23, 25, 20, 43, 23, 22, 20, 35, 27, 28, 22, 19, 18, 29, 20, 33, 36, 29, 28, 33, 29, 24, 18, 19, 21, 25, 32, 30, 22, 34, 31, 19, 19, 19, 27, 22, 22, 20, 19, 18, 25, 21, 25, 20, 20, 19, 19, 24, 20, 33),
               'introduction' = c("high_MA", "low_MA", "high_MA", "low_MA", "high_MA", "low_MA", "high_MA", "low_MA", "high_MA", "low_MA", "low_MA", "high_MA", "low_MA", "high_MA", "high_MA", "low_MA", "low_MA", "high_MA", "low_MA", "high_MA", "low_MA", "low_MA", "high_MA", "low_MA", "high_MA", "high_MA", "high_MA", "low_MA", "low_MA", "low_MA", "high_MA", "low_MA", "high_MA", "low_MA", "high_MA", "high_MA", "low_MA", "low_MA", "high_MA", "low_MA", "high_MA", "low_MA", "high_MA", "low_MA", "high_MA", "low_MA", "high_MA", "low_MA", "high_MA", "low_MA", "high_MA", "high_MA", "low_MA", "low_MA", "high_MA", "high_MA", "low_MA", "low_MA", "high_MA", "high_MA", "low_MA", "high_MA", "low_MA", "high_MA", "high_MA", "high_MA", "low_MA"),
               'MA_think' = c(4.1429, 2.5714, 4.4286, 2.5714, 4.7143, 4.7143, 3.8571, 2, 4, 2.8571, 2, 4.1429, 1.7143, 2.8571, 3.8571, 1.8571, 3.1429, 3.5714, 1, 3.1429, 3.2857, 2.8571, 3.8571, 3, 2.4286, 3.4286, 3.1429, 3.5714, 3.5714, 3.4286, 3.2857, 2.1429, 4.2857, 3.7143, 3.4286, 3.1429, 2.7143, 3, 3.4286, 4.1429, 3.5714, 3.2857, 4.1429, 3.1429, 3.8571, 3.2857, 3, 3.4286, 3.8571, 3.4286, 3.2857, 2.7143, 4, 3.2857, 3.5714, 3.8571, 3.7143, 3, 3.2857, 3.5714, 3.7143, 4, 3.8571, 2.8571, 3.1429, 2.7143, 4.7143))
introduction<-ds[,3] #stores the third column of the dataset into a variable called introduction
ds$introduction[introduction== "high_MA"]<-1 #assign 1 to every value satisfying the logical equality in the introduction column
ds$introduction[introduction== "low_MA"]<-2 #assign 2 to every value satisfying the logical equality in the introduction column
IV_high<-ds[introduction=="high_MA",4] #store all values in the fourth column satisfying the logical equality in a variable called IV_high
IV_low<-ds[introduction =="low_MA",4]  #store all values in the fourth column satisfying the logical equality in a variable called IV_low
nIV_high<-length(IV_high) #get the number of observations for condition 1
nIV_low<-length(IV_low) #get the number of observations for condition 2
percLow<-length(IV_low)/67 #express the group exposed to condition 1 as a percentage of the total number of participants
percHigh<-length(IV_high)/67 #express the group exposed to condition 2 as a percentage of the total number of participants
#descriptives
library(psych) #loads the psych package allowing us to use functions contained therein
describe(ds) #gets the most important statistics of the dataset (mean, variance, sd etc.)
#checking assumption: normality for DV under different IVs conditions
#run Shapiro-Wilk test to get a p-value to compare to level of significance alpha=.05. The test returns a statistically nonsignificant result therefore we establish normality for the measurements under considerations.
pNorm1<-shapiro.test(IV_high)$p #runs the Shapiro-Wilk test for normality for the IV_high group. Testing against H0: distribution is normal. Not being able to reject this null hypothesis is good since this tells us that distribution of data for dataset under consideration is not very far from a normal one.
pNorm2<-shapiro.test(IV_low)$p #runs the Shapiro-Wilk test for normality for the IV_low group. It isolates the p-value and stores it into a variable for later reference. (the piece of code above does the same)
#from the previous lines of code we know that it is a statistically non-significant result (p>alpha) with alpha=.05, therefore normality is established
#to double check the result of the Shapiro-Wilk test let's visualize the frequency distribution through a histogram. As it can be observed data looks normally distributed.
hist(IV_high, xlab="attribution of cognitive abilities", main = "cognitive abilities attribution")
#from the previous lines of code we know that it is a statistically non-significant result, therefore normality is established
#checking assumption: equal variances
library(car) #loads the car package to get the leveneTest function ready to run
leveneTest(ds$MA_think, as.factor(ds$introduction)) #conducts the Levene's test to check for equal variances.
#again, the Levene's test checks against the HO: variances are equal. Therefore, a statistically non significant result (p>alpha)is what we are after. This test gives back a p=.11 which is >alpha, exactly what we were hoping to see.
#t-test
My_t.test<- function(x, y, independent=TRUE) { #this function reproduces exactly what a t-test would do. The logical argument independent is important to make the if statement in the function work
  if (independent==TRUE) { #tells r that if we want to conduct an independent t-test this is what it should do
    uSqrt<-var(x)/length(x)+var(y)/length(y) #finds what goes under the square root for the standard error
    se<-sqrt(uSqrt) #computes the standard error
    t_V<-(mean(x)-mean(y))/se #computes the t-value using the formula
    numDF<-uSqrt^2 #computes numerator of degrees of freedom
    denDF<-(var(x)^2/(length(x)^2*(length(x)-1)))+(var(y)^2/((length(y)^2*(length(y)-1)))) #computes denominator of degrees of freedom
    df<-numDF/denDF #combines numerator and denominator of degrees of freedom to get the actual value of degrees of freedom
    p_I<-pt(t_V, df, lower.tail = FALSE) #computes the p-value knowing the t-value, the deegres of freedom and whether we want an upper or lower-tail test
    Fp_I<-2*p_I #computes the real p-value taking into account both tails of the distribution. That is why we multiply by 2
    t.statistics<-data.frame('t-value'=t_V, 'degrees of freedom'=df, 'p-value'=Fp_I) #add vector with dataframe with all labelled important data we want to be returned
    return(t.statistics) #returns the dataframe with all important statistics for independent t-test
  } else { #second part of the if-statement telling r what to do when we want to run a dependent t-test (same group, no need to check equal variance)
    d <- x-y #stores the difference between x and y into a variable d
    se_D<-sqrt((var(d))/(length(d))) #computes the standard error for dependent t-test
    t_D<-mean(d)/se_D #computes t-value for dependent t-test
    df_D<-length(x)-1 #computes degrees of freedom for dependent t-test
    p_D<-pt(t_D, df_D, lower.tail = FALSE) #computes p-value for dependent t-test knowing t-value, degrees of freedom and whether we want to run an upper or lower-tail test
    Fp_D<-2*p_D #multiplies p-value by 2 to account for both tails of the distribution
    t.stats_D<-data.frame("t-value"=t_D, "degrees of freedom"=df_D, "p-value"=Fp_D) #stores relevant statistics of the dependent t-test into a dataframe
    return(t.stats_D) } #prints and returns all important statistics of the dependent t-test
}
#effect size computations for report
tv<-My_t.test(IV_high,IV_low)$t
dfT <- My_t.test(IV_high, IV_low)$d
r <- sqrt((tv^2)/(tv^2+dfT))
#code to create table for report
library(table1) #loads table1 package
table1::label(ds$gender) <- "Gender" #labels the column containing genders "Gender"
table1::label(ds$age) <- "Age" #labels the column containing ages "Age"
table1::label(ds$introduction) <- #labels the column containing introductions "Introduction"
  table1::label(ds$MA_think) <- "Attribution of cognitive abilities" #labels the column containing measurements of the DV "Attribution of cognitive abilities"
table1::table1(~gender + age + MA_think | introduction, data = ds) #finally puts together the pieces and generates the table
#THE ASSIGNMENT ENDS HERE, HOWEVER, FEEL FREE TO CHECK WHERE I WAS HEADED WITH THE FOLLOWING NON-WORKING T-TEST THAT INCLUDED ALSO ASSUMPTION CHECKS (THERE IS PROBABLY A SILLY MISTAKE WITH THE IF STATEMENT BUT I HOPE AT LEAST THE REASONING BEHIND IT IS CORRECT). ANY SUGGESTION ON HOW I COULD IMPROVE WOULD BE HIGHLY APPRECIATED THANK YOU!
#the following is an example of a possible function that includes assumption checks in the t-test
#My_t.tassumptions<- function(x, y, independent=TRUE) {
#normcheck <- shapiro.test(x)$p #stores p-value of shapiro-test for first group in variable normcheck
#normcheck2 <- shapiro.test(y)$p #stores p-value of shapiro-test for second group in variable normcheck2
#varcheck <- leveneTest(x+y, readline(prompt="select variable containing column with different conditions"))$Pr #stores p-value of Levene's Test for variances, includes readline() function to allow user to select the variable containing all conditions and automatically isolates the p-value
#if (normcheck>.05 && normcheck2>.05 && varcheck>.05){ #if statement that should check for statistically non significant results
# if (independent==TRUE) {
# uSqrt<-var(x)/length(x)+var(y)/length(y)
# se<-sqrt(uSqrt)
#t_V<-(mean(x)-mean(y))/se
#numDF<-uSqrt^2
# denDF<-(var(x)^2/(length(x)^2*(length(x)-1)))+(var(y)^2/((length(y)^2*(length(y)-1))))
#df<-numDF/denDF
#p_I<-pt(t_V, df, lower.tail = FALSE)
#Fp_I<-2*p_I
#t.statistics<-data.frame('t-value'=t_V, 'degrees of freedom'=df, 'p-value'=Fp_I) #add vector with dataframe with all labelled important data we want to be returned
#return(t.statistics)
#} else {
#  d <- x-y
# se_D<-sqrt((var(d))/(length(d)))
#        t_D<-mean(d)/se_D
#       df_D<-length(x)-1
#     p_D<-pt(t_D, df_D, lower.tail = FALSE)
#    Fp_D<-2*p_D
#   t.stats_D<-data.frame("t-value"=t_D, "degrees of freedom"=df_D, "p-value"=Fp_D)
#   return(t.stats_D) }
#  else {
#    return("Normality or Variance assumptions have been violated")
#   }
# }
