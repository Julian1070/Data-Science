# An analysis of the lalonde dataset

# Part 0: Importing and setting up Data - Lalonde

library(Matching)
data(lalonde)
library(dplyr)
library(rafalib)
library(randomForest)

lalonde$u78 <- ifelse(lalonde$re78 == 0, "1", "0")
treat <- filter(lalonde, treat == 1)
control <- filter(lalonde, treat == 0)
HS <- filter(lalonde, nodegr == 0)
HS_treat <- filter(HS, treat == 1)
HS_control <- filter(HS, treat == 0)
NoHS <- filter(lalonde, nodegr == 1)
NoHS_treat <- filter(NoHS, treat == 1)
NoHS_control <- filter(NoHS, treat == 0)

# PART 1:

model <- lm(lalonde$re78 ~ lalonde$age+lalonde$educ+lalonde$married+lalonde$re75+lalonde$u75+lalonde$treat,lalonde)
model_HS <- lm(HS$re78 ~ HS$age+HS$educ+HS$married+HS$re75+HS$u75+HS$treat,HS)
model_NoHS <- lm(NoHS$re78 ~ NoHS$age+NoHS$educ+NoHS$married+NoHS$re75+NoHS$u75+NoHS$treat,NoHS)
summary(model_HS)
summary(model_NoHS)
confint(model_HS, level = 0.95)
confint(model_NoHS, level = 0.95)

# PART 2: 

forest <- randomForest(lalonde$re78 ~ lalonde$age+lalonde$educ+lalonde$married+lalonde$re75+lalonde$u75+lalonde$treat,data=lalonde, 
                       importance=TRUE, ntrees=2000)
forest_HS <- randomForest(HS$re78 ~ HS$age+HS$educ+HS$married+HS$re75+HS$u75+HS$treat,data=HS, importance=TRUE, ntrees=2000)
forest_NoHS <- randomForest(NoHS$re78 ~ NoHS$age+NoHS$educ+NoHS$married+NoHS$re75+NoHS$u75+NoHS$treat,data=NoHS, importance=TRUE, 
                            ntrees=2000)
varImpPlot(forest_HS)
varImpPlot(forest_NoHS)

# PART 3:

mean_treat <- mean(treat$re78)
mean_control <- mean(control$re78)
mean_HS_treat <- mean(HS_treat$re78)
mean_HS_control <- mean(HS_control$re78)
mean_NoHS_treat <- mean(NoHS_treat$re78)
mean_NoHS_control <- mean(NoHS_control$re78)
diff_means <- mean_treat - mean_control
diff_means_HS <- mean_HS_treat - mean_HS_control
diff_means_NoHS <- mean_NoHS_treat - mean_NoHS_control

assign <- function(lalonde) {
  treatment_rows<-sample(nrow(lalonde), nrow(lalonde) /2)
  treatment_lalonde <- (lalonde[treatment_rows,])
  control_lalonde <- (lalonde[-treatment_rows,])
  return(mean(treatment_lalonde$re78) - mean(control_lalonde$re78))
}
iter.lalonde <- function(x){
  store <- NULL 
  for (i in 1:x)
  {store[i] <- assign(lalonde)} 
  return(store)
}
store_2 <- iter.lalonde(1000)
quan <- quantile(store_2, prob = c(.025, .975))
plot(density(store_2))
abline(v=quan[1],lwd=2,col = "black")
abline(v=quan[2],lwd=2,col = "black") 
abline(v=diff_means, lwd =2, col="red")

assign_NoHS <- function(NoHS) {
  treatment_rows_NoHS<-sample(nrow(NoHS), nrow(NoHS) /2)
  treatment_NoHS <- (NoHS[treatment_rows_NoHS,])
  control_NoHS <- (NoHS[-treatment_rows_NoHS,])
  return(mean(treatment_NoHS$re78) - mean(control_NoHS$re78))
}
iter.NoHS <- function(x){
  store_NoHS <- NULL 
  for (i in 1:x)
  {store_NoHS[i] <- assign(NoHS)} 
  return(store_NoHS)
}
store_2_NoHS <- iter.NoHS(1000)
quan_NoHS <- quantile(store_2_NoHS, prob = c(.025, .975))
plot(density(store_2_NoHS))
abline(v=quan[1],lwd=2,col = "black")
abline(v=quan[2],lwd=2,col = "black") 
abline(v=diff_means_NoHS, lwd =2, col="red")

assign_HS <- function(HS) {
  treatment_rows_HS<-sample(nrow(HS), nrow(HS) /2)
  treatment_HS <- (HS[treatment_rows_HS,])
  control_HS <- (HS[-treatment_rows_HS,])
  return(mean(treatment_HS$re78) - mean(control_HS$re78))
}
iter.HS <- function(x){
  store_HS <- NULL 
  for (i in 1:x)
  {store_HS[i] <- assign(HS)} 
  return(store_HS)
}
store_2_HS <- iter.HS(1000)
quan_HS <- quantile(store_2_HS, prob = c(.025, .975))
plot(density(store_2_HS))
abline(v=quan[1],lwd=2,col = "black")
abline(v=quan[2],lwd=2,col = "black") 
abline(v=diff_means_HS, lwd =2, col="red")

# Part 4 - EXTRA:

# for all
got_job <- nrow(filter(lalonde, u75 == 1, u78 == 0))
got_job_treat <- nrow(filter(lalonde, u75 == 1, u78 == 0, treat == 1))
got_job_control <- nrow(filter(lalonde, u75 == 1, u78 == 0, treat == 0))
lost_job <- nrow(filter(lalonde, u75 == 0, u78 == 1))
lost_job_treat <- nrow(filter(lalonde, u75 == 0, u78 == 1, treat == 1))
lost_job_control <- nrow(filter(lalonde, u75 == 0, u78 == 1, treat == 0))
kept_job <- nrow(filter(lalonde, u75 == 0, u78 == 0))
kept_job_treat <- nrow(filter(lalonde, u75 == 0, u78 == 0, treat == 1))
kept_job_control <- nrow(filter(lalonde, u75 == 0, u78 == 0, treat == 0))
never_job <- nrow(filter(lalonde, u75 == 1, u78 == 1))
never_job_treat <- nrow(filter(lalonde, u75 == 1, u78 == 1, treat == 1))
never_job_control <- nrow(filter(lalonde, u75 == 1, u78 == 1, treat == 0))
job_com <- c(got_job, lost_job, kept_job, never_job, got_job_treat, lost_job_treat, kept_job_treat, never_job_treat, 
             got_job_control, lost_job_control, kept_job_control, never_job_control)
job_div <- c(nrow(lalonde), nrow(lalonde), nrow(lalonde), nrow(lalonde), nrow(treat), nrow(treat), nrow(treat), nrow(treat), nrow(control), 
             nrow(control), nrow(control), nrow(control))
jobs <- matrix(job_com, nrow = 4, ncol = 3)
rownames(jobs) <- c("got job", "lost job", "kept job", "never job")
colnames(jobs) <- c("all", "treatment", "control")
jobs_percent <- matrix(100*job_com/job_div, nrow = 4, ncol = 3)
rownames(jobs_percent) <- c("got job", "lost job", "kept job", "never job")
colnames(jobs_percent) <- c("all (%)", "treatment (%)", "control (%)")
jobs
jobs_percent

# for HS
got_job_HS <- nrow(filter(HS, u75 == 1, u78 == 0))
got_job_treat_HS <- nrow(filter(HS, u75 == 1, u78 == 0, treat == 1))
got_job_control_HS <- nrow(filter(HS, u75 == 1, u78 == 0, treat == 0))
lost_job_HS <- nrow(filter(HS, u75 == 0, u78 == 1))
lost_job_treat_HS <- nrow(filter(HS, u75 == 0, u78 == 1, treat == 1))
lost_job_control_HS <- nrow(filter(HS, u75 == 0, u78 == 1, treat == 0))
kept_job_HS <- nrow(filter(HS, u75 == 0, u78 == 0))
kept_job_treat_HS <- nrow(filter(HS, u75 == 0, u78 == 0, treat == 1))
kept_job_control_HS <- nrow(filter(HS, u75 == 0, u78 == 0, treat == 0))
never_job_HS <- nrow(filter(HS, u75 == 1, u78 == 1))
never_job_treat_HS <- nrow(filter(HS, u75 == 1, u78 == 1, treat == 1))
never_job_control_HS <- nrow(filter(HS, u75 == 1, u78 == 1, treat == 0))
job_com_HS <- c(got_job_HS, lost_job_HS, kept_job_HS, never_job_HS, got_job_treat_HS, lost_job_treat_HS, kept_job_treat_HS, 
                never_job_treat_HS, got_job_control_HS, lost_job_control_HS, kept_job_control_HS, never_job_control_HS)
job_div_HS <- c(nrow(HS), nrow(HS), nrow(HS), nrow(HS), nrow(HS_treat), nrow(HS_treat), nrow(HS_treat), nrow(HS_treat), nrow(HS_control), 
                nrow(HS_control), nrow(HS_control), nrow(HS_control))
jobs_HS <- matrix(job_com_HS, nrow = 4, ncol = 3)
rownames(jobs_HS) <- c("got job", "lost job", "kept job", "never job")
colnames(jobs_HS) <- c("all", "treatment", "control")
jobs_percent_HS <- matrix(100*job_com_HS/job_div_HS, nrow = 4, ncol = 3)
rownames(jobs_percent_HS) <- c("got job", "lost job", "kept job", "never job")
colnames(jobs_percent_HS) <- c("all (%)", "treatment (%)", "control (%)")
jobs_HS
jobs_percent_HS

# for NoHS
got_job_NoHS <- nrow(filter(NoHS, u75 == 1, u78 == 0))
got_job_treat_NoHS <- nrow(filter(NoHS, u75 == 1, u78 == 0, treat == 1))
got_job_control_NoHS <- nrow(filter(NoHS, u75 == 1, u78 == 0, treat == 0))
lost_job_NoHS <- nrow(filter(NoHS, u75 == 0, u78 == 1))
lost_job_treat_NoHS <- nrow(filter(NoHS, u75 == 0, u78 == 1, treat == 1))
lost_job_control_NoHS <- nrow(filter(NoHS, u75 == 0, u78 == 1, treat == 0))
kept_job_NoHS <- nrow(filter(NoHS, u75 == 0, u78 == 0))
kept_job_treat_NoHS <- nrow(filter(NoHS, u75 == 0, u78 == 0, treat == 1))
kept_job_control_NoHS <- nrow(filter(NoHS, u75 == 0, u78 == 0, treat == 0))
never_job_NoHS <- nrow(filter(NoHS, u75 == 1, u78 == 1))
never_job_treat_NoHS <- nrow(filter(NoHS, u75 == 1, u78 == 1, treat == 1))
never_job_control_NoHS <- nrow(filter(NoHS, u75 == 1, u78 == 1, treat == 0))
job_com_NoHS <- c(got_job_NoHS, lost_job_NoHS, kept_job_NoHS, never_job_NoHS, got_job_treat_NoHS, lost_job_treat_NoHS, kept_job_treat_NoHS, 
                  never_job_treat_NoHS, got_job_control_NoHS, lost_job_control_NoHS, kept_job_control_NoHS, never_job_control_NoHS)
job_div_NoHS <- c(nrow(NoHS), nrow(NoHS), nrow(NoHS), nrow(NoHS), nrow(NoHS_treat), nrow(NoHS_treat), nrow(NoHS_treat), nrow(NoHS_treat), 
                  nrow(NoHS_control), nrow(NoHS_control), nrow(NoHS_control), nrow(NoHS_control))
jobs_NoHS <- matrix(job_com_NoHS, nrow = 4, ncol = 3)
rownames(jobs_NoHS) <- c("got job", "lost job", "kept job", "never job")
colnames(jobs_NoHS) <- c("all", "treatment", "control")
jobs_percent_NoHS <- matrix(100*job_com_NoHS/job_div_NoHS, nrow = 4, ncol = 3)
rownames(jobs_percent_NoHS) <- c("got job", "lost job", "kept job", "never job")
colnames(jobs_percent_NoHS) <- c("all (%)", "treatment (%)", "control (%)")
jobs_NoHS
jobs_percent_NoHS

