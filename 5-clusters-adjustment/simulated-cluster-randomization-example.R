library(ggplot2)
library(texreg)
library(psych)
library(MASS)
library(lubridate)
library(RcppZiggurat)
library(lme4)
library(lmerTest)
library(rms)
library(randomizr)
#source("BM_StandardErrors.R")
# devtools::install_github("acoppock/commarobust")
#library(commarobust)

rm(list=ls())
## LOAD COLORBLIND SAFE PALETTE
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


####### METHOD FOR SIMULATING A GROUP OUTCOME #######

simulate.group <- function(i, sample.size, control.mean, treat.mean){
 df <- data.frame(within.treatment = complete_ra(sample.size), group=i)
 df$outcome <- ifelse(df$within.treatment == 1, rnorm(sample.size/2, control.mean),  rnorm(sample.size/2, treat.mean) )
 df
}

simulate.zero.effect.dataset <- function(df){
  odd.condition <- sample(c(0,1), 1) == 1
  df$treat <- ifelse(df$group %% 2, odd.condition, !odd.condition)
  m1 <- lm(outcome ~ treat, data=df)
  sim.m1.intercept <- m1$coefficients['(Intercept)']
  sim.m1.treat.effect <- m1$coefficients['treatTRUE']
  sim.m1.treat.coef = coef(summary(m1))[2,]
  sim.m1.stderr <- sim.m1.treat.coef['Std. Error']
  sim.m1.pvalue <- sim.m1.treat.coef['Pr(>|t|)']
  sim.m1.significant <- as.double(sim.m1.pvalue) < 0.05
  data.frame(i=i, sim.ctl.fit = sim.m1.intercept,sim.treat.effect=sim.m1.treat.effect,
             sim.stderr=sim.m1.stderr, sim.pvalue=sim.m1.pvalue,sim.significant=sim.m1.significant)
  
}


########################################
####### INITIAL CASE: TWO GROUPS #######
########################################
set.seed(36)
group.size = 2
group.count = 20
control.mean.a = 5
control.mean.b = 10
treat.mean.a <- 5
treat.mean.b <- 10


for(i in seq(1,5)){
  assignments <- complete_ra(group.count)
  if(assignments[1]==1){
    posts <- simulate.group(1, group.size*i, control.mean.a, treat.mean.a)
  }else{
    posts <- simulate.group(1, group.size*i, control.mean.b, treat.mean.b)
  }
  for(k in seq(2,group.count)){
    if(assignments[k]==1){
      posts <- rbind(posts, simulate.group(k, group.size*i, control.mean.a, treat.mean.a) )
    }else{
      posts <- rbind(posts, simulate.group(k, group.size*i, control.mean.b, treat.mean.b) )
    }
  }
  
  print(
    ggplot(posts, aes(factor(group), outcome, color=factor(group%%2))) +
      geom_boxplot() +
      ylim(0,13) +
      geom_jitter(width=0.1) +
      theme_bw(base_size = 15, base_family = "Helvetica") +
      scale_color_manual(values=cbbPalette, guide=FALSE) +
      ggtitle(paste("Comparing Outcomes Between Groups\n",group.count ,"groups,", group.size*i, "observations per group"))
    )
  print(summary(lm(outcome ~ group %% 2, data=posts)))
  models <- simulate.zero.effect.dataset(posts)
  for(j in seq(1,50)){
    models <- rbind(models,simulate.zero.effect.dataset(posts))
  }
  print(paste("Mean Treatment Effect:", mean(models$sim.treat.effect)))
  
  readline("[return] for next iteration")
}

summary(m1 <- lm(outcome ~ group %% 2 , data=posts))
     
#########################################################
### IF WE WERE MODELING VARIATION BETWEEN STUDENTS
## NOW DE-MEANING (EQUIVALENT TO FIXED EFFECTS MODEL)
#########################################################
# posts$demeaned.outcome=NULL
# for(g in unique(posts$group)){
#   print(g)
#   group.mean <- mean(subset(posts, group==g)$outcome)
#   posts$group.mean[posts$group==g] <- group.mean
# }
# posts$demeaned.outcome <- posts$outcome - posts$group.mean
# 
# ggplot(posts, aes(factor(group), demeaned.outcome)) +
#   geom_boxplot() +
#   geom_jitter(width=0.1) +
#   theme_bw(base_size = 15, base_family = "Helvetica") +
#   ggtitle(paste("Comparing De-Meaned Outcomes Between Groups\n2 groups,", group.size*i, "observations per group"))


#########################################################
### CLUSTERED STANDARD ERRORS
#########################################################

posts$group.treat <- factor(posts$group %% 2)

g1 <- lm(outcome ~ group.treat, data=posts, singular.ok=FALSE)
r1 = ols(outcome ~ group.treat, data=posts, x=TRUE)    

#screenreg(list(g1, r1))
#robcov(r1, cluster=factor(posts$group), method='huber')

screenreg(robcov(r1, cluster=factor(posts$group), method='huber'), type="text")

summary(lmer(outcome ~ group.treat + (1|group), data=posts))
