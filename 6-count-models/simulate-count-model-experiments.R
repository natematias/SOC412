## LOAD LIBRARIES
library(ggplot2)
library(texreg)
library(RcppZiggurat)
library(doParallel)
library(MASS)

## CLEAR PROJECT
rm(list=ls())

set.seed(454734)

## Number of cores to use in parallel processing
num.cores = 2

## GOALS FOR THE BLOG POST
# 1. What are the stakes?
#    A. You have count data on some kid of rare event, and 
#       you want good estimates of the average treatment effect
#       not just whether it increased or decreased
#    B. You want to avoid false positives
#    C. You want to avoid false negatives
#    D. You REALLY want to avoid Type S errors
#    E. You might need to do regression adjustment or adjust for interference
#    F. Power analysis (most important) 
#       you want to limit sample size
#       for reasons of ethics, cost-effectiveness, and learning 
# 2. What are the options (pros/cons)
#    A. log-transformed linear regression. Lots of companies use this
#       - PRO: simple, reliable, understandable
#       - CON: violates assumptions sometimes
#              can produce Type S errors
#              for rare occurrences, can require more observations than necessary
#    B. poisson regression. Google uses this (http://www.unofficialgoogledatascience.com/2015/08/an-introduction-to-poisson-bootstrap26.html)
#       - PRO: count model
#       - CON: assumptions rarely hold
#              can lead to false positives
#              might use too few observations
#    C. negative binomial regression.
#       - PRO: count model
#              fewer false positives
#       - CON: unknown if it's more efficient in terms of observations/assignments
#    D. mann-whitney U test
#       - PRO: nonparametric
#       - CON: doesn't give magnitude information
#              reportedly fails with rare incident data https://www.ncbi.nlm.nih.gov/pubmed/20826766
#    E. logistic regressions on buckets of activity with cutoffs. Example: did anything happen?
#       - PRO: nonparametric (UNSRE)
#       - CON: highly sensitive to cutoffs
#       - CON: can't really estimate magnitude


####################################
## UTILITY METHODS                ##
####################################

## LOAD COLORBLIND SAFE PALETTE
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

####################################
## LOAD DATAFRAMES                ##
####################################
## This dataset is a full set of posts that appeared in a subreddit in 2017
## Authors, post IDs, and dates have been anonymized or fuzzed
##
## COLUMNS:
## anon.author               - anonymized integer indicating the author
## id                        - anonymized post ID. Do not assume this is sorted by time
## author.prev.participation - previous comments and posts in the last six months by the author
## author.prev.posts         - previous posts by the author in the last six months
## front.page                - number of minutes that the post spent on the front page of reddit
## is.selftext               - a post with text rather than a link to a third party website
## newcomer.comments         - number of comments made by someone who hadn't previously commented in the last six months
## newcomer.comments.removed - number of comments by newcomers that were removed
## num.comments              - total number of comments in discussion
## num.comments.removed      - total number of comments removed in the discussion
## visible                   - final state of whether the post was allowed to remain visible by moderators or removed
## weekday                   - day of the week of the post
## weekend                   - whether the post was made on a weekend or not

posts <- subset(read.csv("~/Documents/github/CivilServant-Analysis/papers/r_science_2016/r_science_experiment_1_posts.09.26.2016.csv"), TREAT==0)

##############################################
## DEFINE SIMULATION METHODS                ##
##############################################

#' Return a random sample of observations from the dataset
#' with no upper bound
#' @param df The dataframe to sample from
#' @param n The number of observations to draw
randomSample = function(df,n) { 
  return (df[sample(nrow(df), n,replace=TRUE),])
}

#' Simulate experiment data from a negative binomial model
#' only works if the intercept from the base model is > 0
#' @param sample.size The sample size for the experiment.
#' @param model.nb The model to simulate from
#' @param pct.diff The simulated coefficient, expressed in percent change in the incidence rate, i.e. exp(coefficient)

simulate.data.from.negbin <- function(sample.size, model.nb, pct.diff){
  coef = log(pct.diff) ## for example for a 12% increase, log(1.12) = 0.1133287
  
  intercept <- model.nb$coefficients[1][['(Intercept)']]
  control.group <- data.frame(TREAT=0, dv = rnegbin(sample.size/2, mu = intercept, theta = model.nb$theta),
                              true.effect = pct.diff)
  treat.group <- data.frame(TREAT=1, dv = rnegbin(sample.size/2, mu = intercept + coef, theta = model.nb$theta),
                            true.effect = pct.diff)
  sim.obs <- rbind(control.group, treat.group)
}

#' Simulate experiment data from a poisson model
#' only works if the intercept from the base model is > 0
#' @param sample.size The sample size for the experiment.
#' @param model.p The model to simulate from
#' @param pct.diff The simulated coefficient, expressed in percent change in the incidence rate, i.e. exp(coefficient)

simulate.data.from.poisson <- function(sample.size, model.p, pct.diff){
  coef = log(pct.diff) ## for example for a 12% increase, log(1.12) = 0.1133287
  
  intercept <- model.p$coefficients[1][['(Intercept)']]
  control.group <- data.frame(TREAT=0, dv = rpois(sample.size/2, intercept),
                              true.effect = pct.diff)
  treat.group <- data.frame(TREAT=1, dv = rpois(sample.size/2,intercept + coef),
                            true.effect = pct.diff)
  sim.obs <- rbind(control.group, treat.group)
}


#' Simulate experiment data by drawing from an existing dataset
#' and make the average treatment effect normally distributed
#' @param sample.size The sample size for the experiment.
#' @param df The dataframe to draw from 
#' @param column.name The column name to draw from
#' @param model.nb The model to simulate from
#' @param pct.diff The simulated coefficient, expressed in percent change in the incidence rate, i.e. exp(coefficient)

simulate.study.observational <- function(sample.size, df, column.name, effect.size){
  sim.obs <-randomSample(df, sample.size)
  sim.obs$dv.orig <- sim.obs[[column.name]]
  sim.obs$true.effect <- effect.size
  num.obvs = nrow(sim.obs)
  sim.obs$post.num <- seq(1, num.obvs)
  sim.obs$TREAT <- as.numeric(zrnorm(num.obvs)>0)
  sim.obs$dv.effect[sim.obs$TREAT==0] <- 1
  sim.obs$dv.effect[sim.obs$TREAT==1] <- abs(rnorm(nrow(subset(sim.obs, TREAT==1)), 0,.2) +
                                                                 effect.size)
  
  sim.obs$dv <- round(sim.obs$dv.orig * sim.obs$dv.effect)
  sim.obs[,c("dv","TREAT", "true.effect")]
}


########################################################
## ILLUSTRATE HOW TO SIMULATE DATA WITH THESE METHODS ##
########################################################

base.model.nb <- glm.nb(newcomer.comments ~ 1, data=posts)

base.model.p <- glm(newcomer.comments ~ 1, data=posts, family="poisson")

sim.effect.size = 1.15 # 15% increase

sim.data.nb <- simulate.data.from.negbin(1000, base.model.nb, sim.effect.size)
sim.data.p <- simulate.data.from.poisson(1000, base.model.p, sim.effect.size)
sim.data.obs <- simulate.study.observational(1000, posts, "newcomer.comments", sim.effect.size)

### OBSERVE THE NUMBER OF ZEROES IN THE SIMULATED DATA
summary(sim.data.nb$dv > 0 )
summary(sim.data.p$dv > 0 )
summary(sim.data.obs$dv > 0 )

### ILLUSTRATE EXAMPLE SIMULATION
ggplot(sim.data.nb, aes(TREAT, dv, color=factor(TREAT))) +
#  geom_jitter(height=0) +
  geom_violin() +
  ggtitle(paste("Violin plot of experiment data",
                "Simulated with a negative binomial model", sep="\n"))

### ILLUSTRATE EXAMPLE SIMULATION
ggplot(sim.data.p, aes(TREAT, dv, color=factor(TREAT))) +
  geom_jitter(height=0) +
  geom_violin() +
  ggtitle(paste("Violin plot of experiment data",
                "Simulated with a poisson model", sep="\n"))

ggplot(sim.data.obs, aes(TREAT, dv, color=factor(TREAT))) +
  geom_jitter(height=0) +
  geom_violin() +
  ggtitle(paste("Violin plot of experiment data",
              "Simulated by drawing from observed data", sep="\n"))

###################################################
## METHODS FOR ESTIMATING THE ATE ON COUNT DATA  ##
###################################################

#' Estimate the average treatment effect with a log-transformed linear model
#' @param i  Counter used for aggregation
#' @param df The dataframe to use
estimate.study.loglinear <- function(i,df){
  m1 <- lm(log1p(dv) ~ TREAT, data=df)
  power.sim.m1.intercept <- m1$coefficients[['(Intercept)']]
  power.sim.m1.treat.effect <- m1$coefficients[['TREAT']]
  power.sim.m1.treat.coef <- summary(m1)$coefficients['TREAT',]
  power.sim.m1.stderr <- power.sim.m1.treat.coef[['Std. Error']]
  power.sim.m1.pvalue <- power.sim.m1.treat.coef[['Pr(>|t|)']]
  power.sim.m1.significant <- as.double(power.sim.m1.pvalue) < 0.05
  data.frame(i=i,
             model = "OLS",
             sample.size = nrow(df),
             power.sim.intercept      = power.sim.m1.intercept, 
             power.sim.treat.effect   = power.sim.m1.treat.effect,
             power.sim.stderr         = power.sim.m1.stderr,
             power.sim.pvalue         = power.sim.m1.pvalue,
             power.sim.significant    = power.sim.m1.significant,
             true.effect                       = head(df,1)$true.effect)
             
}

#' Estimate the average treatment effect with a poisson model
#' @param i  Counter used for aggregation
#' @param df The dataframe to use
estimate.study.poisson <- function(i,df){
  m2 <- glm(dv ~ TREAT, data=df, family="poisson")
  power.sim.m2.intercept <- m2$coefficients[['(Intercept)']]
  power.sim.m2.treat.effect <- m2$coefficients[['TREAT']]
  power.sim.m2.treat.coef <- summary(m2)$coefficients['TREAT',]
  power.sim.m2.stderr <- power.sim.m2.treat.coef[['Std. Error']]
  power.sim.m2.pvalue <- power.sim.m2.treat.coef[['Pr(>|z|)']]
  power.sim.m2.significant <- as.double(power.sim.m2.pvalue) < 0.05
  data.frame(i=i,
             model = "poisson",
             sample.size = nrow(df),
             power.sim.intercept      = power.sim.m2.intercept, 
             power.sim.treat.effect   = power.sim.m2.treat.effect,
             power.sim.stderr         = power.sim.m2.stderr,
             power.sim.pvalue         = power.sim.m2.pvalue,
             power.sim.significant    = power.sim.m2.significant,
             true.effect                       = head(df,1)$true.effect)
  
}

#' Estimate the average treatment effect with a negative binomial model
#' @param i  Counter used for aggregation
#' @param df The dataframe to use
estimate.study.negbin <- function(i,df){
  m.nb <- glm.nb(dv ~ TREAT, data=df)
  m.nb.intercept <- m.nb$coefficients[['(Intercept)']]
  m.nb.treat.effect <- m.nb$coefficients[['TREAT']]
  m.nb.treat.coef = coef(summary(m.nb))['TREAT',]
  m.nb.stderr <- m.nb.treat.coef[['Std. Error']]
  m.nb.pvalue <- m.nb.treat.coef[['Pr(>|z|)']]
  m.nb.significant <- as.double(m.nb.pvalue) < 0.05  
  
  data.frame(i=i,
             model="negbin",
             sample.size = nrow(df),
             power.sim.intercept      = m.nb.intercept, 
             power.sim.treat.effect   = m.nb.treat.effect,
             power.sim.stderr         = m.nb.stderr,
             power.sim.pvalue         = m.nb.pvalue,
             power.sim.significant    = m.nb.significant,
             true.effect                       = head(df,1)$true.effect)  
}


#' Estimate the average treatment effect with a logistic regression
#' @param i  Counter used for aggregation
#' @param df The dataframe to use
estimate.study.logistic <- function(i,df){
  df$binary.dv <- df$dv > 0
  m.b <- glm(binary.dv ~ TREAT, data=df, family=binomial)
  m.b.intercept <- m.b$coefficients[['(Intercept)']]
  m.b.treat.effect <- m.b$coefficients[['TREAT']]
  m.b.treat.coef = coef(summary(m.b))['TREAT',]
  m.b.stderr <- m.b.treat.coef[['Std. Error']]
  m.b.pvalue <- m.b.treat.coef[['Pr(>|z|)']]
  m.b.significant <- as.double(m.b.pvalue) < 0.05  
  
  data.frame(i=i,
             model="logistic",
             sample.size = nrow(df),
             power.sim.intercept      = m.b.intercept, 
             power.sim.treat.effect   = m.b.treat.effect,
             power.sim.stderr         = m.b.stderr,
             power.sim.pvalue         = m.b.pvalue,
             power.sim.significant    = m.b.significant,
             true.effect                       = head(df,1)$true.effect)  
}

#' Estimate the median treatment effect with a wilcoxon mann whitney test
#' @param i  Counter used for aggregation
#' @param df The dataframe to use
estimate.study.wilcoxon.mann.whitney <- function(i, df){
  wt <- wilcox.test(formula = dv ~ TREAT, data=df, conf.int=TRUE)
  wt.pvalue <- wt$p.value
  wt.confint.lwr <- wt$conf.int[1]
  wt.confint.upr <- wt$conf.int[2]
  wt.significant <- wt$p.value < 0.05
  wt.treat.effect <- wt$estimate
  data.frame(i = i,
             model="wmw",
             sample.size = nrow(df),
             power.sim.intercept      = NA, 
             power.sim.treat.effect   = wt.treat.effect,
             power.sim.stderr         = NA,
             power.sim.pvalue         = wt.pvalue,
             power.sim.significant    = wt.significant,
             true.effect              = head(df,1)$true.effect)   

}

###################################################
## COMPARING N SIMULATIONS WITH ALL MODEL TYPES  ##
###################################################

#' Working from a base negative binomial model, sample size, and effect size
#' carry out num.sims random samples and compare model results across all models
#' @param sample.size the size of the sample to use in simulated data
#' @param sim.effect.size the simulated effect size
#' @param model.nb the base negative binomial model to use
#' @param num.sims the number of simulations to run

model.comparison.from.nb <- function(sample.size, sim.effect.size, model.nb, num.sims){
  #pb <- txtProgressBar(min = 0 , max = num.sims, style=3)
  
  sim.data <- simulate.data.from.negbin(sample.size, model.nb, sim.effect.size)
  result.df <- estimate.study.loglinear(1, sim.data)
  result.df <- rbind(result.df, estimate.study.negbin(1, sim.data))
  result.df <- rbind(result.df, estimate.study.poisson(1, sim.data))
#  result.df <- rbind(result.df, estimate.study.logistic(1, sim.data))
#  result.df <- rbind(result.df, estimate.study.wilcoxon.mann.whitney(1, sim.data))
  
  for(i in seq(2,num.sims)){
    #setTxtProgressBar(pb, i)
    sim.data <- simulate.data.from.negbin(sample.size, model.nb, sim.effect.size)
    result.df <- rbind(result.df, estimate.study.loglinear(i, sim.data))
    result.df <- rbind(result.df, estimate.study.negbin(i, sim.data))
    result.df <- rbind(result.df, estimate.study.poisson(i, sim.data))
#    result.df <- rbind(result.df, estimate.study.logistic(i, sim.data))
#    result.df <- rbind(result.df, estimate.study.wilcoxon.mann.whitney(i, sim.data))
  }
  result.df
}

#' Working from a observed data, sample size, and effect size
#' carry out num.sims random samples and compare model results across all models
#' @param sample.size the size of the sample to use in simulated data
#' @param sim.effect.size the simulated effect size
#' @param df the dataframe to use
#' @param col the column to use
#' @param num.sims the number of simulations to run
model.comparison.from.observed <- function(sample.size, sim.effect.size, df, col, num.sims){
  #pb <- txtProgressBar(min = 0 , max = num.sims, style=3)
  
  sim.data <- simulate.study.observational(sample.size, df, col, sim.effect.size)
  result.df <- estimate.study.loglinear(1, sim.data)
  result.df <- rbind(result.df, estimate.study.negbin(1, sim.data))
  result.df <- rbind(result.df, estimate.study.poisson(1, sim.data))
#  result.df <- rbind(result.df, estimate.study.logistic(1, sim.data))
  result.df <- rbind(result.df, estimate.study.wilcoxon.mann.whitney(1, sim.data))
  
  for(i in seq(2,num.sims)){
#    setTxtProgressBar(pb, i)
    sim.data <- simulate.study.observational(sample.size,df, col, sim.effect.size)
    result.df <- rbind(result.df, estimate.study.loglinear(i, sim.data))
    result.df <- rbind(result.df, estimate.study.negbin(i, sim.data))
    result.df <- rbind(result.df, estimate.study.poisson(i, sim.data))
#    result.df <- rbind(result.df, estimate.study.logistic(i, sim.data))
    result.df <- rbind(result.df, estimate.study.wilcoxon.mann.whitney(i, sim.data))
  }
  result.df
}

## test and illustrate the model comparison function
sim.effect = 1.2
mcnb <- model.comparison.from.observed(10000, sim.effect, posts, "newcomer.comments", 100)

## plot the performance of different models
ggplot(mcnb, aes(i, power.sim.treat.effect, color = factor(power.sim.significant))) +
  facet_grid(model ~ . ) +
  geom_point() +
  scale_color_manual(values=cbbPalette)

##############################
## CALCULATING POWER CURVES ##
##############################
power.analysis.from.observed <- function(sample.size.min, sample.size.increment, 
                                         sample.size.max, sim.effect.size, df, col, num.sims){
  num.iterations = ceiling((sample.size.max - sample.size.min) / sample.size.increment)
  pb <- txtProgressBar(min = 0 , max = num.iterations, style=3)
  
  #registerDoParallel(cores=num.cores)
  mcob <- model.comparison.from.observed(sample.size.min, sim.effect.size, df, col, num.sims)

  result.df = data.frame(model=NA, sample.size = NA, true.effect.size = NA, num.sims = NA,
                         pct.significant = NA, mean.effect = NA, mean.intercept = NA,
                         mean.pvalue = NA, type.s.rate = NA)
  pb <- txtProgressBar(min = 0 , max = num.iterations, style=3)
  
  foreach(m=c("OLS", "negbin", "poisson", "wmw")) %do% {
    subset.df <- subset(mcob, model==m)
    row.df <- data.frame(model=m, sample.size=sample.size.min, 
                         true.effect.size = sim.effect.size, num.sims = num.sims)
    row.df$pct.significant = nrow(subset(subset.df, power.sim.significant==TRUE)) / num.sims
    row.df$mean.effect = mean(subset.df$power.sim.treat.effect)
    row.df$mean.intercept = mean(subset.df$power.sim.intercept)
    row.df$mean.pvalue = mean(subset.df$power.sim.pvalue)
    row.df$type.s.rate = nrow(subset(subset.df, (power.sim.significant==TRUE & 
                                                 sign(power.sim.treat.effect) != sign(sim.effect.size - 1.0)))) / num.sims
    result.df <- rbind(result.df, row.df)
  }

  
  foreach(i=1:num.iterations) %do% {
    sample.size.sim = sample.size.min + sample.size.increment * i
    mcob <- model.comparison.from.observed(sample.size.sim, sim.effect.size, df, col, num.sims)
    
    foreach(m=c("OLS", "negbin", "poisson", "wmw")) %do% {
      subset.df <- subset(mcob, model==m)
      row.df <- data.frame(model=m, sample.size=sample.size.sim, 
                           true.effect.size = sim.effect.size, num.sims = num.sims)
      row.df$pct.significant = nrow(subset(subset.df, power.sim.significant==TRUE)) / num.sims
      row.df$mean.effect = mean(subset.df$power.sim.treat.effect)
      row.df$mean.intercept = mean(subset.df$power.sim.intercept)
      row.df$mean.pvalue = mean(subset.df$power.sim.pvalue)
      row.df$type.s.rate = nrow(subset(subset.df, (power.sim.significant==TRUE & 
                                                     sign(power.sim.treat.effect) != sign(sim.effect.size - 1.0 )))) / num.sims
      result.df <- rbind(result.df, row.df)
      setTxtProgressBar(pb, i)
    }
  }
  result.df
}



#' Working from a observed data, sample size, and effect size
#' carry out num.sims random samples and compare model results across all models
#' @param sample.size the size of the sample to use in simulated data
#' @param sim.effect.size the simulated effect size
#' @param model.nb negative binomial model
#' @param num.sims the number of simulations to run

power.analysis.from.negbin <- function(sample.size.min, sample.size.increment, 
                                         sample.size.max, sim.effect.size, model.nb, num.sims){
  num.iterations = ceiling((sample.size.max - sample.size.min) / sample.size.increment)
  #registerDoParallel(cores=num.cores)
  
  pb <- txtProgressBar(min = 0 , max = num.iterations, style=3)
  
  mcob <- model.comparison.from.nb(sample.size.min, sim.effect.size, model.nb, num.sims)
  
  result.df = data.frame(model=NA, sample.size = NA, true.effect.size = NA, num.sims = NA,
                         pct.significant = NA, mean.effect = NA, mean.intercept = NA,
                         mean.pvalue = NA, type.s.rate = NA)
  
  foreach(m=c("OLS", "negbin", "poisson", "wmw")) %do% {
    subset.df <- subset(mcob, model==m)
    row.df <- data.frame(model=m, sample.size=sample.size.min, 
                         true.effect.size = sim.effect.size, num.sims = num.sims)
    row.df$pct.significant = nrow(subset(subset.df, power.sim.significant==TRUE)) / num.sims
    row.df$mean.effect = mean(subset.df$power.sim.treat.effect)
    row.df$mean.intercept = mean(subset.df$power.sim.intercept)
    row.df$mean.pvalue = mean(subset.df$power.sim.pvalue)
    row.df$type.s.rate = nrow(subset(subset.df, (power.sim.significant==TRUE & 
                                                   sign(power.sim.treat.effect) != sign(sim.effect.size - 1.0)))) / num.sims
    result.df <- rbind(result.df, row.df)
  }
  setTxtProgressBar(pb, 1)
  
  
  foreach(i=1:num.iterations) %do% {
    sample.size.sim = sample.size.min + sample.size.increment * i
    mcob <- model.comparison.from.nb(sample.size.sim, sim.effect.size, model.nb, num.sims)
    
    foreach(m=c("OLS", "negbin", "poisson", "wmw")) %do% {
      subset.df <- subset(mcob, model==m)
      row.df <- data.frame(model=m, sample.size=sample.size.sim, 
                           true.effect.size = sim.effect.size, num.sims = num.sims)
      row.df$pct.significant = nrow(subset(subset.df, power.sim.significant==TRUE)) / num.sims
      row.df$mean.effect = mean(subset.df$power.sim.treat.effect)
      row.df$mean.intercept = mean(subset.df$power.sim.intercept)
      row.df$mean.pvalue = mean(subset.df$power.sim.pvalue)
      row.df$type.s.rate = nrow(subset(subset.df, (power.sim.significant==TRUE & 
                                                     sign(power.sim.treat.effect) != sign(sim.effect.size - 1.0)))) / num.sims
      result.df <- rbind(result.df, row.df)
      setTxtProgressBar(pb, i)
    }
  }
  result.df
}

#pafo <- power.analysis.from.observed(10000,10000,500000, sim.effect, posts, "newcomer.comments", 100)
#pafo <- subset(pafo, is.na(model)==FALSE)

#pafo$sample.size
#ggplot(pafo, aes(sample.size, mean.effect, color=pct.significant)) + facet_grid(model ~ . ) + geom_point()

#############################################################################
## ESTIMATE POWER ANALYSIS FROM NEGBIN (multiplier = 1.2) (multiplier = 1)

#pafnb <- power.analysis.from.negbin(10000,2000,100000, sim.effect, base.model.nb, 50)
#pafnb <- subset(pafnb, is.na(model)==FALSE)

ggplot(pafnb, aes(sample.size, pct.significant)) + facet_grid(model ~ . ) +
  geom_hline(yintercept = 0.8, linetype=2) +
  geom_point() +
  geom_smooth() +
  ggtitle("% chance of observing a statistically-significant effect with a given sample size (20% increase)")

ggplot(pafnb, aes(sample.size, type.s.rate)) + facet_grid(model ~ . ) + 
  geom_point() +
  geom_smooth() +
  ylim(0,0.2) +
  ggtitle("% chance of observing a type S error with a given sample size (20% increase)")

summary(lm(pct.significant ~ model, data = subset(pafnb, model=="OLS" | model=="negbin")))
####

#pafnb.no.effect <- power.analysis.from.negbin(10000,2000,100000, 1.0, base.model.nb, 50)
#pafnb.no.effect <- subset(pafnb.no.effect, is.na(model)==FALSE)

ggplot(pafnb.no.effect, aes(sample.size, pct.significant)) + facet_grid(model ~ . ) +
  geom_hline(yintercept = 0.8, linetype=2) +
  geom_point() +
  geom_smooth() +
  ggtitle("% chance of a false positive with a given sample size (no effect)")

ggplot(pafnb.no.effect, aes(sample.size, type.s.rate)) + facet_grid(model ~ . ) +
  geom_hline(yintercept = 0.8, linetype=2) +
  geom_point() +
  geom_smooth() +
  ggtitle("% chance of a type S error with a given sample size (no effect)")


summary(lm(pct.significant ~ model, data = subset(pafnb.no.effect, model=="OLS" | model=="negbin" | model =="poisson")))


#############################################################################
## ESTIMATE POWER ANALYSIS FROM OBSERVED (multiplier = 1.2) (multiplier = 1)
#############################################################################

pafob <- power.analysis.from.observed(10000,2000,100000, sim.effect, posts, "newcomer.comments", 50)
pafob <- subset(pafob, is.na(model)==FALSE)

ggplot(pafob, aes(sample.size, pct.significant)) + facet_grid(model ~ . ) +
  geom_hline(yintercept = 0.8, linetype=2) +
  geom_point() +
  geom_smooth() +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ggtitle("% chance of observing a statistically-significant effect with a given sample size (20% increase)")

ggplot(pafob, aes(sample.size, type.s.rate)) + facet_grid(model ~ . ) + 
  geom_point() +
  geom_smooth() +
  ylim(0,0.2) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ggtitle("% chance of observing a type S error with a given sample size (20% increase)")

summary(lm(pct.significant ~ model, data = subset(pafob, model=="OLS" | model=="negbin")))
####

pafob.no.effect <- power.analysis.from.negbin(10000,2000,100000, 1.0, base.model.nb, 50)
pafob.no.effect <- subset(pafob.no.effect, is.na(model)==FALSE)

ggplot(pafob.no.effect, aes(sample.size, pct.significant)) + facet_grid(model ~ . ) +
  geom_hline(yintercept = 0.8, linetype=2) +
  geom_point() +
  geom_smooth() +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ggtitle("% chance of a false positive with a given sample size (no effect)")

ggplot(pafob.no.effect, aes(sample.size, type.s.rate)) + facet_grid(model ~ . ) +
  geom_hline(yintercept = 0.8, linetype=2) +
  geom_point() +
  geom_smooth() +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ggtitle("% chance of a type S error with a given sample size (no effect)")


summary(lm(pct.significant ~ model, data = subset(pafnb.no.effect, model=="OLS" | model=="negbin" | model =="poisson")))


save.image("simulate-count-model-experiment-outcomes.RData")