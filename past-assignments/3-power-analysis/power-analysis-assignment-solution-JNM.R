## LOAD LIBRARIES
library(ggplot2)
library(corrplot)
library(gmodels)
library(texreg)
library(rms)
library(blockrand)
library(lubridate)
library(psych)
library(RcppZiggurat)
library(MASS)

## CLEAR PROJECT
rm(list=ls())

###########################
## SUBREDDIT DATA ANALYSIS
##
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


####################################
## UTILITY METHODS                ##
####################################

## randomSample pulls a random number of rows
## from a dataframe up to the number of rows
## in that dataframe
randomSample = function(df,n) { 
  return (df[sample(nrow(df), n,replace=TRUE),])
  #return(sample(df, n, replace=TRUE))
}

## LOAD COLORBLIND SAFE PALETTE
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

####################################
## LOAD DATAFRAMES                ##
####################################
posts <- read.csv("subreddit_posts.csv")
posts$log.newcomers <- log1p(posts$newcomer.comments)
posts$is.selftext <- posts$is.selftext == "True"
posts$created.utc <- as.Date(posts$created.utc)
posts$weekend <- (posts$weekday == 5 | posts$weekday ==6)
posts$made.front.page <- posts$front_page >0

hist(log1p(posts$num.comments))

####################################
## SUMMARY DATA                   ##
####################################


### TIME ELAPSED
max(date(posts$created.utc)) - min(date(posts$created.utc))

## POSTS PER DAY
nrow(posts) / as.numeric(max(date(posts$created.utc)) - min(date(posts$created.utc)))

## POSTS REMOVED PER DAY
nrow(subset(posts, visible==0)) / as.numeric(max(date(posts$created.utc)) - min(date(posts$created.utc)))

####################################
## UNIVARIATE ANALYSIS            ##
####################################

## NUMBER OF COMMENTS
summary(posts$num.comments)
hist(posts$num.comments)
hist(log1p(posts$num.comments))

## NUMBER OF COMMENTS REMOVED
summary(posts$num.comments.removed)
hist(posts$num.comments.removed)
hist(log1p(posts$num.comments.removed))

## NUMBER OF NEWCOMER COMMENTS
summary(posts$newcomer.comments)
hist(posts$newcomer.comments)
hist(log1p(posts$newcomer.comments))

## NUMBER OF NEWCOMER COMMENTS REMOVED
summary(posts$newcomer.comments.removed)
hist(posts$newcomer.comments.removed)
hist(log1p(posts$newcomer.comments.removed))

####################################
## BIVARIATE ANALYSIS             ##
####################################
corrplot(cor(posts[c('author.prev.participation', 'author.prev.posts', 'front_page', 
                     'newcomer.comments', 'newcomer.comments.removed', 'num.comments',
                     'num.comments.removed', 'visible')]))

## NUMBER OF COMMENTS BY FRONT PAGE AND VISIBILITY
ggplot(posts, aes(made.front.page, log1p(num.comments), color=factor(visible))) +
  geom_violin()

## NUMBER OF COMMENTS REMOVED BY FRONT PAGE AND VISIBILITY
ggplot(posts, aes(made.front.page, log1p(num.comments.removed), color=factor(visible))) +
  geom_violin()

## NUMBER OF NEWCOMER COMMENTS BY FRONT PAGE AND VISIBILITY
ggplot(posts, aes(made.front.page, log1p(newcomer.comments), color=factor(visible))) +
  geom_violin()

## NUMBER OF NEWCOMER COMMENTS REMOVED BY FRONT PAGE AND VISIBILITY
ggplot(posts, aes(made.front.page, log1p(newcomer.comments.removed), color=factor(visible))) +
  geom_violin()


##################################################
## MODEL NEGATIVE BINOMIAL DISTRIBUTIONS OF DVs ##
## (models to be used for power analysis)       ##
##################################################

base.num.comments                <- glm.nb(num.comments ~ 1, data=posts)
base.num.comments.weekend        <- glm.nb(num.comments ~ weekend, data=posts)
base.num.comments.log.lm         <- lm(log1p(num.comments) ~ 1, data=posts)
base.num.comments.p              <- glm(num.comments ~ 1, data=posts, family="poisson")

base.num.comments.removed        <- glm.nb(num.comments.removed ~ 1, data=posts)
base.num.comments.removed.p      <- glm(num.comments.removed ~ 1, data=posts, family="poisson")

base.newcomer.comments           <- glm.nb(newcomer.comments ~ 1, data=posts)
base.newcomer.comments.p         <- glm(newcomer.comments ~ 1, data=posts, family="poisson")

base.newcomer.comments.removed   <- glm.nb(newcomer.comments.removed ~ 1, data=posts)
base.newcomer.comments.removed.p <- glm(newcomer.comments.removed ~ 1, data=posts, family="poisson")


fp.posts <- subset(posts, made.front.page==TRUE)
fp.num.comments                <- glm.nb(num.comments ~ 1, data = fp.posts)
fp.num.comments.p              <- glm(num.comments ~ 1, data=posts, family="poisson")

fp.num.comments.removed        <- glm.nb(num.comments.removed ~ 1, data = fp.posts)
fp.num.comments.removed.p      <- glm(num.comments ~ 1, data=posts, family="poisson")

fp.newcomer.comments           <- glm.nb(newcomer.comments ~ 1, data = fp.posts)
fp.newcomer.comments.p         <- glm(newcomer.comments ~ 1, data = fp.posts, family="poisson")

fp.newcomer.comments.removed   <- glm.nb(newcomer.comments.removed ~ 1, data = fp.posts)
fp.newcomer.comments.removed.p <- glm(newcomer.comments.removed ~ 1, data = fp.posts, family="poisson")

stargazer(list(base.num.comments, fp.num.comments), type="text", star.cutoffs=c(0.05, 0.01, 0.001))
stargazer(list(base.newcomer.comments, fp.newcomer.comments), type="text", star.cutoffs=c(0.05, 0.01, 0.001))
stargazer(list(base.newcomer.comments.removed, fp.newcomer.comments.removed), type="text", star.cutoffs=c(0.05, 0.01, 0.001))

#####################################################
## COMPARE SIMULATED NEGBIN DISTS TO OBSERVED DIST ##
#####################################################
## NOTE ON THETA PARAMETER IN R VERSUS STATA:
## https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/

num.obs.to.simulate <- nrow(fp.posts)

### OBSERVED DENSITY
ggplot(fp.posts, aes(num.comments)) +
  geom_density(color="black", fill="white", alpha=0.5) 

### MODELED DENSITY
ggplot(fp.posts, aes(num.comments)) +
  geom_density(fill="white", alpha=0.5, aes(x=rpois(num.obs.to.simulate, fp.num.comments.p$coefficients[1]['(Intercept)']))) +
  geom_density(fill="blue", color="blue", alpha=0.5, aes(x=rnegbin(num.obs.to.simulate, mu = fp.num.comments$coefficients[1][['(Intercept)']], theta = fp.num.comments$theta))) +
  geom_density(fill="red", color="red", alpha=0.5, aes(x=exp(rnorm(num.obs.to.simulate, abs(base.num.comments.log.lm$coefficients[1]['(Intercept)'])))))

### CURRENT STATUS

#######################################################
## EXAMPLE OF AN ITERATION IN A POWER ANALYSIS       ##
#######################################################

simulate.study.from.negbin <- function(i, sample.size, model.nb, pct.diff){
  cat(".")
  coef = log(pct.diff) ## for example for a 12% increase, log(1.12) = 0.1133287
  control.group <- data.frame(TREAT=0, dv = rnegbin(sample.size/2, mu = model.nb$coefficients[1][['(Intercept)']], theta = model.nb$theta))
  treat.group <- data.frame(TREAT=1, dv = rnegbin(sample.size/2, mu = model.nb$coefficients[1][['(Intercept)']] + coef, theta = model.nb$theta))
  sim.obs <- rbind(control.group, treat.group)
  
  m.nb <- glm.nb(dv ~ TREAT, data=sim.obs)
  m.nb.intercept <- m.nb$coefficients[['(Intercept)']]
  m.nb.treat.effect <- m.nb$coefficients[['TREAT']]
  m.nb.treat.coef = coef(summary(m.nb))[2,]
  m.nb.stderr <- m.nb.treat.coef['Std. Error']
  m.nb.pvalue <- m.nb.treat.coef['Pr(>|z|)']
  m.nb.significant <- as.double(m.nb.pvalue) < 0.05  
  
  
  #m1 <- lm(log1p(dv) ~ TREAT, data=sim.obs)
  # m1.intercept <- m1$coefficients[['(Intercept)']]
  # m1.treat.effect <- m1$coefficients[['TREAT']]
  # m1.treat.coef = coef(summary(m1))[2,]
  # m1.stderr <- m1.treat.coef['Std. Error']
  # m1.pvalue <- m1.treat.coef['Pr(>|t|)']
  # m1.significant <- as.double(m1.pvalue) < 0.05
  data.frame(i=i,
             power.sim.comments.intercept      = m.nb.intercept, 
             power.sim.comments.treat.effect   = m.nb.treat.effect,
             power.sim.comments.stderr         = m.nb.stderr,
             power.sim.comments.pvalue         = m.nb.pvalue,
             power.sim.comments.significant    = m.nb.significant)
}


simulate.study <- function(i, sample.size, effect.size){
  cat(".")
  #print(paste("sample", sample.size, ". Effect size", effect.size, "."))

  sim.posts <-randomSample(posts, sample.size)
  sim.posts$wday <- wday(sim.posts$created.utc)
  sim.posts$weekend <- (sim.posts$wday == 5 | sim.posts$wday==6)
  num.obvs = nrow(sim.posts)
  sim.posts$post.num <- seq(1, num.obvs)
  time.end <- Sys.time()

  sim.posts$condition <- as.numeric(zrnorm(num.obvs)>0)
  sim.posts$num.comments.effect[sim.posts$condition==0] <- 1
  
  sim.posts$num.comments.effect[sim.posts$condition==1] <- abs(rnorm(nrow(subset(sim.posts, condition==1)), 0,.2) +
                                                                      effect.size)
  sim.posts$num.comments.sim <- round(sim.posts$num.comments * sim.posts$num.comments.effect)
  #ggplot(sim.posts, aes(factor(condition), log1p(num.comments.sim))) + geom_violin()
  #ggplot(sim.posts, aes(factor(condition), log1p(num.comments))) + geom_violin()

  #ggplot(sim.posts, aes(log1p(num.comments), log1p(num.comments.sim), color=factor(condition))) + geom_point()
  
  #sim.posts$num.comments.effect <- comments.effect.size
  #sim.posts$num.comments.sim <- ifelse(sim.posts$condition !=0, sim.posts$num.comments * sim.posts$num.comments.effect, sim.posts$num.comments) 
  #ggplot(sim.posts, aes(condition, log1p(newcomer.comments.sim), color=factor(condition)))+ geom_violin()
  
  m1 <- lm(log1p(num.comments.sim) ~ condition, data=sim.posts)
  power.sim.m1.intercept <- m1$coefficients['(Intercept)']
  power.sim.m1.treat.effect <- m1$coefficients['condition']
  power.sim.m1.treat.coef = coef(summary(m1))[2,]
  power.sim.m1.stderr <- power.sim.m1.treat.coef['Std. Error']
  power.sim.m1.pvalue <- power.sim.m1.treat.coef['Pr(>|t|)']
  power.sim.m1.significant <- as.double(power.sim.m1.pvalue) < 0.05
  
  m.nb <- glm.nb(num.comments.sim ~ condition, data=sim.posts)
  m.nb.intercept <- m.nb$coefficients[['(Intercept)']]
  m.nb.treat.effect <- m.nb$coefficients[['condition']]
  m.nb.treat.coef = coef(summary(m.nb))[2,]
  m.nb.stderr <- m.nb.treat.coef['Std. Error']
  m.nb.pvalue <- m.nb.treat.coef['Pr(>|z|)']
  m.nb.significant <- as.double(m.nb.pvalue) < 0.05  
  

  #print(paste("num comments significant", power.sim.m1.significant,"."))
  
  data.frame(i=i,
             power.sim.comments.intercept      = power.sim.m1.intercept, 
             power.sim.comments.treat.effect   = power.sim.m1.treat.effect,
             power.sim.comments.stderr         = power.sim.m1.stderr,
             power.sim.comments.pvalue         = power.sim.m1.pvalue,
             power.sim.comments.significant    = power.sim.m1.significant,
             
             m.nb.intercept                    = m.nb.intercept,
             m.nb.treat.effect                 = m.nb.treat.effect,
             m.nb.stderr                       = m.nb.stderr,
             m.nb.pvalue                       = m.nb.pvalue,
             m.nb.significant                  = m.nb.significant,
             comments.true.effect = effect.size)
}



power.analysis <- function(i, sample.size,  comments.effect.size, num.models){
  pm <- simulate.study(1,sample.size,comments.effect.size)
  for(i in seq(2,num.models)){
    pm <- rbind(pm, simulate.study(i,sample.size,comments.effect.size))
  }

  pct.comments.significant <- sum(pm$power.sim.comments.significant)/nrow(pm)*100
  mean.comments.effect <- mean(pm$power.sim.comments.treat.effect)
  
  mean.comments.nb.effect <- mean(pm$m.nb.treat.effect)
  pct.comments.nb.significant <- sum(pm$m.nb.significant)/nrow(pm)*100
  
  data.frame(i=i, sample.size=sample.size, 
             pct.comments.significant = pct.comments.significant,
             mean.comments.effect = mean.comments.effect,
             pct.comments.nb.significant = pct.comments.nb.significant,
             mean.comments.nb.effect = mean.comments.nb.effect)
}


power.analysis.nb <- function(i, sample.size, model.nb, pct.diff, num.models){
  pm <- simulate.study.from.negbin(1,sample.size,model.nb, pct.diff)
  for(i in seq(2,num.models)){
    pm <- rbind(pm, simulate.study.from.negbin(i,sample.size,model.nb, pct.diff))
  }
  
  pct.significant <- sum(pm$power.sim.comments.significant)/nrow(pm)*100
  mean.effect <- mean(pm$power.sim.comments.treat.effect)
  data.frame(i=i, sample.size=sample.size, 
             pct.significant = pct.significant, 
             mean.effect = mean.effect)
}


## SIMULATE ONE STUDY USING THE MULTIPLICATION METHOD
sim.newcomer.effect <- 1.38
sim.comments.effect <- 1.1
sim.sample.size <- 10000

x <- power.analysis(1, sim.sample.size, sim.comments.effect, 10)

## SIMULATE ONE STUDY USING THE NEGATIVE BINOMIAL MODELING METHOD
x <- power.analysis.nb(1,1000, base.num.comments, 1.38, 100)

##################################################################
###### POWER ANALYSIS USING THE NEGATIVE BINOMIAL MODELING METHOD
##################################################################
maximum.sample.size <- 50000
minimum.sample.size <- 5000
iterate.by          <- 5000
sim.effect          <- 1.38
num.models          <- 100

models.decision.nb <- power.analysis.nb(1,minimum.sample.size, base.num.comments.weekend, sim.effect, num.models)
for(i in seq(1,maximum.sample.size/iterate.by)){
  print(paste("sample.size",minimum.sample.size + iterate.by*i))
  models.decision.nb <- rbind(models.decision.nb, power.analysis.nb(i,
       minimum.sample.size + iterate.by*i, base.num.comments.weekend,
       sim.effect, num.models))
}

ggplot(models.decision.nb, aes(sample.size, pct.significant)) +
  geom_smooth() +
  geom_point ()

##################################################################
###### POWER ANALYSIS USING THE MULTIPLICATION METHOD ############
##################################################################
models.decision.mult <- power.analysis(1, minimum.sample.size, sim.effect, num.models)
for(i in seq(1,maximum.sample.size/iterate.by)){
  print(paste("sample.size",minimum.sample.size + iterate.by*i))
  models.decision.mult <- rbind(models.decision.mult, power.analysis(i,
                                                                minimum.sample.size + iterate.by*i,
                                                                sim.effect, num.models))
                           
}

######################################
### LOOK FURTHER AT SAMPLES BELOW 5000
maximum.sample.size <- 5000
minimum.sample.size <- 500
iterate.by          <- 500
sim.effect          <- 1.38
num.models          <- 100

for(i in seq(1,maximum.sample.size/iterate.by)){
  print(paste("sample.size",minimum.sample.size + iterate.by*i))
  models.decision.nb <- rbind(models.decision.nb, power.analysis.nb(i,
                                                                    minimum.sample.size + iterate.by*i, base.num.comments.weekend,
                                                                    sim.effect, num.models))
}

for(i in seq(1,maximum.sample.size/iterate.by)){
  print(paste("sample.size",minimum.sample.size + iterate.by*i))
  models.decision.mult <- rbind(models.decision.mult, power.analysis(i,
                                                                     minimum.sample.size + iterate.by*i,
                                                                     sim.effect, num.models))
  
}

######################################
### PLOT ALL POWER ANALYSIS RESULTS

ggplot(models.decision.mult, aes(sample.size, pct.comments.significant)) +
  geom_point (aes(color="lm sampled")) +
  geom_smooth(aes(color="lm sampled")) +
    geom_point(aes(x=models.decision.nb$sample.size, y=models.decision.nb$pct.significant, color="nb sim")) +
  geom_smooth(aes(x=models.decision.nb$sample.size, y=models.decision.nb$pct.significant, color="nb sim")) +
  geom_point(aes(x=models.decision.mult$sample.size, y=models.decision.mult$pct.comments.nb.significant, color="nb sampled")) +
#  geom_smooth(aes(x=models.decision.mult$sample.size, y=models.decision.mult$pct.comments.nb.significant, color="nb sampled")) +
  ylim(0,106) +
  scale_y_continuous(breaks=c(seq(0,100, by=10))) +
  scale_color_manual(values=cbbPalette) +
  scale_x_continuous(breaks=c(seq(0,50000, by=10000))) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Statistical power for different sampling & modeling approaches\n(true effect: 38% increase in number of comments)")
  


