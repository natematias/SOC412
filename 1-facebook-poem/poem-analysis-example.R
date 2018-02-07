library(ggplot2)
library(texreg)
library(psych)
library(MASS)
library(gmodels)
library(corrplot)
library(dotwhisker)


rm(list=ls())
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

########################################################
#### LOAD DATA: POEMS  #################################
#### Each record is the (simulated) 24 hour performance
####   of a poem shared on Facebook
#### Experiment procedure: 
#### https://medium.com/@natematias/how-anyone-can-audit-facebooks-newsfeed-b879c3e29015
#### 
#### wday: weekday integer (1-7, is Sunday)
#### weekend: binary (Sat & Sun = TRUE)
#### interactions: number of likes, comments, and shares on Facebook
#### date: the date
#### day.num: an integer indicating the day after the experiment started


poems <- read.csv("poem-experiment-simulation.csv")
poems$condition <- relevel(poems$condition, ref="Text")


########################################################
#### UNIVARIATE EXPLORATION
summary(poems$interactions)
summary(poems$weekend)
summary(factor(poems$wday))

ggplot(poems, aes(interactions)) +
  geom_histogram(binwidth=5) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ylab("Number of Poems") +
  xlab("Facebook Interactions") +
  scale_color_manual(values=cbbPalette, name="Intervention") +
  ggtitle("Histogram of Facebook Interactions Per Poem")

ggplot(poems, aes(log1p(interactions))) +
  geom_histogram(binwidth=0.25) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ylab("ln Number of Poems") +
  xlab("Facebook Interactions") +
  scale_color_manual(values=cbbPalette, name="Intervention") +
  ggtitle("Histogram of log-transformed Facebook Interactions Per Poem")


########################################################
#### BIVARIATE EXPLORATION

## SUMMARY STATISTICS BY CONDITION
describeBy(poems$interactions, poems$condition)

## SUMMARY STATISTICS BY WEEKEND
describeBy(poems$interactions, poems$weekend)

## WEEKEND TO CONDITION
CrossTable(poems$weekend, poems$condition, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE)

## PLOT POINTS BY DAY
ggplot(poems, aes(day.num, interactions, color=condition)) +
  geom_point(size=3) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  xlab("Day Number") +
  ylab("Facebook Interactions") +
  scale_color_manual(values=cbbPalette, name="Intervention") +
  ggtitle("Facebook Interactions Per Poem")

## BOXPLOT
ggplot(poems, aes(condition, interactions, fill=condition)) +
  geom_boxplot() +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  xlab("Condition") +
  ylab("Facebook Interactions") +
  scale_fill_manual(values=cbbPalette, name="Intervention") +
  ggtitle("Facebook Interactions Per Poem")

## VIOLIN PLOT
ggplot(poems, aes(condition, interactions, fill=condition)) +
  geom_violin() +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  xlab("Condition") +
  ylab("Facebook Interactions") +
  scale_fill_manual(values=cbbPalette, name="Intervention") +
  ggtitle("Facebook Interactions Per Poem")

########################################################
#### MULTIVARIATE EXPLORATION
poems$i.condition <- as.integer(poems$condition)

## CORRELATION PLOT
corrplot(cor(poems[c("day.num", "weekend", "interactions", 'i.condition')]), method="circle")


## BOXPLOT
ggplot(poems, aes(weekend, interactions, fill=condition)) +
  geom_violin() +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  xlab("Weekend") +
  ylab("Facebook Interactions") +
  scale_fill_manual(values=cbbPalette, name="Condition") +
  ggtitle("Facebook Interactions Per Poem")

## VIOLIN PLOT
ggplot(poems, aes(weekend, interactions, fill=condition)) +
  geom_boxplot() +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  xlab("Weekend") +
  ylab("Facebook Interactions") +
  scale_fill_manual(values=cbbPalette, name="Condition") +
  ggtitle("Facebook Interactions Per Poem")




########################################################
#### STATISTICAL MODELS

#################
#### BASE MODELS:

## LINEAR REGRESSION
summary(base.linear <- lm(interactions ~ condition, data=poems))

## LOG-TRANSFORMED LINEAR REGRESSION
summary(base.log.linear <- lm(log1p(interactions) ~ condition, data=poems))


#####################
#### TABLES:

screenreg(list(base.linear, base.log.linear),
          custom.model.names=c("Linear", "Log-Transformed"),
          include.adjrs=FALSE,
          custom.coef.names = c("(Intercept)", "Color"),
          custom.note = "Linear models estimating log-transformed\nlikes, comments, and shares per poem")

#####################
#### PLOT EFFECT

dwplot(base.log.linear, dot_args = list(size=2)) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(plot.margin = unit(c(1, 5, 1, 1), "mm"),
        axis.title.x=element_text(size=18, hjust=0, color="#555555"),
        axis.title.y=element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=18),
        plot.title=element_text(size=20, hjust=0)) +
  scale_color_discrete(guide=FALSE) +
  xlim(0,2) +
  ggtitle("TITLE HERE") + 
  xlab(paste("                                          EFFECT LEGEND HERE\n\n",
             "This line indicates.... \n", 
             "(n = ", nrow(poems), " poems published from ",
             min(as.POSIXct(poems$date))," to ",
             max(as.POSIXct(poems$date)), "\n",
             "by NAME A, NAME BY", sep=""))


#############################################
#############################################
## EXTRA MATERIALS (NOT NEEDED FOR WEEK ONE)
#####################


## NEGATIVE BINOMIAL MODEL
## (ignore in week one assignment)
summary(base.neg.bin <- glm.nb(interactions ~ condition, data=poems))

#####################
#### ADJUSTED MODELS:
#### (IGNORE IN WEEK ONE ASSIGNMENTS)

## LINEAR REGRESSION
summary(adj.linear <- lm(interactions ~ condition + weekend, data=poems))

## LOG-TRANSFORMED LINEAR REGRESSION
summary(adj.log.linear <- lm(log1p(interactions) ~ condition + weekend, data=poems))

## NEGATIVE BINOMIAL MODEL
summary(adj.neg.bin <- glm.nb(interactions ~ condition + weekend, data=poems))


#####################
#### TABLES:


## TABLE FOR LINEAR REGRESSION
screenreg(list(base.linear, adj.linear),
          custom.model.names = c("Baseline", "Adjusted"),
          custom.coef.names = c("(Intercept)", "Color", "Weekend"),
          include.adjrs=FALSE,
          custom.note = "Linear model estimating \nlikes, comments, and shares per poem")


## TABLE FOR LOG-TRANSFORMED LINEAR REGRESSION
screenreg(list(base.log.linear, adj.log.linear),
          custom.model.names=c("Baseline", "Adjusted"),
          include.adjrs=FALSE,
          custom.coef.names = c("(Intercept)", "Color", "Weekend"),
          custom.note = "Linear model estimating log-transformed\nlikes, comments, and shares per poem")

## TABLE FOR NEGATIVE BINOMIAL MODEL
screenreg(list(base.neg.bin, adj.neg.bin),
          custom.model.names=c("Baseline", "Adjusted"),
          custom.coef.names = c("(Intercept)", "Color", "Weekend"),
          custom.note = "Negative binomial model estimating \nthe incidence rate of likes, comments, \nand shares per poem")
