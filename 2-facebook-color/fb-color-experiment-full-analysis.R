library(ggplot2)
library(texreg)
library(psych)
library(MASS)
library(gmodels)
library(corrplot)
library(dotwhisker)
library(lme4)
library(lmerTest)
library(lubridate)
library(blockrand)
library(ri2)
library(cowplot)

rm(list=ls())
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


########################################################
#### LOAD DATA: posts  #################################
#### Each record is the (simulated) 24 hour performance
####   of a Post shared on Facebook
#### Experiment procedure: 
#### https://medium.com/@natematias/how-anyone-can-audit-facebooks-newsfeed-b879c3e29015
#### 
#### wday: weekday integer (1-7, is Sunday)
#### weekend: binary (Sat & Sun = TRUE)
#### interactions: number of likes, comments, and shares on Facebook
#### date: the date
#### day.num: an integer indicating the day after the experiment started
#### prediction: the prediction made by the researcher, where available (not available for JMM)
#### experimenter: the experimenter
#### date: the date


all.posts <- read.csv("full-student-JNM-results-spring-2018.csv")
all.posts$condition <- relevel(all.posts$condition, ref="plain")
all.posts$date <- parse_date_time(all.posts$day, "%m/%d/%y")
all.posts$wday <- wday(all.posts$date)
all.posts$weekend <- (all.posts$wday == 7 | all.posts$wday==1)
all.posts$experimenter <- paste(all.posts$Experimenter.ID, all.posts$Team.Name, sep="")
experimenters <- unique(all.posts$experimenter)


## IMPUTE MEAN ON MISSED OBSERVATIONS
## ALL MISSING OBVS ARE FROM 2mms
all.posts$interactions[is.na(all.posts$likes)==TRUE] <- mean(subset(all.posts,experimenter=="2mms")$interactions)

########################################################
#### UNIVARIATE EXPLORATION
summary(all.posts$interactions)
hist(all.posts$interactions)
hist(log1p(all.posts$interactions))

summary(all.posts$weekend)
summary(factor(all.posts$wday))

ggplot(all.posts, aes(interactions)) +
  geom_histogram(binwidth=5) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ylab("Number of posts") +
  xlab("Facebook Interactions") +
  scale_color_manual(values=cbbPalette, name="Intervention") +
  ggtitle("Histogram of Facebook Interactions Per Post")

ggplot(all.posts, aes(log1p(interactions))) +
  geom_histogram(binwidth=0.25) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ylab("ln Number of posts") +
  xlab("Facebook Interactions") +
  scale_color_manual(values=cbbPalette, name="Intervention") +
  ggtitle("Histogram of log-transformed Facebook Interactions Per Post")


########################################################
#### BIVARIATE EXPLORATION

## SUMMARY STATISTICS BY CONDITION (INTERACTIONS)
describeBy(all.posts$interactions, all.posts$condition)
describeBy(log1p(all.posts$interactions), all.posts$condition)

## SUMMARY STATISTICS BY CONDITION (TEXT LENGTH)
describeBy(all.posts$text.length, all.posts$condition)
describeBy(log1p(all.posts$text.length), all.posts$condition)

## CHECK BALANCE BETWEEN TEXT LENGTH AND CONDITION
summary(lm(log1p(text.length) ~ condition, data=all.posts))

## SUMMARY STATISTICS BY WEEKEND 
describeBy(all.posts$interactions, all.posts$weekend)
describeBy(log1p(all.posts$interactions), all.posts$weekend)


## PREDICTIONS
## OMITTED SINCE COMBINED DATASET DOESN'T ALWAYS INCLUDE PREDICTIONS
## summary(lm(log1p(all.posts$interactions) ~ prediction, all.posts))

## CHECK FOR BALANCE IN PREDICTIONS: NOT BALANCED
## QUESTION: WERE YOU INCORPORATING THE TREATMENT EFFECT
## INTO YOUR PREDICTION?? 
## summary(lm(prediction ~ condition, all.posts))

## WEEKEND TO CONDITION (CHECK BALANCE)
CrossTable(all.posts$weekend, all.posts$condition, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE)

## PLOT POINTS BY DAY PER EXPERIMENTER
ggplot(all.posts, aes(Post.ID, interactions, color=condition)) +
  geom_point(size=1.5) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  xlab("Day Number") +
  ylab("Facebook Interactions") +
  scale_color_manual(values=cbbPalette, name="Intervention") +
  ggtitle("Social Media Interactions Per Post") +
  facet_grid(experimenter ~ ., scales="free")

## BOXPLOT OF THE EXPERIMENT PER EXPERIMENTER
ggplot(all.posts, aes(condition, log1p(interactions), fill=condition)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  xlab("Condition") +
  ylab("Facebook Interactions") +
  scale_fill_manual(values=cbbPalette, name="Intervention") +
  ggtitle("ln Facebook Interactions Per Post") +
  facet_grid(experimenter ~ ., scales="free")


########################################################
#### MULTIVARIATE EXPLORATION
all.posts$i.condition <- as.integer(all.posts$condition)

## CORRELATION PLOT
corrplot(cor(all.posts[c("Post.ID", "weekend", "interactions", 'i.condition')]), method="circle")


########################################################
#### STATISTICAL MODELS
########################################################

#######################################
#### AGGREGATE MODELS ACROSS EVERYONE:
#### USING A RANDOM INTERCEPTS MODEL

## RANDOM INTERCEPT MODEL THAT ACCOUNTS FOR DIFFERENCES BETWEEN EXPERIMENTS
summary(ri.log.linear <- lmer(log1p(interactions) ~ condition + (1|experimenter), data=all.posts))

summary(ri.log.linear <- lmer(log1p(interactions) ~ condition + (1|experimenter), data=all.posts))


summary(ri.log.linear.adjusted <- lmer(log1p(interactions) ~ condition + weekend + (1|experimenter), data=all.posts))

## RANDOM INTERCEPT MODEL THAT ACCOUNTS FOR DIFFERENCES BETWEEN EXPERIMENTS (FACEBOOK ONLY)
summary(ri.log.linear.fb <- lmer(log1p(interactions) ~ condition + (1|experimenter), data=subset(all.posts, (Platform=="Facebook"))))

summary(ri.log.linear.fb.adjusted <- lmer(log1p(interactions) ~ condition + weekend + (1|experimenter), data=subset(all.posts, Platform=="Facebook")))

## NEGATIVE BINOMIAL RANDOM INTERCEPT MODELS (NOT REPORTED, USED FOR ILLUSTRATION)
summary(ri.nb <- glmer.nb(interactions ~ condition + (1|experimenter), data=all.posts))
summary(ri.nb.adj <- glmer.nb(interactions ~ condition + weekend + (1|experimenter), data=all.posts))

summary(ri.nb.fb <- glmer.nb(interactions ~ condition + (1|experimenter), data=subset(all.posts, Platform=="Facebook")))
summary(ri.nb.fb.adj <- glmer.nb(interactions ~ condition + weekend + (1|experimenter), data=subset(all.posts, Platform=="Facebook")))

##COMPARE MODELS (ALL POSTS)
screenreg(list(ri.log.linear, ri.log.linear.adjusted, ri.nb, ri.nb.adj))

##COMPARE MODELS (FACEBOOK POSTS)
screenreg(list(ri.log.linear.fb, ri.log.linear.fb.adjusted, ri.nb.fb, ri.nb.fb.adj))


##########################################################
#### GENERATE COEFFICIENTS FOR COMBINED RESULTS
#### FROM NON_ADJUSTED AND ADJUSTED RESULTS, FOR CHARTS
##########################################################

ri.log.linear.confint <- confint(ri.log.linear)
poem.model.ri.log.linear <- data.frame(
  color.coef = summary(ri.log.linear)$coefficients['conditioncolor',][['Estimate']],
  color.coef.lwr = ri.log.linear.confint['conditioncolor',][['2.5 %']],
  color.coef.upr = ri.log.linear.confint['conditioncolor',][['97.5 %']],
  experimenter = "combined"
  )

ri.log.linear.adjusted.confint <- confint(ri.log.linear.adjusted)
poem.model.ri.log.linear.adjusted <- data.frame(
  color.coef = summary(ri.log.linear.adjusted)$coefficients['conditioncolor',][['Estimate']],
  color.coef.lwr = ri.log.linear.adjusted.confint['conditioncolor',][['2.5 %']],
  color.coef.upr = ri.log.linear.adjusted.confint['conditioncolor',][['97.5 %']],
  experimenter = "combined"
)


ri.log.linear.confint.fb <- confint(ri.log.linear.fb)
poem.model.ri.log.linear.fb <- data.frame(
  color.coef = summary(ri.log.linear.fb)$coefficients['conditioncolor',][['Estimate']],
  color.coef.lwr = ri.log.linear.confint.fb['conditioncolor',][['2.5 %']],
  color.coef.upr = ri.log.linear.confint.fb['conditioncolor',][['97.5 %']],
  experimenter = "combined"
)

ri.log.linear.adjusted.confint.fb <- confint(ri.log.linear.fb.adjusted)
poem.model.ri.log.linear.adjusted.fb <- data.frame(
  color.coef = summary(ri.log.linear.fb.adjusted)$coefficients['conditioncolor',][['Estimate']],
  color.coef.lwr = ri.log.linear.adjusted.confint.fb['conditioncolor',][['2.5 %']],
  color.coef.upr = ri.log.linear.adjusted.confint.fb['conditioncolor',][['97.5 %']],
  experimenter = "combined"
)

#################
#### PER-EXPERIMENTER MODELS

post.subset <- subset(all.posts, experimenter==as.character(experimenters[1]))
m1 <- lm(log1p(interactions) ~ condition, data=post.subset)
confints <- confint(m1)
p.m1 <- data.frame(
  condition=(factor(c("color", "plain"))),
  weekend=FALSE
)
p.m1 <- cbind(p.m1, predict(m1, p.m1, interval="predict"))

poem.models <- data.frame(
  intercept  = coef(m1)['(Intercept)'],
  color.coef = coef(m1)['conditioncolor'],
  color.coef.lwr = confints['conditioncolor',][['2.5 %']],
  color.coef.upr = confints['conditioncolor',][['97.5 %']],
  stderr = coef(summary(m1))[2,]['Std. Error'],
  pvalue = coef(summary(m1))[2,]['Pr(>|t|)'],
  significant = coef(summary(m1))[2,]['Pr(>|t|)'] < 0.05,
  color.fit = subset(p.m1, condition=="color")$fit,
  color.lwr = subset(p.m1, condition=="color")$lwr,
  color.upr = subset(p.m1, condition=="color")$upr,
  plain.fit = subset(p.m1, condition=="plain")$fit,
  plain.upr = subset(p.m1, condition=="plain")$upr,
  plain.lwr = subset(p.m1, condition=="plain")$lwr,
  experimenter = experimenters[1],
  sd = sd(log1p(post.subset$interactions)),
  friends = mean(post.subset$Friends),
  platform = as.character(post.subset$Platform[1])
)

for(i in seq(2,length(experimenters))){
  post.subset <- subset(all.posts, experimenter==as.character(experimenters[i]))
  m1 <- lm(log1p(interactions) ~ condition , data=post.subset)
  confints <- confint(m1)
  p.m1 <- data.frame(
    condition=(factor(c("color", "plain"))),
    weekend=FALSE
  )
  p.m1 <- cbind(p.m1, predict(m1, p.m1, interval="predict"))
  model <- data.frame(
    intercept  = coef(m1)['(Intercept)'],
    color.coef = coef(m1)['conditioncolor'],
    color.coef.lwr = confints['conditioncolor',][['2.5 %']],
    color.coef.upr = confints['conditioncolor',][['97.5 %']],
    stderr = coef(summary(m1))[2,]['Std. Error'],
    pvalue = coef(summary(m1))[2,]['Pr(>|t|)'],
    significant = coef(summary(m1))[2,]['Pr(>|t|)'] < 0.05,
    color.fit = subset(p.m1, condition=="color")$fit,
    color.lwr = subset(p.m1, condition=="color")$lwr,
    color.upr = subset(p.m1, condition=="color")$upr,
    plain.fit = subset(p.m1, condition=="plain")$fit,
    plain.upr = subset(p.m1, condition=="plain")$upr,
    plain.lwr = subset(p.m1, condition=="plain")$lwr,
    experimenter = experimenters[i],
    sd = sd(log1p(post.subset$interactions)),
    friends = mean(post.subset$Friends),
    platform = as.character(post.subset$Platform[1])
  )
  poem.models <- rbind(poem.models, model)
}

post.subset <- subset(all.posts, experimenter==as.character(experimenters[1]))
m2 <- lm(log1p(interactions) ~ condition + weekend, data=post.subset)
confints <- confint(m2)
p.m2 <- data.frame(
  condition=(factor(c("color", "plain"))),
  weekend=FALSE
)
p.m2 <- cbind(p.m2, predict(m2, p.m2, interval="predict"))

poem.models.adjusted <- data.frame(
  intercept  = coef(m2)['(Intercept)'],
  color.coef = coef(m2)['conditioncolor'],
  color.coef.lwr = confints['conditioncolor',][['2.5 %']],
  color.coef.upr = confints['conditioncolor',][['97.5 %']],
  stderr = coef(summary(m2))[2,]['Std. Error'],
  pvalue = coef(summary(m2))[2,]['Pr(>|t|)'],
  significant = coef(summary(m1))[2,]['Pr(>|t|)'] < 0.05,
  color.fit = subset(p.m2, condition=="color")$fit,
  color.lwr = subset(p.m2, condition=="color")$lwr,
  color.upr = subset(p.m2, condition=="color")$upr,
  plain.fit = subset(p.m2, condition=="plain")$fit,
  plain.upr = subset(p.m2, condition=="plain")$upr,
  plain.lwr = subset(p.m2, condition=="plain")$lwr,
  experimenter = experimenters[1],
  sd = sd(log1p(post.subset$interactions)),
  friends = mean(post.subset$Friends),
  platform = as.character(post.subset$Platform[1])
)

for(i in seq(2,length(experimenters))){
  post.subset <- subset(all.posts, experimenter==as.character(experimenters[i]))
  m2 <- lm(log1p(interactions) ~ condition + weekend , data=post.subset)
  confints <- confint(m2)
  p.m2 <- data.frame(
    condition=(factor(c("color", "plain"))),
    weekend=FALSE
  )
  p.m2 <- cbind(p.m2, predict(m2, p.m2, interval="predict"))
  model <- data.frame(
    intercept  = coef(m2)['(Intercept)'],
    color.coef = coef(m2)['conditioncolor'],
    color.coef.lwr = confints['conditioncolor',][['2.5 %']],
    color.coef.upr = confints['conditioncolor',][['97.5 %']],
    stderr = coef(summary(m2))[2,]['Std. Error'],
    pvalue = coef(summary(m2))[2,]['Pr(>|t|)'],
    significant = coef(summary(m2))[2,]['Pr(>|t|)'] < 0.05,
    color.fit = subset(p.m2, condition=="color")$fit,
    color.lwr = subset(p.m2, condition=="color")$lwr,
    color.upr = subset(p.m2, condition=="color")$upr,
    plain.fit = subset(p.m2, condition=="plain")$fit,
    plain.upr = subset(p.m2, condition=="plain")$upr,
    plain.lwr = subset(p.m2, condition=="plain")$lwr,
    experimenter = experimenters[i],
    sd = sd(log1p(post.subset$interactions)),
    friends = mean(post.subset$Friends),
    platform = as.character(post.subset$Platform[1])
  )
  poem.models.adjusted <- rbind(poem.models.adjusted, model)
}

#############################
######## ILLUSTRATE MODELS ##
#############################

poem.models$experimenter <- factor(poem.models$experimenter, levels = poem.models$experimenter[order(poem.models$friends)])
poem.models.adjusted$experimenter <- factor(poem.models.adjusted$experimenter, levels = poem.models.adjusted$experimenter[order(poem.models.adjusted$friends)])

##########################################
######## NON-ADJUSTED ALL MODELS        ##
##########################################

ggplot(poem.models, aes(experimenter, color.coef, color=platform)) + 
  geom_hline(yintercept=0, color="black", size=0.8) +
  geom_point(size=2) +
#  geom_hline(yintercept=0.17690, color="gray", size=1.5) +
  # geom_hline(yintercept=mean(poem.models$color.coef), color="gray", size=2) +
  geom_errorbar(aes(ymin=color.coef.lwr, ymax=color.coef.upr), width=0.1, size=0.8) +
  ylim(-0.7,1.5) +
  ylab("Effect on ln interactions") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(legend.position=c(0.09, 0.80),
        axis.text.x = element_blank(),#element_text(angle = 45, hjust = 1, size=18),
        plot.margin = unit(c(1, 5, 1, 1), "mm"),
        axis.title.x=element_text(size=15, hjust=0, color="#555555"),
        axis.title.y=element_text(size=18),
        axis.text.y=element_text(size=18),
        plot.title=element_text(size=20, hjust=0)) +
  scale_color_manual(values=cbbPalette[c(2, 3, 7)], name="Platform") +
#  ggtitle("Using a Colored Background DOES SOMETHING\nby SOMETHING on average SOMEWHERE") +
   xlab(paste("(n=",nrow(poem.models)," experiments conducted on Facebook, Twitter, and Instagram)",
              "\n", "Linear models estimating effect on log-transformed interactions.",
              "\n", "Experiment by SOC 412: github.com/natematias/SOC412",sep="")) +
  ggsave("charts/poem.models.linear.all.png", width=200, height=105, units="mm")


ggplot(poem.model.ri.log.linear, aes(experimenter, color.coef)) +
  geom_hline(yintercept=0, color="black", size=0.8) +
  geom_point(size=2, color=cbbPalette[1]) +
  geom_errorbar(aes(ymin=color.coef.lwr, ymax=color.coef.upr), width=0.05, size=0.8, color=cbbPalette[1]) +
  #  geom_hline(yintercept=0.17690, color="gray", size=1.5) +
  # geom_hline(yintercept=mean(poem.models$color.coef), color="gray", size=2) +
  ylab("Effect on ln interactions") +
  ylim(-0.7,1.5) +
  xlab(paste("Combined effect across\n",nrow(poem.models), " field experiments\n", "random intercepts (p=",sprintf("%.3f", summary(ri.log.linear)$coefficients['conditioncolor',][['Pr(>|t|)']]),')', sep="")) +
  #xlab(paste("(n=",length(experimenters)," experiments conducted on Facebook, Twitter, and Instagram)", sep="")) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  geom_text(size=4, hjust = 0, nudge_x = 0.05, label=paste(sprintf("%0.2f", poem.model.ri.log.linear$color.coef), "\n(",
                                                           sprintf("%2.0f", round(exp(poem.model.ri.log.linear$color.coef)*100-100)),
                                                           "%)",sep="")) +
  geom_text(aes(x=c(0.52), y=c(1.5), label=c("Combined Results"), hjust=c(0)), size=5) +
  theme(axis.text.x = element_blank(),
        plot.margin = unit(c(1, 5, 1, 1), "mm"),
        axis.title.x=element_text(size=15, hjust=0, color="#555555"),
        axis.title.y=element_text(size=18),
        axis.text.y=element_text(size=18),
        plot.title=element_text(size=20, hjust=0)) +
  scale_color_manual(values=cbbPalette, name="Facebook") +
  # xlab(paste("(n=",nrow(poem.models)," experiments conducted on Facebook, Twitter, and Instagram)", 
  #            "\n", "Results of a log-transformed linear regression. Experiment by SOC 412",
  #            "\n", "github.com/natematias/SOC412",sep="")) +
  ggsave("charts/poem.models.linear.combined.png", width=90, height=105, units="mm")


###############################
## ADJUSTED MODEL CHARTS     ##
###############################

ggplot(poem.models.adjusted, aes(experimenter, color.coef, color=platform)) + 
  geom_hline(yintercept=0, color="black", size=0.8) +
  geom_point(size=2) +
  #  geom_hline(yintercept=0.17690, color="gray", size=1.5) +
  # geom_hline(yintercept=mean(poem.models$color.coef), color="gray", size=2) +
  geom_errorbar(aes(ymin=color.coef.lwr, ymax=color.coef.upr), width=0.1, size=0.8) +
  ylim(-0.7,1.5) +
  ylab("Effect on ln interactions") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(legend.position=c(0.09, 0.80),
        axis.text.x = element_blank(),#element_text(angle = 45, hjust = 1, size=18),
        plot.margin = unit(c(1, 5, 1, 1), "mm"),
        axis.title.x=element_text(size=15, hjust=0, color="#555555"),
        axis.title.y=element_text(size=18),
        axis.text.y=element_text(size=18),
        plot.title=element_text(size=20, hjust=0)) +
  scale_color_manual(values=cbbPalette[c(2, 3, 7)], name="Platform") +
  #  ggtitle("Using a Colored Background DOES SOMETHING\nby SOMETHING on average SOMEWHERE") +
  xlab(paste("(n=",nrow(poem.models.adjusted)," experiments conducted on Facebook, Twitter, and Instagram)",
             "\n", "Linear models estimating effect on log-transformed interactions, adjusted by",
             "\n", "weekend. Experiment by SOC 412: github.com/natematias/SOC412",sep="")) +
  ggsave("charts/poem.models.linear.all.adjusted.png", width=200, height=105, units="mm")


ggplot(poem.model.ri.log.linear.adjusted, aes(experimenter, color.coef)) +
  geom_hline(yintercept=0, color="black", size=0.8) +
  geom_point(size=2, color=cbbPalette[1]) +
  geom_errorbar(aes(ymin=color.coef.lwr, ymax=color.coef.upr), width=0.05, size=0.8, color=cbbPalette[1]) +
  #  geom_hline(yintercept=0.17690, color="gray", size=1.5) +
  # geom_hline(yintercept=mean(poem.models$color.coef), color="gray", size=2) +
  ylab("Effect on ln interactions") +
  ylim(-0.7,1.5) +
  xlab(paste("Combined effect across\n",nrow(poem.models), " field experiments\n", "random intercepts (p=",sprintf("%.3f", summary(ri.log.linear.adjusted)$coefficients['conditioncolor',][['Pr(>|t|)']]),')', sep="")) +
  #xlab(paste("(n=",length(experimenters)," experiments conducted on Facebook, Twitter, and Instagram)", sep="")) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  geom_text(size=4, hjust = 0, nudge_x = 0.05, label=paste(sprintf("%0.2f", poem.model.ri.log.linear.adjusted$color.coef), "\n(",
                                                           sprintf("%2.0f", round(exp(poem.model.ri.log.linear.adjusted$color.coef)*100-100)),
                                                           "%)",sep="")) +
  geom_text(aes(x=c(0.52), y=c(1.5), label=c("Combined Results"), hjust=c(0)), size=5) +
  theme(axis.text.x = element_blank(),
        plot.margin = unit(c(1, 5, 1, 1), "mm"),
        axis.title.x=element_text(size=15, hjust=0, color="#555555"),
        axis.title.y=element_text(size=18),
        axis.text.y=element_text(size=18),
        plot.title=element_text(size=20, hjust=0)) +
  scale_color_manual(values=cbbPalette, name="Facebook") +
  # xlab(paste("(n=",nrow(poem.models)," experiments conducted on Facebook, Twitter, and Instagram)", 
  #            "\n", "Results of a log-transformed linear regression. Experiment by SOC 412",
  #            "\n", "github.com/natematias/SOC412",sep="")) +
  ggsave("charts/poem.models.linear.combined.adjusted.png", width=90, height=105, units="mm")


############################################
## NON-ADJUSTED FACEBOOK MODEL CHARTS     ##
############################################


ggplot(subset(poem.models, platform=="Facebook"), aes(experimenter, color.coef)) + 
  geom_hline(yintercept=0, color="black", size=0.8) +
  geom_point(size=2, color=cbbPalette[3]) +
  #  geom_hline(yintercept=0.17690, color="gray", size=1.5) +
  # geom_hline(yintercept=mean(poem.models$color.coef), color="gray", size=2) +
  geom_errorbar(aes(ymin=color.coef.lwr, ymax=color.coef.upr), width=0.1, size=0.8, color=cbbPalette[3]) +
  ylim(-0.7,1.5) +
  ylab("Effect on ln interactions") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(legend.position=c(0.09, 0.80),
        axis.text.x = element_blank(),#element_text(angle = 45, hjust = 1, size=18),
        plot.margin = unit(c(1, 5, 1, 1), "mm"),
        axis.title.x=element_text(size=15, hjust=0, color="#555555"),
        axis.title.y=element_text(size=18),
        axis.text.y=element_text(size=18),
        plot.title=element_text(size=20, hjust=0)) +
  #  scale_color_manual(values=cbbPalette[c(2, 3, 7)], name="Platform") +
  #  ggtitle("Using a Colored Background DOES SOMETHING\nby SOMETHING on average SOMEWHERE") +
  xlab(paste("(n=",nrow(subset(poem.models, platform=="Facebook"))," experiments conducted on Facebook)",
             "\n", "Linear models estimating effect on log-transformed interactions.",
             "\n", "Experiment by SOC 412: github.com/natematias/SOC412",sep="")) +
  ggsave("charts/poem.models.linear.fb.png", width=200, height=105, units="mm")


ggplot(poem.model.ri.log.linear.fb, aes(experimenter, color.coef)) +
  geom_hline(yintercept=0, color="black", size=0.8) +
  geom_point(size=2, color=cbbPalette[1]) +
  geom_errorbar(aes(ymin=color.coef.lwr, ymax=color.coef.upr), width=0.05, size=0.8, color=cbbPalette[1]) +
  #  geom_hline(yintercept=0.17690, color="gray", size=1.5) +
  # geom_hline(yintercept=mean(poem.models$color.coef), color="gray", size=2) +
  ylab("Effect on ln interactions") +
  ylim(-0.7,1.5) +
  xlab(paste("Combined effect across\n",nrow(subset(poem.models, platform=="Facebook")), " field experiments\n", "random intercepts (p=",sprintf("%.3f", summary(ri.log.linear.fb.adjusted)$coefficients['conditioncolor',][['Pr(>|t|)']]),')', sep="")) +
  #xlab(paste("(n=",length(experimenters)," experiments conducted on Facebook, Twitter, and Instagram)", sep="")) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  geom_text(size=4, hjust = 0, nudge_x = 0.05, label=paste(sprintf("%0.2f", poem.model.ri.log.linear.fb$color.coef), "\n(",
                                                           sprintf("%2.0f", round(exp(poem.model.ri.log.linear.fb$color.coef)*100-100))
                                                           ,"%)",sep="")) +
  geom_text(aes(x=c(0.52), y=c(1.5), label=c("Combined Results"), hjust=c(0)), size=5) +
  theme(axis.text.x = element_blank(),
        plot.margin = unit(c(1, 5, 1, 1), "mm"),
        axis.title.x=element_text(size=15, hjust=0, color="#555555"),
        axis.title.y=element_text(size=18),
        axis.text.y=element_text(size=18),
        plot.title=element_text(size=20, hjust=0)) +
  # xlab(paste("(n=",nrow(poem.models)," experiments conducted on Facebook, Twitter, and Instagram)", 
  #            "\n", "Results of a log-transformed linear regression. Experiment by SOC 412",
  #            "\n", "github.com/natematias/SOC412",sep="")) +
  ggsave("charts/poem.models.linear.combined.fb.png", width=90, height=105, units="mm")



########################################
## ADJUSTED FACEBOOK MODEL CHARTS     ##
########################################

ggplot(subset(poem.models.adjusted, platform=="Facebook"), aes(experimenter, color.coef)) + 
  geom_hline(yintercept=0, color="black", size=0.8) +
  geom_point(size=2, color=cbbPalette[3]) +
  #  geom_hline(yintercept=0.17690, color="gray", size=1.5) +
  # geom_hline(yintercept=mean(poem.models$color.coef), color="gray", size=2) +
  geom_errorbar(aes(ymin=color.coef.lwr, ymax=color.coef.upr), width=0.1, size=0.8, color=cbbPalette[3]) +
  ylim(-0.7,1.5) +
  ylab("Effect on ln interactions") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(legend.position=c(0.09, 0.80),
        axis.text.x = element_blank(),#element_text(angle = 45, hjust = 1, size=18),
        plot.margin = unit(c(1, 5, 1, 1), "mm"),
        axis.title.x=element_text(size=15, hjust=0, color="#555555"),
        axis.title.y=element_text(size=18),
        axis.text.y=element_text(size=18),
        plot.title=element_text(size=20, hjust=0)) +
#  scale_color_manual(values=cbbPalette[c(2, 3, 7)], name="Platform") +
  #  ggtitle("Using a Colored Background DOES SOMETHING\nby SOMETHING on average SOMEWHERE") +
  xlab(paste("(n=",nrow(subset(poem.models.adjusted, platform=="Facebook"))," experiments conducted on Facebook)",
             "\n", "Linear models estimating effect on log-transformed interactions, adjusted by",
             "\n", "weekend. Experiment by SOC 412: github.com/natematias/SOC412",sep="")) +
  ggsave("charts/poem.models.linear.fb.adjusted.png", width=200, height=105, units="mm")


ggplot(poem.model.ri.log.linear.adjusted.fb, aes(experimenter, color.coef)) +
  geom_hline(yintercept=0, color="black", size=0.8) +
  geom_point(size=2, color=cbbPalette[1]) +
  geom_errorbar(aes(ymin=color.coef.lwr, ymax=color.coef.upr), width=0.05, size=0.8, color=cbbPalette[1]) +
  #  geom_hline(yintercept=0.17690, color="gray", size=1.5) +
  # geom_hline(yintercept=mean(poem.models$color.coef), color="gray", size=2) +
  ylab("Effect on ln interactions") +
  ylim(-0.7,1.5) +
  xlab(paste("Combined effect across\n",nrow(subset(poem.models, platform=="Facebook")), " field experiments\n", "random intercepts (p=",sprintf("%.3f", summary(ri.log.linear.fb.adjusted)$coefficients['conditioncolor',][['Pr(>|t|)']]),')', sep="")) +
  #xlab(paste("(n=",length(experimenters)," experiments conducted on Facebook, Twitter, and Instagram)", sep="")) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  geom_text(size=4, hjust = 0, nudge_x = 0.05, label=paste(sprintf("%0.2f", poem.model.ri.log.linear.adjusted.fb$color.coef), "\n(",
                                                           sprintf("%2.0f", round(exp(poem.model.ri.log.linear.adjusted.fb$color.coef)*100-100))
                                                           ,"%)",sep="")) +
  geom_text(aes(x=c(0.52), y=c(1.5), label=c("Combined Results"), hjust=c(0)), size=5) +
  theme(axis.text.x = element_blank(),
        plot.margin = unit(c(1, 5, 1, 1), "mm"),
        axis.title.x=element_text(size=15, hjust=0, color="#555555"),
        axis.title.y=element_text(size=18),
        axis.text.y=element_text(size=18),
        plot.title=element_text(size=20, hjust=0)) +
  # xlab(paste("(n=",nrow(poem.models)," experiments conducted on Facebook, Twitter, and Instagram)", 
  #            "\n", "Results of a log-transformed linear regression. Experiment by SOC 412",
  #            "\n", "github.com/natematias/SOC412",sep="")) +
  ggsave("charts/poem.models.linear.combined.fb.adjusted.png", width=90, height=105, units="mm")




############################################
## INVESTIGATE RELATIONSHIPS AMONG MODELS ##
############################################

summary(lm(color.coef ~ log1p(friends), data=poem.models))
ggplot(poem.models, aes(log1p(friends), color.coef, color=platform)) +
  geom_point(size=2) +
  xlim(4,8) +
  geom_smooth(method="lm", color="black") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(axis.text.x = element_text( size=15),
        plot.margin = unit(c(1, 5, 1, 1), "mm"),
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18),
        axis.text.y=element_text(size=18),
        plot.title=element_text(size=18, hjust=0)) +
  ggtitle("Experimenters with more friends saw higher effects on average")

##### CONSTRUCT MODELS DATASET

write.table(poem.models, "poem.models.03.14.2018.csv",sep=",")
write.table(poem.models.adjusted, "poem.models.adjusted.03.14.2018.csv",sep=",")
