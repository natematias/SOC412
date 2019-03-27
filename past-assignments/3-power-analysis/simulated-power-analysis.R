library(ggplot2)
library(texreg)
library(psych)
library(MASS)
library(lubridate)
library(blockrand)

rm(list=ls())

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

####################################
### METHOD FOR SIMULATING A STUDY  #
####################################

set.seed(424)

simulate.study <- function(i, sample.size, control.mean, treat.mean){
  cat(".")
  start.date <- "2017/01/01"
  poems <- data.frame(
    date =seq(as.Date(start.date), as.Date(start.date) + days(sample.size -1), by="day")
  )
  poems$wday <- wday(as.Date(poems$date))
  poems$weekend <- (poems$wday == 7 | poems$wday==1)
  num.obvs = nrow(poems)
  poems$day.num <- seq(1, num.obvs)
  randomizations <- blockrand(n=num.obvs, num.levels = 2, block.sizes = c(4,4), id.prefix='post', block.prefix='block',stratum='post')
#  print(paste("Poem row count", nrow(poems)))
#  print(paste("Randomizations row count", nrow(randomizations)))
  poems$condition <- head(randomizations$treatment, nrow(poems))
  poems$condition <- relevel(poems$condition, ref="A")
  poems$interactions <- NA
  poems$interactions[poems$condition=="A"] <- rnorm(nrow(poems)/2, control.mean) 
  poems$interactions[poems$condition=="B"] <- rnorm(nrow(poems)/2, treat.mean) 
  
  m1 <- lm(interactions ~ condition, data=poems)
  
  power.sim.intercept <- m1$coefficients['(Intercept)']
  power.sim.treat.effect <- m1$coefficients['conditionB']
  treat.coef = coef(summary(m1))[2,]
  power.sim.stderr <- treat.coef['Std. Error']
  power.sim.pvalue <- treat.coef['Pr(>|t|)']
  power.sim.significant <- as.double(power.sim.pvalue) < 0.05
  data.frame(i=i, power.sim.ctl.fit = power.sim.intercept,power.sim.treat.effect=power.sim.treat.effect,
             power.sim.stderr=power.sim.stderr, power.sim.pvalue=power.sim.pvalue,power.sim.significant=power.sim.significant,
             true.effect = treat.mean - control.mean)
}


###################################################
### SIMULATE AND PLOT EXPERIMENTS                ##
### WHERE THERE IS NO DIFFERENCE BETWEEN MEANS   ##
###################################################
num.models.no.effect <- 500
num.days <- 100
mean.ctl.no.effect <- 10
mean.treat.no.effect <- 10

poem.models.none <- simulate.study(1,num.days,mean.ctl.no.effect,mean.treat.no.effect)
for(i in seq(2,num.models.no.effect)){
  poem.models.none <- rbind(poem.models.none, simulate.study(i,num.days,mean.ctl.no.effect,mean.treat.no.effect))
}

## REPORT HOW MANY RESULTS WERE STATISTICALLY SIGNIFICANT
paste(sprintf("%.01f", as.numeric(summary(poem.models.none$power.sim.significant)[['TRUE']])/nrow(poem.models.none)*100), 
      "% results statistically significant", sep="")

## SHOW STUDIES WHERE THE RESULT IS STATISTICALLY SIGNIFICANT
ggplot(poem.models.none, aes(power.sim.significant, power.sim.treat.effect, color=power.sim.significant)) +
  geom_jitter() +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ggtitle(paste(sprintf("%.01f", as.numeric(summary(poem.models.none$power.sim.significant)[['TRUE']])/nrow(poem.models.none)*100), 
                "% results statistically significant in\nsimulated experiments with no effect", sep=""))


#########################################################
### SIMULATE AND PLOT EXPERIMENTS WITH A SMALL SAMPLE  ##
### WHERE THERE iS A DIFFERENCE BETWEEN MEANS          ##
#########################################################
num.models.small.effect <- 100
mean.ctl.small.effect <- 10
mean.treat.small.effect <- 10.2
num.days <- 100

poem.models.small <- simulate.study(1,num.days,mean.ctl.small.effect,mean.treat.small.effect)
for(i in seq(2,num.models.small.effect)){
  poem.models.small <- rbind(poem.models.small, simulate.study(i,num.days,mean.ctl.small.effect,mean.treat.small.effect))
}

## REPORT HOW MANY RESULTS WERE STATISTICALLY SIGNIFICANT
paste(sprintf("%.01f", as.numeric(summary(poem.models.small$power.sim.significant)[['TRUE']])/nrow(poem.models.small)*100), 
      "% results statistically significant", sep="")

## SHOW STUDIES WHERE THE RESULT IS STATISTICALLY SIGNIFICANT
ggplot(poem.models.small, aes(power.sim.significant, power.sim.treat.effect, color=power.sim.significant)) +
  geom_jitter() +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ggtitle(paste(sprintf("%.0f", as.numeric(summary(poem.models.small$power.sim.significant)[['TRUE']])/nrow(poem.models.small)*100), 
                "% results statistically significant\nin simulated experiments", sep=""))

## THE MEAN OF THE EFFECTS CONVERGES ON THE EFFECT
mean(poem.models.small$power.sim.treat.effect)

##########################################################
### POWER ANALYSIS SHOWING HOW MANY OBSERVATIONS TO GET ##
### AN EIGHTY PERCENT CHANCE OF OBSERVING THE EFFECT    ##
##########################################################
power.num.models <- 50
power.mean.ctl <- 10
power.mean.treat <- 10.4
power.max.num.days <- 600
power.starting.num.days <- 12
power.increase.num.days.by <- 12

power.analysis <- function(i, sample.size, control.mean, treat.mean, num.models){
  print(paste("Sample Size:", sample.size, "Control.mean:", control.mean, "Treat.mean:", treat.mean))
  pm <- simulate.study(1,sample.size,control.mean, treat.mean)
  for(i in seq(2,num.models)){
    pm <- rbind(pm, simulate.study(i,sample.size, control.mean,treat.mean))
  }
#  print(paste(nrow(pm), " Models calculated"))
  pct.significant <- as.numeric(summary(pm$power.sim.significant)[['TRUE']])/nrow(pm)*100
  mean.effect.significant <- mean(subset(pm, power.sim.significant ==TRUE)$power.sim.treat.effect)
  mean.effect <-  mean(pm$power.sim.treat.effect)
  
  data.frame(i=i, sample.size=sample.size, pct.significant = pct.significant, 
             mean.effect.significant = mean.effect.significant, 
             mean.effect = mean.effect)
}

### GENERATE AN OUTCOME FOR A SMALL EFFECT
p.analyses <- power.analysis(1,power.starting.num.days,power.mean.ctl,power.mean.treat,power.num.models)
for(i in seq(power.starting.num.days/power.increase.num.days.by, power.max.num.days/power.increase.num.days.by)){
  sample.size <- i*power.increase.num.days.by
  p.analyses <- rbind(p.analyses, power.analysis(i,i*power.increase.num.days.by,power.mean.ctl,power.mean.treat,power.num.models))
}
## generate an analysis of the file drawer
p.analyses$effect.difference <- abs(p.analyses$mean.effect.significant - p.analyses$mean.effect)


### PLOT THE RELATIONSHIP BETWEEN THE SAMPLE SIZE AND STATISTICAL POWER
ggplot(p.analyses, aes(sample.size, pct.significant, color=pct.significant>=80)) +
  geom_point() +
  scale_y_continuous(breaks = round(seq(0,100, by = 10),1)) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ggtitle("The larger the sample size, the greater the chance of observing the effect")


### PLOT THE RELATIONSHIP BETWEEN THE STATISTICAL POWER AND FILE DRAWER
ggplot(p.analyses, aes(pct.significant, effect.difference)) +
  geom_point() +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ggtitle("The larger the file drawer, the greater the absolute bias in the estimate")



###########################################
### SIMULATE A VARIETY OF DECISION RULES  #
###########################################
set.seed(425)


num.models.decision <- 1000
num.days <- 100
mean.ctl.decision <- 10
mean.treat.decision <- 10

poem.models.decision <- simulate.study(1,num.days,mean.ctl.decision,mean.treat.decision + runif(1, -0.5, 0.5))
for(i in seq(2,num.models.decision)){
  poem.models.decision <- rbind(poem.models.decision, simulate.study(i,num.days,mean.ctl.decision,mean.treat.decision + runif(1, -0.5, 0.5)))
}

poem.models.decision$effect.difference <- poem.models.decision$power.sim.treat.effect -  poem.models.decision$true.effect

poem.models.decision$opposite.finding <- (poem.models.decision$power.sim.treat.effect > 0) != (poem.models.decision$true.effect >0)


#### NOW PLOT THE TRUE EFFECTS AS A HISTOGRAM
hist(poem.models.decision$true.effect)

#### NOW PLOT THE TRUE EFFECTS AGAINST THE ESTIMATED EFFECT
ggplot(poem.models.decision, aes(true.effect, power.sim.treat.effect)) +
  geom_point() +
  xlab("True Effect") +
  ylab("Estimated Treatment Effect")


#### NOW PLOT THE INTERVENTIONS THAT EXPERIMENTERS 
#### SHOULD USE TO MAXIMIZE THEIR GOAL IF THE KNEW THE TRUE EFFECT
ggplot(poem.models.decision, aes(power.sim.treat.effect, true.effect, color=factor(poem.models.decision$true.effect>0))) +
  geom_point() +
  #  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = 0) + 
  scale_color_discrete(name="Positive\nEffect") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ylab("True Effect") +
  xlab("Estimated Treatment Effect") +
  ggtitle("Perfect Decisionmaking Would Always Choose\nOnly Positive Effects")

#### NOW PLOT THE TRUE EFFECTS AGAINST THE ESTIMATED EFFECT
#### AND HIGHLIGHT STATISTICALLY-SIGNIFICANT FINDINGS
ggplot(poem.models.decision, aes(power.sim.treat.effect, true.effect, color=factor(poem.models.decision$power.sim.significant))) +
  geom_point() +
  geom_hline(yintercept = 0) + 
  scale_color_discrete(name="Statistically\nSignificant") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ylab("True Effect") +
  xlab("Estimated Treatment Effect")

#### NOW PLOT THE TRUE EFFECTS AGAINST THE ESTIMATED EFFECT
#### AND HIGHLIGHT TYPE S ERRORS
ggplot(poem.models.decision, aes(power.sim.treat.effect, true.effect, color=factor(poem.models.decision$opposite.finding))) +
  geom_point() +
  scale_color_discrete(name="Type S\nErrors") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ylab("True Effect") +
  xlab("Estimated Treatment Effect")

#### NOW ESTIMATE THE OUTCOMES OF VARIOUS DECISION RULES
poem.models.decision.pos.sig <- subset(poem.models.decision, (power.sim.treat.effect > 0 & power.sim.significant))
poem.models.decision.pos <- subset(poem.models.decision, (power.sim.treat.effect > 0))
poem.models.decision.pos.tenpct <- subset(poem.models.decision, (power.sim.treat.effect > 0 & power.sim.pvalue < 0.1))
poem.models.decision.pos.true <- subset(poem.models.decision, (true.effect > 0))

## ADOPTING ALL INTERVENTIONS
paste("Adopting all interventions. Total outcome: ",
      sprintf("%0.1f", sum(poem.models.decision$true.effect)), sep="")

## ADOPTING ALL POSITIVE TRUE EFFECTS
paste(sprintf("%.01f", nrow(poem.models.decision.pos.true) / nrow(subset(poem.models.decision,true.effect>0))*100), 
      "% of positive true effects (optimal decisions). Total outcome: ",
      sprintf("%0.1f", sum(poem.models.decision.pos.true$true.effect)), sep="")

## ADOPTING ALL POSITIVE ESTIMATED EFFECTS
paste(sprintf("%.01f", nrow(poem.models.decision.pos) / nrow(subset(poem.models.decision,true.effect>0))*100), 
      "% of positive interventions chosen if estimated effect positive. Total outcome: ",
      sprintf("%0.1f", sum(poem.models.decision.pos$true.effect)), sep="")

## ADOPTING ALL STATISTICALLY-SIGNIFICANT ESTIMATED EFFECTS
paste(sprintf("%.01f", nrow(poem.models.decision.pos.sig) / nrow(subset(poem.models.decision,true.effect>0))*100), 
      "% of positive interventions chosen if expecting statistical-significance. Total outcome: ",
      sprintf("%0.1f", sum(poem.models.decision.pos.sig$true.effect)), sep="")

## ADOPTING ESTIMATED EFFECTS AT p < 0.1
paste(sprintf("%.01f", nrow(poem.models.decision.pos.tenpct) / nrow(subset(poem.models.decision,true.effect>0))*100), 
      "% of positive interventions chosen if p<0.1 Total outcome: ",
      sprintf("%0.1f", sum(poem.models.decision.pos.tenpct$true.effect)), sep="")

