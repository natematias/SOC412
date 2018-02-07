library(ggplot2)
library(texreg)
library(psych)
library(MASS)
library(lubridate)

rm(list=ls())

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#################################
### GENERATE INITIAL DATAFRAME  #
#################################

## BIG OUTLIER:
#set.seed(415)

## POSSIBLE CHANGE IN EFFECT
#set.seed(419)

## THIS IS IT. LINEAR MODEL GETS CONFUSED
## AND LOSES STAT SIGNIFICANCE WHEN WEEKEND ADDED
set.seed(424)


poems <- data.frame(
  date =seq(as.Date("2017/01/01"), as.Date("2017/03/31"), by="day")
)
poems$wday <- wday(as.Date(poems$date))
poems$weekend <- (poems$wday == 7 | poems$wday==1)
num.obvs = nrow(poems)
poem.color <- data.frame(interactions =  round(exp(rnorm(num.obvs/2, mean=2.209694))), 
                         condition="Text")
poem.text <- data.frame(interactions = round(exp(rnorm(num.obvs/2, mean=2.77868))), 
                        condition="Color")
testdf <- rbind(poem.text, poem.color)
testdf <- testdf[sample(nrow(testdf)),]
testdf$day.num <- seq(1, num.obvs)
poems$interactions <- testdf$interactions
poems$condition <- testdf$condition
poems$day.num <- testdf$day.num
poems$interactions <- poems$interactions -1

poems$interactions[poems$weekend==TRUE] <- round(poems$interactions[poems$weekend==TRUE] * 0.5)
poems$condition <- relevel(poems$condition, ref="Text")
### SUMMARY PLOTS
ggplot(poems, aes(day.num, interactions, color=condition)) +
  geom_line(size=2) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  xlab("") +
  ylab("") +
  scale_fill_manual(values=cbbPalette, name="Intervention")

### CROSSTABLES
describeBy(poems$interactions, poems$condition)

describeBy(poems$interactions, poems$weekend)

### MODELS
summary(lm(interactions ~ condition, data=poems))
summary(lm(log1p(interactions) ~ condition, data=poems))
summary(glm.nb(interactions ~ condition, data=poems))

summary(lm(interactions ~ condition + weekend, data=poems))
summary(lm(log1p(interactions) ~ condition + weekend, data=poems))
summary(glm.nb(interactions ~ condition + weekend, data=poems))

write.table(poems, "poem-experiment-simulation.csv", sep=",")
