library(ggplot2)
library(stargazer)
library(gmodels)
library(texreg)
library(rms)
library(stargazer)
library(data.table)

rm(list=ls())
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

beanbags <- fread("beanbag-ktc.csv")
beanbags$weight <- beanbags$`Weight oz`
beanbags$`Weight oz` <- NULL

beanbags <- subset(beanbags, is.na(weight)!=TRUE)

## ANALYZE WEIGHT 

ggplot(beanbags, aes(Batch, weight, fill=factor(Color))) +
  geom_boxplot() + 
  geom_point() +
  ggtitle("Beanbag weight") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(plot.margin = unit(c(1, 5, 1, 1), "mm"),
        axis.title.x=element_text(size=18, hjust=0, color="#555555"),
        axis.title.y=element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=18),
        plot.title=element_text(size=20, hjust=0)) +
  scale_fill_discrete(guide=FALSE) +
  ylim(14, 16) +
  ylab("weight (oz)") +
  ggtitle("Cornhole Challenge Beanbag Weights Vary by less than a Dollar in Quarters") + 
  xlab(paste("                                                      Beanbag Group\n\n",
             "                    (n = ", nrow(beanbags), " beanbags). Measurements by J. Nathan Matias", sep=""))


summary(lm(weight ~ factor(Batch), data=beanbags))
