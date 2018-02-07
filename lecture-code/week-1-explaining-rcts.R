library(ggplot2)
library(MASS)

rm(list=ls())

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

### SIMULATE CORRELATION
set.seed(423)

simdata <- mvrnorm(100, mu = c(0,0), Sigma = matrix(c(2,0.05,0.05,2), ncol = 2),
        empirical = TRUE)

cor.data <- data.frame(dep=simdata[,1], screen=simdata[,2])

ggplot(cor.data, aes(screen, dep)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlim(-4,4) +
  ylim(-4,4) +
  ylab("Depression")+
  xlab("Screen Time") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  ggtitle("Simulated 0.05 correlation")
  
