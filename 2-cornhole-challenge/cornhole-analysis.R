library(ggplot2)
library(stargazer)
library(gmodels)
library(texreg)
library(rms)
library(stargazer)
library(data.table)

rm(list=ls())
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

throws <- fread("https://docs.google.com/spreadsheets/d/e/2PACX-1vTH0yKJIxMJAYQFoIemQw8k9p9SPKflVBpaU0tYzvhkVlUJe0Cz8LmGkOlByWxxRsLqA5t7wvwxWnni/pub?gid=1213554876&single=true&output=csv")

throws$any.score <- (throws$in.hole + throws$on.side) > 0

### UNIVARIATE STATISTICS
summary(factor(throws$condition))
summary(throws$height.in.inches)
summary(throws$in.hole)
summary(throws$on.side)
summary(throws$throw)
summary(factor(throws$corrective.eyewear))
summary(factor(throws$major))
summary(factor(throws$organized.sports))
summary(factor(throws$coordination.hobbies))


### REST OF CODE TBD

