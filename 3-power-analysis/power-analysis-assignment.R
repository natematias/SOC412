## LOAD LIBRARIES
library(ggplot2)
library(corrplot)
library(gmodels)
library(texreg)
library(rms)
library(blockrand)
library(psych)

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
  return (df[sample(nrow(df), n),])
}

## LOAD COLORBLIND SAFE PALETTE
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

####################################
## LOAD DATAFRAMES                ##
####################################
posts <- read.csv("subreddit_posts.csv")
posts$is.selftext <- posts$is.selftext == "True"
posts$weekend <- (posts$weekday == 6 | posts$weekday ==7)

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

## TODO: NUMBER OF COMMENTS REMOVED

## TODO: NUMBER OF NEWCOMER COMMENTS

## TODO: NUMBER OF NEWCOMER COMMENTS REMOVED

####################################
## BIVARIATE ANALYSIS             ##
####################################
corrplot(cor(posts[c('author.prev.participation', 'author.prev.posts', 'front_page', 
            'newcomer.comments', 'newcomer.comments.removed', 'num.comments',
            'num.comments.removed', 'visible')]))

#######################################
## EXAMPLE OF TAKING A RANDOM SAMPLE ##
#######################################

## EXAMPLE FOR TAKING A SAMPLE SMALLER THAN THE SOURCE
posts.10k <-randomSample(posts, 10000)

## EXAMPLE FOR TAKING A SAMPLE LARGER THAN THE SOURCE
posts.80k <- randomSample(posts, 40000)
posts.80k <- rbind(posts.80k, randomSample(posts, 40000))

#######################################################
## EXAMPLE OF AN ITERATION IN A POWER ANALYSIS       ##
#######################################################

## SETTINGS
num.observations = 10000

sim.posts <-randomSample(posts, num.observations)

## GENERATE RANDOMIZATIONS
randomizations <- blockrand(n=nrow(sim.posts), num.levels = 2, block.sizes = c(12,12), id.prefix='post', block.prefix='block',stratum='post')
sim.posts$condition <- head(randomizations$treatment, nrow(sim.posts))

## GENERATE AVERAGE TREATMENT EFFECT FOR NUM.COMMENTS
effect.multiplier = 1.5

sim.posts$num.comments.effect <- 1
sim.posts$num.comments.effect[sim.posts$condition=="B"] <- abs(rnorm(nrow(subset(sim.posts, condition=="B")), 
                                                                     effect.multiplier))
sim.posts$num.comments.sim <- sim.posts$num.comments * sim.posts$num.comments.effect

## PLOT RELATIONSHIP BETWEEN SIMULATED NUMBER AND OBSERVED NUMBER
#ggplot(sim.posts, aes(num.comments, num.comments.sim, color=condition)) +
#  geom_jitter()

## PLOT SIMULATED AVERAGE TREATMENT EFFECT
#ggplot(sim.posts, aes(condition, log1p(num.comments.sim), color=condition)) +
#  geom_violin()

## ESTIMATE AVERAGE TREATMENT EFFECT ON LOG-TRANSFORMED VARIABLE
summary(lm(log1p(num.comments.sim) ~ condition, data=sim.posts))


