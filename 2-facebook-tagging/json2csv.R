## This script converts the friends.json file
## to friends.csv
library(jsonlite)
friends <- fromJSON(paste(readLines(file("personal-data/friends.json", encoding = "UTF-8")), collapse=" "))$friends[c('name', 'timestamp')]
write.csv(friends, file="personal-data/friends.csv")
