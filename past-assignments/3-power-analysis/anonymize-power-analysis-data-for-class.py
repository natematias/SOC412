import inspect, os, sys, copy, pytz, re, glob, csv, random
import pandas as pd
from dateutil import parser
import datetime
import numpy as np
from collections import Counter, defaultdict

all_posts = []
with open(sys.argv[1], "r") as f:
    for line in csv.DictReader(f):
        if 'body.length' in line.keys():
            del line['body.length']
        if 'url' in line.keys():
            del line['url']
        if(line['visible']=='None'):
            line['visible']='True'
        line['visible'] = int(line['visible']=="True")
        all_posts.append(line)

## ANONYMIZE AUTHORS
authors = {}
unique_authors = len(set([x['author'] for x in all_posts]))
for post in all_posts:
    if(post['author'] in authors.keys()):
        post['anon.author'] = authors[post['author']]
    else:
        random_index = random.randint(1,unique_authors*100)
        while(random_index in authors.values()):
            random_index = random.randint(1,unique_authors*100)
        authors[post['author']] = random_index
        post['anon.author'] = random_index

## FUZZ DATES/TIMES
for post in all_posts:
    t = parser.parse(post['created.utc'])
    new_t = t + datetime.timedelta(seconds=random.randint(-600, 600))
    post['created.utc'] = str(new_t)


## ANONYMIZE POST IDs
i = 0
for post in all_posts:
    post['id'] = i
    i+= 1

## REMOVE REMAINING FIELDS AND OUTPUT TO FILE
for post in all_posts:
    del post['author']

pd.DataFrame(all_posts).to_csv(sys.argv[2])
