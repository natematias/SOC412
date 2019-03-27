# Analyzing Results from the Social Media Question Experiment (Week 11)
**SOC 412: Designing Field Experiments at Scale**

Princeton University Department of Sociology

J. Nathan Matias ([@natematias](https://twitter.com/natematias))

## Background
In this class assignment:
* Analyze your results from the social media question experiment. 
    * If you were unable to complete the experiment, pick one of the result sets and write about that for the assignment
* Create a chart to illustrate the results ([example in this post](https://medium.com/@natematias/how-anyone-can-audit-facebooks-newsfeed-b879c3e29015)). This illustration should:
    * State the finding
    * Include 95% confidence intervals (up to you if you want a point estimate or not)
    * Label all axes
    * Include the sample size
    * State the time period of the study
    $ State the platform where the study occurred
    * State the estimator
    * Include a treatment example
    * Include your name
* Complete and submit but **do not post** the debriefing essay you wrote to your friends in a previous assignment
* Submit the assignment by 5pm on Friday

## Materials provided
**Data**: I have merged all of the completed experiment results into a single, consistent dataframe, available at *question-experiment-03.26.2019.all.students.csv* in this folder. I have removed your names, removed the text of the posts, and added random "fuzz" to your count of friends to make it slightly harder for anyone to de-anonymize students from the class.

**Code**: I have provided example code for analyzing all of the results at *Analyzing the Question Experiment.ipynb* in this folder. I have included examples for estimating the effect based on the same kind of linear models that we used early in the class.

## What to write in the assignment
Using the code and data, you will need to develop an estimate and a chart for just *your* result. You will then write 1-2 paragraphs about your individual result and what it means. Remember to think about how to illustrate and write about the result to a non-expert audience. When writing, think about what it might mean for your relationship with those in your life that you have estimated the probability that they will respond when you ask for their comments.

* Please **submit the full, updated debriefing note** in your assignment.

* **Do not post this result to your friends**. The final estimates may be different after further analysis.

## Explaining null results
In this study, some of you will have null results. You will have to find a way to explain a null result to the people in your life. To help you explain a null result, you may find [this power calculator on egap](https://egap.shinyapps.io/power-app/) helpful, especially if you choose to report the linear regression / difference in mean estimator. Using this power calculator, you can estimate the smallest effect you would have been realiably able to detect with your individual sample size.

If you have questions about how to explain null results, we can talk more about them in the precept or on Slack.
