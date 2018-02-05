# Facebook Poem Experiment (Week 1 Assignment)

**SOC 412: Designing Field Experiments at Scale**

Princeton University Department of Sociology

J. Nathan Matias ([@natematias](https://twitter.com/natematias))

> *Disclaimer: while this assignment includes reference to factual people and events, the assignment, data, and outcomes are a simulated exercise for classroom purposes.*

## Background

One day, you receive an email from the Poetry Foundation, an organization founded in 2003 with [a $185 million gift from the bequest of Ruth Lilly](https://www.forbes.com/global/2011/0117/companies-ruth-lilly-poetry-foundation-no-rhyme-or-reason.html#52b0bc6061ac). The foundation, which operates [a widely-read poetry website](https://poetryfoundation.org) wants to bring poetry to the greatest number of people. They ask you to carry out an evidence-based impact evaluation on the reach and conversation they facilitate on social media, starting with how they share poetry on Facebook.

As an experimenter who's taken SOC412, you convince the foundation to carry out a field experiment to compare the outcomes of different ways to share poems on Facebook. Over 90 days, the Foundation's social media account shares, on its Facebook Page, excerpts of poems with a [colored background](https://techcrunch.com/2016/12/19/facebook-status-background-color/) compared to posting plain text. Posted excerpts all have a similar length. Poems in the experiment are shared at the same time each day, and you consistently collect data about the number of likes, shares, and comments that each poem receives over a 24 hour period of time ([I have written up a similar experiment here](https://medium.com/@natematias/how-anyone-can-audit-facebooks-newsfeed-b879c3e29015)).

Now that the experiment is complete and you have collected all the data, it's time for you to analyze the data and write your first report to the Poetry Foundation. But there's a wrinkle: in the time since you were commissioned to do the study, a social media analytics company offered to provide real-time analytics to the Poetry Foundation based on "predictive models" and "data science." This company argues that A/B testing is slow and wasteful, and that their linear regression models will give the foundation "real-time metrics" that they can use to make daily decisions about how to post poetry.

## Assignment
For this assignment, **work in pairs** to a report for the foundation that describes what you learned, explains the benefits of causal inference, and argues why field experiments could help the foundation evaluate its programs on social media and beyond. Your essay should include:

* a paragraph **describing the experiment design**, including the intervention being tested, the outcome measures being used, how long the study was conducted, and how many poems were included.
* a paragraph **summarizing the findings**. It should describe the range of the dependent variable, the means for each condition, and a statement of the effect size.
* a paragraph that **suggests a course of action**, contextualizing the findings in a way that the organization would normally think about, such as the outcome per month of choosing the course of action you suggest. For example: "If the Poetry Foundation's social media account were to always use <colored/text> backgrounds...." Make sure to reflect on the limitations of the sample, which is drawn from the foundation's page on a single social media platform.
* include a **table of results** and an **illustration of the average treatment effect**. You could (a) show the effect with error bars or (b) show fitted(predicted) values for each condition, with error bars for the treatment (color). If you show fitted values, document details of any covariates(predictors) used to generate the fitted values (such as weekend). 
* a paragraph that builds on this finding in the attempt to **convince the foundation to do more evaluation**, with social media, and more broadly in the organization.

Please submit the assignment via Blackboard in HTML or Word format using the naming convention SURNAME1-SURNAME2-WEEK1-POETRY.doc

## Purpose of the assignment
The main purpose of this assignment is to gain a rough sense of where different students are in the skills that this class will support you to develop

* using R to analyze data, including conduct regression models
* reasoning through the contribution and the limitations of a result
* illustrating results graphically
* writing clear, accurate language about statistical findings in a way that the general public can understand

## Resources
This folder includes everything you need to do this assignment:
* data from the experiment (poem-experiment-simulation.csv)
* code for conducting the analysis (poem-analysis-example.R)

The code does not currently include examples for generating any visual illustrations. If needed, we can walk through this in lab.
