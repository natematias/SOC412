# Cornhole Challenge Experiment (Week 2-3 Optional Assignment)

**SOC 412: Designing Field Experiments at Scale**

Princeton University Department of Sociology

J. Nathan Matias ([@natematias](https://twitter.com/natematias))

## Who Should Choose This Assignment
The main assignment for this week is the **[Facebook Color Experiment](../2-facebook-color/)**.  If you do not feel comfortable conducting this study in your own social network, you may conduct the Cornhole Challenge as an alternative. You will need to agree with your partner on which assignment to take on, so please make a decision ASAP. Declining to conduct this study will not reflect on your grades and will not affect your standing with the instructor, your department, or Princeton University.

## The Cornhole Challenge

How does the design of a game interact with a person's skill and experience to influence a person's final score? [The Cornhole Challenge (https://civic.mit.edu/blog/natematias/the-cornhole-experiment-a-workshop-for-teaching-randomized-trials) is a study that asks this question, and also informs participants about how to conduct a field experiment.

In this exercise, you will organize a game of skill or chance (a Cornhole kit is available), where you allocate people to one of two versions of the game, survey them about their prior sports experience, and compare the scores between the two versions. You then analyze the data and discuss the results with the people who participated in the game.

## Purpose of the assignment
This assignment will support you to:

* Complete a full field experiment from start to finish in two weeks
* Think about the ethics of experimentation with the people in your life, as a precursor to doing experiments in other people's lives
* Debrief people about the experiment process
* Communicate experiment results to a public audience
* Collect data that we will be able to use to analyze multiple field experiments

## Eligible Games
This study has been optimized for the game of [cornhole](https://en.wikipedia.org/wiki/Cornhole). A portable cornhole board may be available for this assignment upon request.

While the easiest games to set up for this assignment will involve some kind of throwing at a goal, the research ethics protocol includes the following games: 

> physical games of skill and chance such as cornhole, shuffleboard, bocce/bowls, croquet, (non-alcoholic) beer pong, (velcro) darts, velcro soccerball darts, paper football, ladder toss (involving ping pong balls), horseshoes, [rutabaga curling](http://www.rutabagacurl.com/), washer toss, [pancake races](https://www.youtube.com/watch?v=jxAICVug-54), the Egg Drop Challenge, pick-up sticks, and giant Jenga

To improve the quality of the data collection, alcohol should not be included at the gathering where this study is conducted.

## Study Procedure
To conduct this study, you will need to carry out the following procedures:

* Acquire all materials for the experiment and **do a test run** of the study
  * Time things so you can be sure to fit the study procedures in the available time
* Schedule **an event for the the experiment** and invite people to join. The event should be 12 people or more. You will need to **conduct a version of this event at least two times**.
  * The venue should be large enough for two versions of the game in question
  * You will want a small table for providing people with consent information to read, and where they can complete the form
  * You may also want a chair or table for the people who are entering scores
  * You may also wish to set up a screen in the venue, where you can display any results 
* This study can be conducted in two ways:
  * As a group event where everyone shows up for a period of time and conducts the study together
  * As an ongoing event, where people are enrolled as they arrive (for example, outside of dinner)
* Hold the event, where you conduct the study. At the end of the event, you should analyze the results and discuss them with participants

Advertisements for your event should include the following statement:

> This workshop will include an optional chance to participate in a practical example of an experiment, including a few anonymous survey questions. The purpose of the experiment will be to observe the effects of personal characteristics and game design on the outcomes of the game. Anyone interested in attending the workshop can opt out of the experiment and still join the workshop.

Within the event, here are the procedures to follow. Because this assignment involves playing two different versions of the game, you may wish to split up investigators and also ask participants to help with recording scores.

* For each participant:
  * Show them the [consent information](CONSENT.md) 
  * If they wish to continue
   * Assign them a random number
   * Ask them to take [a version of this survey that you have customized](https://goo.gl/forms/VTmg4HWL58zVWFGn1), and explain that completing the survey indicates consent
  * Assign them to one of the two versions of the game
  * Record their scores (this is most easily done by asking a participant to record scores a Google Spreadsheet by entering the values as others make their throws)
    * Scoring criteria for cornhole are included in [this blog post](https://civic.mit.edu/blog/natematias/the-cornhole-experiment-a-workshop-for-teaching-randomized-trials) 
* Repeat this procedure until all participants have completed the game
* Prepare the data:
  * Copy-paste answers from the Google Form into the scores spreadsheet, duplicating a person's answers alongside every throw they made
    * [Google spreadsheet example](https://docs.google.com/spreadsheets/d/17ZT7xQfjl9XKXt_Ixt6bIVKkW_BocSiqM6lOJj8iJdc/edit?usp=sharing)
    * [Excel spreadsheet example](Cornhole-Challenge-RCT-Example-Spreadsheet.xlsx)
* Analyze the data:
  * Estimate the average treatment effect on the chance of someone to score ( you can [publish the data to the web directly from Google Sheets as a CSV](https://support.google.com/docs/answer/37579?co=GENIE.Platform%3DDesktop&hl=en) and [load it directly into R](https://www.r-bloggers.com/getting-data-from-an-online-source/))
    * The R script [cornhole-analysis.R](cornhole-analysis.R) includes example code for loading data directly from Google spreadsheets
  * Illustrate the effect in a chart
* Share the results with participants, asking them to discuss the question of whether they prefer a particular version of the game, and encouraging them to incorporate what they learned about the game after conducting the analysis. Take notes during or after the event, since you will write about this discussion later.

## Assignment
For this assignment, work in pairs to discuss your intervention, create experiment records, check in on research validity, and write results.

In the **first week**, you will be expected to conduct your first experiment and write up the results in a report. The report should include:
* a paragraph **describing the experiment design**
* a paragraph **summarizing the findings**
* graphics:
  * illustrating the interventions
  * illustrating the findings

Please submit the assignment via Blackboard in HTML or Word format using the naming convention SURNAME1-SURNAME2-CORNHOLE-1.doc

In the **second week**, you will conduct the experiment a second time. Building on data from both studies you will write:
1. A **4-6 paragraph personal essay** that asks the question: **what contribution could experiments make to decisions about games of skill and chance, and what are the limitations?** Base this on your own experience and on the discussion with participants about which version of the game they prefer. You are encouraged to discuss ideas with your partner, but your essay should be solely your own work
2. A **joint essay reporting your results**: this essay combines data from both of your studies and reports the collective results (details TBA)
