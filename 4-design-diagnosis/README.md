# Research Design Diagnosis Assignment

J. Nathan Matias ([natematias.com](https://natematias.com)) ([@natematias](https://twitter.com/natematias))
[SOC412](http://natematias.com/courses/soc412/), April 2019

In this assignment, you work together with your whole team to conduct a power analysis related to your team's project. This will involve:

* Thinking about and stating assumptions about your study
  * What the potential outcomes are for each of the arms of the experiment
  * What statistical tests your team will be conducting
* Editing the power analysis "starting point" to customize the power analysis for your team's assumptions
* Writing a short submission (no more than 2 pages) that reports:
  * Your assumptions:
    * Your reasons for the potential outcomes used
    * Your reasons for the statistical tests used
  * A diagnosis of this design:
    * The minimum sample size you anticipate for getting an 80% chance of observing a statistically-significant effect
    * Any systematic bias in the results
    * Any changes you made leading to this final design, based on the diagnosis
  * The code for your power analysis (Jupyter notebook preferred, but an R script is also acceptable)
  * A contributions statement indicating how each person contributed to the assignment
    * Each team should have at least two people run the code independently before submitting, to confirm that it works

**PLEASE CONTACT ME ON SLACK** if you have questions or get puzzled by something.

Team-specific notes:
* **reddit analysis team**: please work from `Starting Point - Count Outcome`, which includes code for setting your control group potential outcomes from observed data in the CSV I provided you with
* **dark patterns team**: working from `Starting Point - Binary Outcome`, diagnose your team's research design
* **moral messaging team**: please imagine that you are testing the effect of some messaging intervention on a binary outcome. `Starting Point - Binary Outcome` will be a helpful starting point for you. If possible, try to diagnose a design where the potential outcomes differ between two groups
* **team bandit**: you have one of two options:
  * (preferred) use the code in `Lecture - Bandit Algorithms vs AB Tests.ipynb` to simulate how much regret you could minimize on average, with the number of participants and arms you are considering, using the bandit algorithm your team plans to use (or something similar)
  * use `Starting Point - Binary Outcome` in the lecture-code/ folder to estimate how many observations you would need in order to differentiate between arms in a randomized trial
