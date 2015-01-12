# BaseballMarkov
Analysis of baseball states through Markov chains for Retrosheet Event Files.

Downloading Retrosheet Event Files
===========================
Refer to this repository: https://gist.github.com/bayesball/8892981

TransitionMatrix.R
=====================
Produces a state transition matrix for a specific season for the MLB or a specific team.

EventFields.csv
================
Names of event file columns and their descriptions

StateRuns.csv
================
Runs scored for each of the 576 transient transitions (transitions that dont start or end in 3 outs).
