# BaseballMarkov
Analysis of baseball states through Markov chains for Retrosheet Event Files.

Downloading Retrosheet Event Files
===========================
Refer to this repository: https://gist.github.com/bayesball/8892981

data
===============
EventFields.csv --> Names of event file columns and their descriptions

StateRuns.csv --> Runs scored for each of the 576 transient transitions (transitions that dont start or end in 3 outs).

TransitionMatrix.R
=====================
Contains functions that produce state transition matricies for a specific season for the MLB, a specific team, or a list of players. Transition matricies are returned in two forms: 1. Transition probabilities for events in which the batter advances or is out. 2. Transition probabilities for non batter events such as SB, WP, PO, etc.

Lineup.R
================
Creates a list of transition matricies for a lineup of 9 players. Players can be from different years or all the same year.

GameSimulation.R
================
Functions to simulate a game (and full season) given a lineup of 9 players and their transition matricies. Returns runs scored in each game.

Example.R
===============
Some examples of how to use the functions and files.
