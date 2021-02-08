# TFT-Reroll-Calculator

Published [here](https://harvestgolem.shinyapps.io/TFT-Reroll-Calculator/).

Last updated for patch: 11.3 

A generalized calculator that computes the probability of hitting a certain # of units under any given game state in TFT. 
The following functionalities have been implemented:
- View probability distribution of hitting any combination of up to 5 different unit conditions 
- Comparing expected cost between rerolling and leveling then rerolling
- Customizing the baseline game values for analysis of hypothetical patches (e.g. unit probabilities, # copies per pool, etc)

RShiny's computational power is a bit lacking, so there may be significant delay when computing conditions for rerolling many copies of a unit or many different units.
