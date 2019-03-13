---
title: "Data Dictionary"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(eval = FALSE)
```

#Data Dictionary
team_name: character, team name

game_date: character, game date

season: numeric, season year

period: numeric, the period in the game (4 periods, 12 minutes each)

minutes_remaining: numeric, time left in a period

seconds_remaining: numeric, minutes remainingd down to the seconds level

shot_made_flag: character, "y" for make, "n" for miss

action_type: character, move used to make shot

shot_type: character, 2-pointer or 3-pointer

shot_distance: numeric, distance to basket (feet)

opponent: character, opposing team

x: numeric, x coordinate of shot (inches)

y: numeric, y coordinate of shot (inchese)