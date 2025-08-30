
## GOAL
## 1. tree weight for ratio estimators
## 2. tree weight for subplot per ha values

## In the context of ratio estimator tree weight converts small size trees 
## to the are of the bigger sized trees

## 1. tree weight for ratio estimator ####

tree <- tree |>
  mutate(
    tree_weight = if_else(tree_dbh < 30, 4, 1)
  )

## EX-01: where 4 comes from ####
## - Calculate 'Asmall' as the area of a circle of 8m radius
## - Calculate 'Abig' as the area of a circle of 16m radius
## - Calculate Abig / Asmall

## !!! SOL
Asmall <-  pi * 8^2
Abig <- pi * 16^2

Abig / Asmall
## !!!


## 2. tree weight for subplot per ha values ####

tree <- tree |>
  mutate(
    tree_spha = if_else(tree_dbh < 30, pi * 16^2 / 10000, pi * 16^2 / 10000),
    tree_weight_spha = 1 / tree_spha
  )


## EX-02: find the error ####
## - copy/paste the previous code snippet
## - find and correct the error in 'tree_spha'

## !!! SOL
tree <- tree |>
  mutate(
    tree_spha = if_else(tree_dbh < 30, pi * 16^2 / 10000, pi * 16^2 / 10000),
    tree_weight_spha = 1 / tree_spha
  )
## !!!
