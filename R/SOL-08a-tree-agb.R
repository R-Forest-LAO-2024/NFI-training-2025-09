
## GOALS:
## 1. add AGB at tree level for each forest type
## 2. Add AGB from chave 05 and 14 without height
## 3. Make a figure for natural forest and one for plantations
##    showing forest type AGB, and chave models

## List of models:
# "EG"    ~ 0.3112 * tree_dbh^2.2331,
# "MD"    ~ 0.523081 * tree_dbh^2,
# "DD"    ~ 0.2137 * tree_dbh^2.2575,
# "CF"    ~ 0.1277 * tree_dbh^2.3944,
# "MCB"   ~ 0.1277 * tree_dbh^2.3944,
# "P_AC"  ~ 0.1173 * tree_dbh^2.454,
# "P_EC"  ~ 0.199 * tree_dbh^2.185,
# "P_RB"  ~ 0.0082 * (pi*tree_dbh)^2.5623, ## Rubber model uses circumference
# "P_TK"  ~ 0.077 * tree_dbh^2.546,
# "P_OTH" ~ 0.3112 * tree_dbh^2.2331,
# "RV"    ~ 0.6 * exp(-1.499 + 2.148 * log(tree_dbh) + 0.207 * (log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3),
# "B"     ~ 0.6 * exp(-1.499 + 2.148 * log(tree_dbh) + 0.207 * (log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3),

# Chave14 = round(exp(-1.803 - 0.976*plot_E + 0.976*log(0.6) + 2.673*log(tree_dbh) -0.0299*(log(tree_dbh))^2)
# Chave05 = round(0.6 * exp(-1.499 + 2.148*log(tree_dbh) + 0.207*(log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3)



## 1. AGB per forest type ####

## Get the names of forest types
table(tree$lc_code)

## Implement AGB per forest type

tree <- tree |>
  mutate(
    tree_agb_final = case_when(
      lc_code == "EG" ~ 0.3112 * tree_dbh^2.2331,
      ## ADD OTHER equations here
      TRUE ~ 0
    )
  )


## EX-01: complete tree_agb_final ####
## - for all forest land covers add their equations as shown at the beginning of the script
## - For land cover with no equation, assign 0
## - make a figure with 'tree_agb_final' against 'tree_dbh' as dots
## - make a figure with 'tree_agb_final' against 'tree_dbh' as line with color based on 'lc_code'

## !!! SOL
tree <- tree |>
  mutate(
    tree_agb_final = case_when(
      lc_code == "EG"    ~ 0.3112 * tree_dbh^2.2331,
      lc_code == "MD"    ~ 0.523081 * tree_dbh^2,
      lc_code == "DD"    ~ 0.2137 * tree_dbh^2.2575,
      lc_code == "CF"    ~ 0.1277 * tree_dbh^2.3944,
      lc_code == "MCB"   ~ 0.1277 * tree_dbh^2.3944,
      lc_code == "P_AC"  ~ 0.1173 * tree_dbh^2.454,
      lc_code == "P_EC"  ~ 0.199 * tree_dbh^2.185,
      lc_code == "P_RB"  ~ 0.0082 * (pi*tree_dbh)^2.5623, ## Rubber model uses circumference
      lc_code == "P_TK"  ~ 0.077 * tree_dbh^2.546,
      lc_code == "P_OTH" ~ 0.3112 * tree_dbh^2.2331,
      lc_code == "RV"    ~ 0.6 * exp(-1.499 + 2.148 * log(tree_dbh) + 0.207 * (log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3),
      lc_code == "B"     ~ 0.6 * exp(-1.499 + 2.148 * log(tree_dbh) + 0.207 * (log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3),
      TRUE ~ 0
    )
  )

tree |>
  ggplot(aes(x = tree_dbh, y = tree_agb_final)) +
  geom_point()

tree |>
  ggplot(aes(x = tree_dbh, y = tree_agb_final)) +
  geom_line(aes(color = lc_code))
## !!!


## 2. AGB from Chave 2005 and 2014 ####

## EX-02: add equations for all trees ####
## - Add 'tree_agb_chave05' with Chave et al. 2005 equation
## - Add 'tree_agb_chave14' with Chave et al. 2014 equation
## - Add  'tree_agb_EG' with the evergreen forest model

## !!! SOL
tree <- tree |> 
  mutate(
    tree_agb_chave05 = 0.6 * exp(-1.499 + 2.148*log(tree_dbh) + 0.207*(log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3),
    tree_agb_chave14 = exp(-1.803 - 0.976*plot_E + 0.976*log(0.6) + 2.673*log(tree_dbh) -0.0299*(log(tree_dbh))^2),
    tree_agb_EG      = 0.3112 * tree_dbh^2.2331
    )
## !!!


## 3. Compare AGB models

## 3.1. Natural forest ####
tree |>
  filter(tree_agb_final != 0, lc_no <= 20) |>
  ggplot(aes(x = tree_dbh, y = tree_agb_final)) +
  geom_point() +
  geom_line(aes(y = tree_agb_chave05), color = "grey60") +
  geom_line(aes(y = tree_agb_chave14), color = "darkred") +
  geom_line(aes(y = tree_agb_EG), color = "darkgreen") +
  facet_wrap(~lc_no)


## 3.2 planted forest ####

## EX-03 compare models for plantations ####
## - Make a similar graph than 3.1 but for planted forest only

## !!! SOL
tree |>
  filter(tree_agb_final != 0, lc_no >= 160) |>
  ggplot(aes(x = tree_dbh, y = tree_agb_final)) +
  geom_point() +
  geom_line(aes(y = tree_agb_chave05), color = "grey60") +
  geom_line(aes(y = tree_agb_chave14), color = "darkred") +
  geom_line(aes(y = tree_agb_EG), color = "darkgreen") +
  facet_wrap(~lc_code)
## !!!

