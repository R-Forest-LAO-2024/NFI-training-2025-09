
## GOALS:
## 1. calculate tree basal area


tree <- tree |> mutate(tree_ba = pi * (tree_dbh/200)^2)


## EX-01: Basal area figure ####
## - group tree data by plot and subplot_no and summarise subplot_ba as the sum 
##   of tree_ba * tree_weight_spha 
## - join lc_class_center from the subplot table
## - make a boxplot of subplot_ba against lc_class_center

## !!! SOL
tmp_sp <- subplot |> select(plot_no, subplot_no, subplot_lc_class_center)
tree |>
  group_by(plot_no, subplot_no) |>
  summarise(subplot_ba = sum(tree_ba * tree_weight_spha), .groups = "drop") |>
  left_join(tmp_sp, by = join_by(plot_no, subplot_no)) |>
  ggplot(aes(x = subplot_lc_class_center, y = subplot_ba)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1, shape = 21) ## bonus to see the observations on top of the boxplot distribution
## !!!


