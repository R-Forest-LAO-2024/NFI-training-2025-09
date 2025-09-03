

## GOALS
## 1. Sum tree agb to subplots
## 2. Make plot majority LC class
## 3. Create plot level AGB based on mean subplot AGB
## 4. Create Forest type AGB as the mean of plot AGB per forest type
## 5. Calculate sampling error.

## 1. 
subplot_agb <- tree |>
  group_by(plot_no, subplot_no) |>
  summarise(
    sp_count_tree_meas = n(),
    sp_count_tree_ha = sum(tree_weight_spha),
    sp_agb = sum(tree_agb_final * tree_weight_spha),
    .groups = "drop"
  )

## 2. 
plot_lc <- read_csv("data/plot_lc.csv", show_col_types = F)

## 3.

