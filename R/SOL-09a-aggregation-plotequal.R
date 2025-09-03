

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
    sp_ba = sum(tree_ba * tree_weight_spha),
    sp_agb = sum(tree_agb_final * tree_weight_spha / 1000),
    .groups = "drop"
  )

## 2. 
plot_lc <- read_csv("data/training_plot_lc.csv", show_col_types = F)

## 3.
plot <- subplot_agb |>
  group_by(plot_no) |>
  summarise(
    plot_count_tree_meas = sum(sp_count_tree_meas),
    plot_count_tree_ha   = mean(sp_count_tree_ha),
    plot_ba = mean(sp_ba),
    plot_agb = mean(sp_agb)
  ) |> left_join(plot_lc, by = join_by(plot_no))

plot |>
  ggplot(aes(x = plot_lc, y = plot_agb)) +
  geom_boxplot()

## 4. 
ftype <- plot |>
  mutate(plot_lc_num = as.numeric(plot_lc)) |>
  filter(!is.na(plot_lc), plot_lc_num < 30) |>
  group_by(plot_lc) |>
  summarise(
    plot_count = n(),
    tree_meas = sum(plot_count_tree_meas),
    tree_dens = mean(plot_count_tree_ha),
    ba = mean(plot_ba),
    agb = mean(plot_agb),
    agb_sd = sd(plot_agb)
  ) |>
  mutate(
    agb_se = agb_sd / sqrt(plot_count),
    agb_me = agb_se * qt(0.975, plot_count-1),
    agb_U  = agb_me / agb * 100,
    agb_cilower = agb - agb_me,
    agb_ciupper = agb + agb_me
  )


ftype |>
  ggplot(aes(x = plot_lc, y = agb)) +
  geom_col(aes(fill = plot_lc), col = "black") +
  geom_errorbar(aes(ymin = agb_cilower, ymax = agb_ciupper)) +
  theme(legend.position = "none")

ftype
  







