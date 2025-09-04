


subplot_agb <- tree |>
  group_by(plot_no, subplot_no) |>
  summarise(
    sp_count_tree_meas = n(),
    sp_count_tree_ha = sum(tree_weight_spha),
    sp_agb = sum(tree_agb_final * tree_weight_spha),
    .groups = "drop"
  )

## 2. 
table(subplot$subplot_lc_class_center, useNA = "ifany")

tmp_plot_lc <- subplot |>
  group_by(plot_no, subplot_lc_class_center) |>
  summarise(count_splc = n(), .groups = "drop")


## Get unique LC plots 
tmp_plot_ulc <- tmp_plot_lc |>
  group_by(plot_no) |>
  summarise(count_plotlc = n(), .groups = "drop") |>
  filter(count_plotlc == 1) |>
  pull(plot_no)

tmp_plot_unique <- tmp_plot_lc |> 
  filter(plot_no %in% tmp_plot_ulc)

## Solve first duplicates with majority LC
tmp_plot_maxlc <- tmp_plot_lc |>
  filter(!plot_no %in% tmp_plot_ulc) |>
  group_by(plot_no) |>
  summarise(max_lc = max(count_splc))

tmp_plot_solve <-  tmp_plot_lc |>
  filter(!plot_no %in% tmp_plot_ulc) |>
  left_join(tmp_plot_maxlc, by = join_by(plot_no)) |>
  filter(count_splc == max_lc)

tmp_plot_ulc2 <- tmp_plot_solve |>
  group_by(plot_no) |>
  summarise(count_plotlc = n(), .groups = "drop") |>
  filter(count_plotlc == 1) |>
  pull(plot_no)

tmp_plot_ulc_final <- tmp_plot_solve |>
  filter(count_splc == max_lc, plot_no %in% tmp_plot_ulc2) |>
  select(-max_lc)

## Solve remaining duplicates with min LC class
tmp_plot_minlcc <- tmp_plot_lc |>
  filter(!plot_no %in% tmp_plot_ulc, !plot_no %in% tmp_plot_ulc2, !is.na(subplot_lc_class_center)) |>
  group_by(plot_no) |>
  summarise(min_lcc = min(subplot_lc_class_center))

tmp_plot_solve2 <- tmp_plot_lc |>
  filter(!plot_no %in% tmp_plot_ulc, !plot_no %in% tmp_plot_ulc2, !is.na(subplot_lc_class_center)) |>
  left_join(tmp_plot_minlcc, by = join_by(plot_no)) |>
  filter(subplot_lc_class_center == min_lcc) |>
  select(-min_lcc)

plot_lc <- tmp_plot_unique |>
  bind_rows(tmp_plot_ulc_final) |>
  bind_rows(tmp_plot_solve2) |>
  select(plot_no, plot_lc = subplot_lc_class_center, count_splc)

length(unique(subplot$plot_no))

## Check dup ####
plot_lc |> 
  group_by(plot_no) |>
  summarise(count = n()) |>
  filter(count > 1)

write_csv(plot_lc, "data/training_plot_lc.csv")
