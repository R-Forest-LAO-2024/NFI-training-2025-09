

## GOALS:
## 1. Prepare subplot x LCS level table
## 2. Use the aggregate function 'nfi_aggregate3()' to get all aggregation levels
## 3. Compare results with equal plot statistiscs (table 'ftype')


## 1.
anci$ph2 <- read_csv("data/training_anci_phase2.csv", show_col_types = F)

ph2_subplot <- anci$ph2
# format(nrow(anci$ph2) / 20, big.mark = ",")


## Check
vec_ph2 <- anci$ph1 |> 
  filter(!is.na(plot_id)) |>
  pull(plot_id) |>
  sort()

test_ph2 <- expand_grid(
  plot_id = vec_ph2,
  subplot_no = c("A", "B", "C", "D"),
  lcs_no = 1:5
) |>
  mutate(subplot_id = paste0(subplot_no, lcs_no))

tmp_ph2 <- anci$ph2 |> select(plot_id, subplot_no, lcs_no, subplot_id)

all.equal(tmp_ph2, test_ph2)


## Joins
tmp_sp <- subplot |> select(plot_id = plot_no, subplot_no, subplot_access)

tmp_ph1 <- anci$ph1 |>
  filter(!is.na(plot_id)) |>
  select(plot_id, subpop, stratum, prov_no = ph1_prov_no, prov_name = ph1_prov_name)

ph2_subplot <- ph2_subplot |>
  mutate(
    subplot_access = NA,
    subpop = NA, 
    stratum = NA,
    prov_no = NA,
    prov_name = NA
  ) |>
  left_join(tmp_ph1, by = join_by(plot_id), suffix = c("_rm", "")) |>
  left_join(tmp_sp, by = join_by(plot_id, subplot_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))

## Recode access and add area
ph2_subplot <- ph2_subplot |>
  mutate(
    access = case_when(
      plot_id <= 636 & subplot_access == "accessible" ~ TRUE,
      plot_id <= 636 & subplot_access != "accessible" ~ FALSE,
      plot_id <= 636 & is.na(subplot_access) ~ FALSE,
      stratum %in% 1:3 ~ FALSE,
      stratum == 4 ~ TRUE,
      TRUE ~ NA
    ),
    sp_area = if_else(lcs_no == 1, 12^2, (pi*16^2 - 12^2)/4) / 10000
  )


## Add trees


