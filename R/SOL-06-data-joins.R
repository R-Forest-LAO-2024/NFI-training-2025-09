
## GOALS:
## 1. Add lcs_no to tree table
## 2. Join lcs info and Chave E to tree table

## Create a list to store temporary objects
tmp <- list()

## 1. Recalc lcs_no at tree level based on tree position ####
## See PPT TIP 01-03

tree <- tree |>
  mutate(
    tree_x = cos((90 - tree_azimuth) * pi/180) * tree_distance,
    tree_y = sin((90 - tree_azimuth) * pi/180) * tree_distance,
    lcs_no = case_when(
      abs(tree_x) <= 6 & abs(tree_y) <= 6 ~ 1,
      tree_azimuth > 315 | tree_azimuth <=45  ~ 2,
      tree_azimuth >  45 & tree_azimuth <=135 ~ 3,
      tree_azimuth > 135 & tree_azimuth <=225 ~ 4,
      tree_azimuth > 225 & tree_azimuth <=315 ~ 5,
      TRUE ~ NA_integer_
    )
  )

## EX-01: make a figure of tree positions ####
## - For subplot '631C'
## - Use circ08 and circ16 to show subplot boundaries
## - Use geom_hline(yintercept = 0) and geom_vline(xintercept = 0) 
##   to make south-north and east-west lines
## - Use geom_label_repel() to show the azimuths 
## - Use coord_fixed to make sure the x and y axis have the same unit length
## - Check visually that the trees are positioned correctly based on their azimuth

## !!! SOL
tree |>
  filter(plot_no == 123, subplot_no == "B") |>
  ggplot(aes(x = tree_x, y = tree_y)) +
  geom_point() +
  geom_path(data = circ16, aes(x = x, y = y)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_label_repel(aes(label = tree_azimuth)) +
  coord_fixed()
## !!!

## 2. join tables ####

## 2.1. prepare join ####

## We don't want all the lcs data into tree (we don't want 'lcs_name')
## >> make a temporary object and use select()
tmp$lcs <- lcs |> select(-lcs_name)

## Same as:
tmp$lcs <- lcs |> select(plot_no, subplot_no, lcs_no, lc_no, lc_code)

## 2.2. join tables ####

## 2.2.1. with changing object ####

## It is recommended to change object name when using join to avoid undesired suffixes
tree_join <- tree |> left_join(tmp$lcs, by = join_by(plot_no, subplot_no, lcs_no))

## EX-02: create 'tree_err' ####
## - create tree_err by joining tree and tmp$lcs
## - re-create tree_err by joining tree_err and tmp$lcs
## >> observe in names(tree_err) that there is now a suffix .x and .y to duplicated columns

## !!! SOL
tree_err <- tree |> left_join(tmp$lcs, by = join_by(plot_no, subplot_no, lcs_no))
tree_err <- tree_err |> left_join(tmp$lcs, by = join_by(plot_no, subplot_no, lcs_no))
names(tree_err)

## 2.2.2. With keeping the same object name ####
## To keep the same object when using joins, we have to control the suffixes:
## 1. Create the columns desired from join with NAs,
## 2. join the table and add suffixes that add "_rm" to the NA columns and "" to the joined columns
## 3. remove the columns that end with "_rm"

tree <- tree |>
  mutate(lc_no = NA, lc_code = NA) |>
  left_join(tmp$lcs, by = join_by(plot_no, subplot_no, lcs_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))


## EX-03: Join chave E with tree ####
## - Use the second method (same object 'tree')
## - Check if there are NAs in column plot_E after joining

## !!! SOL
names(anci$plot_E)

tree <- tree |>
  mutate(plot_E = NA) |>
  left_join(anci$plot_E, by = join_by(plot_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))

summary(tree$plot_E)
## !!!


## EX-04: make a figure of tree positions ####
## - Remake the figure of EX-01 with these differences:
## - Change the color of trees based on lc_code
## - Remove geom_label_repel() to show the azimuths
## - Remove hline and vline
## - Add geom_abline() with intercept = 0 and slope = 1
## - Add geom_abline() with intercept = 0 and slope = -1
## - Check visually that different land cover are in different quadrants

## !!! SOL
tree |>
  filter(plot_no == 123, subplot_no == "B") |>
  ggplot(aes(x = tree_x, y = tree_y)) +
  geom_point(aes(color = lc_code)) +
  geom_path(data = circ16, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = -1) +
  coord_fixed()
## !!!

  