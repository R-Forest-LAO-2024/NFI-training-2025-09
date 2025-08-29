

## GOALS:
## 1. Load initial tables
## 2. Correct errors in subplot table (duplicates in subplot_id)
## 3. Add lcs_no to tree table
## 4. Join subplot and lcs info to tree table
## 5. add ancillary info (optional)


##
## 1. Load initial harmonized tables ####
##

## 1.1. Load subplot, lcs, tree tables ####
subplot_init <- read_csv("data/training_subplot.csv", show_col_types = FALSE)


## EX: Load tree and lcs tables as tree_init and lcs_init ####
## !!! SOL
lcs_init <- read_csv("data/training_lcs.csv", show_col_types = FALSE)
tree_init <- read_csv("data/training_tree.csv", show_col_types = FALSE)
## !!!


## 1.2. Load Chave E and Phase 1 ancillary data ####
anci <- list()
anci$ph1    <- read_csv("data/training_anci_phase1.csv", show_col_types = FALSE)
anci$plot_E <- read_csv("data/training_anci_plotE.csv", show_col_types = FALSE)


## 1.3 Visualize ####
circ16 <- tibble(
  theta = seq(0, 2*pi, length = 100),
  x = 16 * cos(theta),
  y = 16 * sin(theta)
)

tree_init |>
  filter(plot_no == 631, subplot_no == "C") |>
  ggplot(aes(x = tree_x, y = tree_y)) +
  geom_point() +
  geom_path(data = circ16, aes(x = x, y = y)) +
  coord_fixed()

## EX: make a tree location figure ####
## - for subplot '1A' -> filter()
## - change color for trees with DBH <= 30cm and trees with DBH > 30cm -> mutate(), if_else()
## - create table circ08 with the data to add the 8m radius circle to the figure
## 

## !!! SOL
circ08 <- tibble(
  theta = seq(0, 2*pi, length = 100),
  x = 8 * cos(theta),
  y = 8 * sin(theta)
)

tree_init |> 
  filter(plot_no == 1, subplot_no == "A") |>
  mutate(tree_dbh_cat = if_else(tree_dbh <= 30, "small", "big")) |>
  ggplot(aes(x = tree_x, y = tree_y)) +
  geom_point(aes(color = tree_dbh_cat)) +
  geom_path(data = circ08, aes(x = x, y = y)) +
  geom_path(data = circ16, aes(x = x, y = y)) +
  coord_fixed()
## !!!



## 
## 2. Clean subplot ####
## 

## Identify duplicated in subplot_id (ex. 631A)
vec_dup <- subplot_init |>
  group_by(subplot_id) |>
  summarise(count = n(), .groups = "drop") |>
  filter(count > 1) |>
  pull(subplot_id)
vec_dup

## EX: Identify duplicates of subplot_plot_no
## !!! SOL
vec_dup2 <- subplot_init |>
  group_by(subplot_plot_no) |>
  summarise(count = n(), .groups = "drop") |>
  filter(count > 4) |>
  pull(subplot_plot_no)
vec_dup2
## !!!


## How to correct
## Check the duplicates:
tt <- subplot_init |> filter(subplot_plot_no == 631)
#View(tt)

subplot <- subplot_init |>
  mutate(
    subplot_no = case_when(
      subplot_id == "631C" & ONA_index == 109 ~ "B",
      ## ADD more corrections,
      TRUE ~ subplot_no
    ),
    subplot_id = case_when(
      subplot_plot_no < 10 ~ paste0("00", plot_no, subplot_no),
      subplot_plot_no < 100 ~ paste0("0", plot_no, subplot_no),
      TRUE ~ paste0(plot_no, subplot_no)
    )
  )
