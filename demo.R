
# Load required packages ----------------------------------------------------------------------------------------------------

if (!requireNamespace("pacman")) install.packages("pacman", dependencies = TRUE)

pacman::p_load(
  tidyverse,
  palmerpenguins
)



# Scenario one: vector functions --------------------------------------------------------------------------------------------

clamp_age <- function(x) {

  ifelse(x < 0 | x > 15, NA, x)
}


ages <- c(2, 7, 19, 22, 12, 7, 9, -2, 10)

clamp_age(ages)


df_pediatrics <- tibble(
  sex = factor(sample(c("m", "f"), 10, replace = TRUE), levels = c("m","f"),labels = c("male", "female")),
  age = c(2, 7, 12, -3, 14, 22, 9, 14, 17, 8),
  weight = c(12, 22, 30, -6, 35, 40,30, 39, 74, 26)
)


df_pediatrics |>
  mutate(
    age_cleaned = clamp_age(age),
    .after = age
  )


clamp_number <- function(x, min = 0, max = 15) {

  ifelse(x < min | x > max, NA, x)
}

df_pediatrics |>
  mutate(
    age_cleaned = clamp_number(age),
    weight_cleaned = clamp_number(weight, 0, 45)
  )



# know which tidyverse function is data-masking or tidy-select --------------------------------------------------------------
?dplyr::select

?dplyr::arrange

?dplyr::filter


# Scenario two: dataframe function for a specific task ----------------------------------------------------------------------

# 1. summarise diamond cut by price
summarise_cut_price <- function(df){

  df |>
    group_by(cut) |>
    summarise(
      min = min(price, na.rm = TRUE),
      average  = mean(price, na.rm = TRUE),
      max = max(price, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_cut_price(diamonds)

# 2. summarise diamonds cut by depth
summarise_cut_depth <- function(df){

  df |>
    group_by(cut) |>
    summarise(
      min = min(depth, na.rm = TRUE),
      average  = mean(depth, na.rm = TRUE),
      max = max(depth, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_cut_depth(diamonds)

# 3. summarise diamonds color by price
summarise_color_price <- function(df){

  df |>
    group_by(color) |>
    summarise(
      min = min(price, na.rm = TRUE),
      average  = mean(price, na.rm = TRUE),
      max = max(price, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_color_price(diamonds)

# 4. summarise diamonds color by depth
summarise_color_depth <- function(df){

  df |>
    group_by(color) |>
    summarise(
      min = min(depth, na.rm = TRUE),
      average  = mean(depth, na.rm = TRUE),
      max = max(depth, na.rm = TRUE),
      .groups = "drop"
    )
}



# scenario three: reduce the 4 functions to one ---------------------------------------------------------------------------------

summarise_diamonds <- function(df, group, var) {

  df |>
    group_by({{ group }}) |>
    summarise(
      min = min({{ var }}, na.rm = TRUE),
      average = mean({{ var }}, na.rm = TRUE),
      max = max({{ var }}, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_diamonds(diamonds, group = cut, var = price)



# scenario four: multiple values to single arguments ----------------------------------------------------------------------------


# 1. tidy-select
summarise_diamonds <- function(df, group, var, choose) {

  df |>
    group_by({{ group }}) |>
    summarise(
      min = min({{ var }}, na.rm = TRUE),
      average = mean({{ var }}, na.rm = TRUE),
      max = max({{ var }}, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |>
    select({{ group }}, {{ choose }})
}


summarise_diamonds(diamonds, group = cut, var = price, choose = c(min, max))

summarise_diamonds(diamonds, group = cut, var = price, choose = n)



# 2. data-masking

summarise_diamonds(diamonds, group = c(cut, color), var = price, choose = c(min, max))


summarise_diamonds <- function(df, group, var, choose) {

  df |>
    group_by(pick({{ group }})) |>
    summarise(
      min = min({{ var }}, na.rm = TRUE),
      average = mean({{ var }}, na.rm = TRUE),
      max = max({{ var }}, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |>
    select({{ group }}, {{ choose }})
}

summarise_diamonds(diamonds, group = c(cut, color), var = price, choose = c(min, max))



# scenario five: use user-supplied variable names ---------------------------------------------------------------------------

summarise_diamonds <- function(df, group, var, nm) {

  df |>
    group_by({{ group }}) |>
    summarise(
      {{ nm }} := mean({{ var }}, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_diamonds(diamonds, group = cut, var = price, nm = mean_price)


summarise_diamonds <- function(df, group, var) {
  df |>
    group_by({{ group }}) |>
    summarise(
      {{ var }} := mean({{ var }}, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_diamonds(diamonds, group = cut, var = price)



summarise_diamonds <- function(df, group, var) {
  df |>
    group_by({{ group }}) |>
    summarise(
      "min_{{ var }}" := min({{ var }}, na.rm = TRUE),
      "average_{{ var }}" := mean({{ var }}, na.rm = TRUE),
      "max_{{ var }}" := max({{ var }}, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_diamonds(diamonds, group = cut, var = price)

summarise_diamonds(diamonds, group = color, var = table)



# scenario six: use a different dataset that has a categorical and a continuous variable ------------------------------------

penguins

## update function name
summarise_df <- function(df, group, var) {
  df |>
    group_by(pick({{ group }})) |>
    summarise(
      "min_{{ var }}" := min({{ var }}, na.rm = TRUE),
      "average_{{ var }}" := mean({{ var }}, na.rm = TRUE),
      "max_{{ var }}" := max({{ var }}, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_df(penguins, group = c(species, island, sex), var = body_mass_g)

summarise_df(diamonds, group = cut, var = price)
