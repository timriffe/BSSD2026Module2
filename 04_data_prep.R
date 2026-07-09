library(tidyverse)
library(readr)
library(janitor)
source("https://raw.githubusercontent.com/timriffe/BSSD2026Module2/master/02_lifetables.R")
B <- read_csv("https://raw.githubusercontent.com/timriffe/BSSD2026Module2/master/data/ES_B2014.csv", show_col_types = FALSE) |> 
  mutate(sex = "total") |> 
  select(age, sex, births = total) 

D<- read_csv("https://raw.githubusercontent.com/timriffe/BSSD2026Module2/master/data/ES_D2014.csv", show_col_types = FALSE) |> 
  filter(year == 2014) |> 
  select(-open_interval) |> 
  pivot_longer(female:total, 
               names_to = "sex", 
               values_to = "deaths")

P <- read_csv("https://raw.githubusercontent.com/timriffe/BSSD2026Module2/master/data/ES_P.csv.gz", show_col_types = FALSE) |> 
  clean_names() |> 
  select(-open_interval) |> 
  filter(year== 2014) |> 
  pivot_longer(female1:total2, 
               names_to = "sex", 
               values_to = "pop") |> 
  mutate(period = parse_number(sex),
         sex = gsub('[[:digit:]]+', '', sex)) |> 
  pivot_wider(names_from = period, values_from = pop, names_prefix="pop")

ES2014 <- 
  left_join(P, D, by = join_by(year,sex, age)) |> 
  left_join(B, by = join_by(sex, age))

LT <- 
  ES2014 |> 
  mutate(exposure = (pop1 + pop2) / 2,
         mx = deaths / exposure,
         mx = if_else(mx == 0,1,mx)) |> 
  select(sex,age,mx) |> 
  group_by(sex) |> 
  group_modify(~lt_full(.x, .y, radix = 1)) |> 
  ungroup()

