# this script was set up just to demonstrate differences between R script files and R markdown files. It just has selected pasted chunks from 01_session.Rmd.

library(tidyverse)
# install.packages("tidyverse")
B <- read_csv("https://raw.githubusercontent.com/timriffe/BSSD2026Module2/refs/heads/master/data/ES_B2014.csv")
D <- read_csv("https://raw.githubusercontent.com/timriffe/BSSD2026Module2/refs/heads/master/data/ES_D2014.csv")
P <- read_csv("https://raw.githubusercontent.com/timriffe/BSSD2026Module2/refs/heads/master/data/ES_P2014.csv")


# reshape population to long format...
P_long <- 
  P |> 
  select(-open_interval) |> 
  pivot_longer(c(female, male, total),
               names_to = "sex",
               values_to = "pop")

# calculate exposures:
E <- 
  P_long |> 
  arrange(sex, age, year) |> 
  # group_by(sex, age) |> 
  mutate(pop2 = lead(pop),
         .by = c(sex, age)) |>  
  filter(!is.na(pop2)) |> 
  mutate(exposure = (pop + pop2) / 2) 



