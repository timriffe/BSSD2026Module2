# update HMD HFD:

library(tidyverse)
library(HMDHFDplus)
availableHFD
B <- readHFDweb("ESP",item = "birthsRR", username = Sys.getenv("us"), password = Sys.getenv("pw"))
P <- readHMDweb("ESP",item = "Population", username = Sys.getenv("us"), password = Sys.getenv("pw"))
D <- readHMDweb("ESP",item = "Deaths_1x1", username = Sys.getenv("us"), password = Sys.getenv("pw"))
data.table::fwrite(B, file = "data/ES_B.csv.gz")
data.table::fwrite(P, file = "data/ES_P.csv.gz")
data.table::fwrite(D, file = "data/ES_D.csv.gz")

E <- readHMDweb("ESP",item = "Exposures_1x1", username = Sys.getenv("us"), password = Sys.getenv("pw"))

El <-
E |> 
  clean_names() |> 
  select(-open_interval) |> 
  pivot_longer(female:total, names_to = "sex", values_to = "exposure") 

Dl <- D |> 
  clean_names() |> 
  select(-open_interval) |> 
  pivot_longer(female:total, names_to = "sex", values_to = "deaths")

mort <- left_join(El,Dl, by = join_by(year, age, sex)) |> 
  mutate(mx = deaths / exposure)
data.table::fwrite(mort, file = "data/ES_mort.csv.gz")
