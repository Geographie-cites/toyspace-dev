
# load packages ----

library(sf)
library(tidyverse)


# load functions ----

source("toyspace_functions.R")

# load data ----

communes <- readRDS("data/pomacom.Rds") %>% 
  st_transform(crs = 2154) %>% 
  mutate(FINGERPLAN = sample(x = c(0, 1), size = 1300, replace = TRUE, prob = c(0.8, 0.2)))

tabFlows <- readRDS("data/tabflows.Rds") %>% 
  group_by(ORI, DES) %>% 
  summarise(FLOW = sum(FLOW))

# test ----

fg_flows <- finger_plan(pol = communes, 
                        id = "CODGEO", 
                        cand = "FINGERPLAN", 
                        tabflows = tabFlows,
                        idori = "ORI",
                        iddes = "DES",
                        idflow = "FLOW")




