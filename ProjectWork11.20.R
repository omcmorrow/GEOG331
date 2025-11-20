##Plotting land cover of the colorado river basin for 2000
#Reading in nlcd land cover data for US in 2000
library(terra)
library(tidyterra)
library(FedData)
nlcd_2000 <-get_nlcd(template = FedData::meve,
                       label = "meve",
                       year = 2000,
                       extraction.dir = "Z:/GEOG331_F25/mloranty/data/")
get_nlcd_annual()