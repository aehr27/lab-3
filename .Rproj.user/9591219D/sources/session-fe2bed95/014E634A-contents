
library(spdep)
library(tidyverse)
library(sf)
library(tmap)
library(ggplot2)



#T.1 + T.2 - Subset Shapefile/Variable

#counties <- st_read("./data/ACS_2020_5YR_COUNTY.gdb", layer = "ACS_2020_5YR_COUNTY")
#edu <- st_read("./data/ACS_2020_5YR_COUNTY.gdb", layer = "X15_EDUCATIONAL_ATTAINMENT")

#yellowstone_edu <- counties %>%
#  left_join(edu %>% 
#              mutate(GEOID = substr(GEOID, 8, 20)) %>%
#              select(GEOID, B15003e1, B15003e22:B15003e25),
#            by = "GEOID") %>%
#  filter(STATEFP %in% c("30", "56", "16", "46")) %>%  
#  mutate(
#    pct_bachelors = (B15003e22 + B15003e23 + B15003e24 + B15003e25)/B15003e1*100,
#    state_abbr = case_when(
#      STATEFP == "30" ~ "MT",
#      STATEFP == "56" ~ "WY", 
#      STATEFP == "16" ~ "ID",
#      STATEFP == "46" ~ "SD"
#    )
#  ) %>%
#  select(GEOID, NAMELSAD, STATEFP, pct_bachelors, state_abbr, Shape)

#st_write(yellowstone_edu, "yellowstone_edu_complete.gpkg")

yellowstone_edu <- st_read("./data/yellowstone_edu_complete.gpkg")

glimpse(yellowstone_edu)

#T.3 - Histogram

ggplot(yellowstone_edu, aes(x = pct_bachelors, fill = state_abbr)) +
  geom_histogram(binwidth = 5, alpha = 0.7) +
  scale_fill_manual(values = c("MT" = "darkgreen", "WY" = "yellow", 
                               "ID" = "orange", "SD" = "darkblue")) +
  labs(title = "Bachelor's Degree Attainment by County",
       x = "% with Bachelor's Degree",
       y = "Number of Counties",
       fill = "State") +
  theme_minimal()

#T.4 - Choropleth Map

tm_shape(yellowstone_edu) +
  tm_polygons(
    fill = "pct_bachelors",
    fill.scale = tm_scale_intervals(
      values = "brewer.blues",  
      n = 5
    ),
    fill.legend = tm_legend(title = "% with\nBachelor's")
  ) +
  tm_title("Educational Attainment in Yellowstone Region")

#T.5 - 

county_sp <- as(yellowstone_edu, "Spatial")

nb <- poly2nb(county_sp, queen = TRUE)
lw <- nb2listw(nb, style = "W")

neighbor_counts <- card(nb)
hist(neighbor_counts, 
     main = "Distribution of Neighboring Counties",
     xlab = "Number of Neighbors",
     ylab = "Frequency",
     col = "lightblue",
     border = "white")

cat("Average number of neighbors:", mean(neighbor_counts), "\n")

moran.plot(yellowstone_edu$pct_bachelors, lw,
           main = "Moran's I Scatterplot",
           xlab = "% with Bachelor's Degree",
           ylab = "Spatially Lagged %",
           pch = 16,
           col = "darkblue")

#T.6 - IDW Method

lw_idw <- nb2listwdist(nb, 
                       x = county_sp, 
                       type = "idw", 
                       style = "W",  
                       alpha = 1,    
                       zero.policy = TRUE)

neighbor_counts <- card(nb)
hist(neighbor_counts, 
     main = "Distribution of Neighboring Counties (IDW)",
     xlab = "Number of Neighbors",
     ylab = "Frequency",
     col = "lightblue",
     border = "white")

cat("Average number of neighbors:", mean(neighbor_counts), "\n")

moran.plot(yellowstone_edu$pct_bachelors, lw_idw,
           main = "Moran's I Scatterplot (IDW Weights)",
           xlab = "% with Bachelor's Degree",
           ylab = "Spatially Lagged %",
           pch = 16,
           col = "darkblue")
