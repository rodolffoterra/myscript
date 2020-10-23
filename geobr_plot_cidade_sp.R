install.packages("geobr")

library(geobr)

sp <- geobr::read_census_tract(
  code_tract = 3550308,
  year = 2010,
  simplified = TRUE,
  showProgress = TRUE
)


# plotar

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())



# Plot all Brazilian states
ggplot() +
  geom_sf(data=sp, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  no_axis