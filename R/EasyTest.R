library(animint2)

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')
freedom <- freedom[, !names(freedom) %in% c("Region_Code", "Status")]
freedom$CL <- as.numeric(freedom$CL)


years <- data.frame(year=unique(freedom$year))

world_map <- map_data("world")
world_map$subregion <- NULL
world_map$region[world_map$region == "USA"] <- "United States of America"

library(plyr)
# add freedom index to each country
map_freedom <- ldply(unique(freedom$year), function(yr) {
  df <- subset(freedom, year == yr)
  merge(world_map, df, by.x = "region", by.y = "country")
})
map_freedom$country <- map_freedom$region

map_freedom <- map_freedom[order(
                                 map_freedom$year,
                                 map_freedom$order), ]

viz.scatter <- ggplot()+
  ggtitle("Freedom in the World")+
  xlab("Civil Liberties") + ylab("Political rights") +
  geom_jitter(aes(x=CL, y=PR, color=Region_Name, 
                  key=country),
              showSelected="year",
              data=freedom,
              width = 0.25, height = 0.25)+
  geom_text(aes(
    x=CL, y=PR, label=paste(country, "in", year, sep=" "),
    key=country),
    showSelected=c("year", "country", "Region_Name"),
    data=freedom)

viz.timeSeries <- ggplot()+
  ggtitle("Year vs Civil Liberties")+
  ylab("Civil Liberties") +
  geom_tallrect(aes(
    xmin=year-0.5, xmax=year+0.5),
    clickSelects="year",
    alpha=0.5,
    data=years)+
  geom_line(aes(
    x=year, y=CL, group=country, color=Region_Name),
    clickSelects=c("year","country"),
    size=3,
    alpha=0.6,
    data=freedom)+
  geom_text(aes(
    x=year, y=CL, label=country, key=country),
    showSelected=c("year", "country", "Region_Name"),
    data=freedom)


theme_opts <- list(theme(panel.grid.minor = element_blank(), 
                         panel.grid.major = element_blank(), 
                         panel.background = element_blank(), 
                         panel.border = element_blank(), 
                         plot.background = element_rect(fill = "#E6E8Ed"), 
                         axis.line = element_blank(), 
                         axis.text.x = element_blank(), 
                         axis.text.y = element_blank(), 
                         axis.ticks = element_blank(), 
                         axis.title.x = element_blank(), 
                         axis.title.y = element_blank()))

viz.worldMap <- ggplot()+
  ggtitle("Civil Liberties Index World Map")+
  geom_polygon(aes(x = long, y = lat, group = group, fill = CL),
               showSelected = "year",
               clickSelects="country",
               data=map_freedom, 
               colour="grey") + 
  scale_fill_gradient2(low = "white", high = "red", breaks = 0:7, guide = "none") + 
  theme_opts + 
  theme_animint(width = 1000, height= 500)


(viz.animint <- animint(viz.scatter,
                        viz.timeSeries,
                        viz.worldMap,
                        title="Freedom",
                        duration = list(year=2000),
                        time=list(variable="year", ms=8000)
                        ))



animint2dir(viz.animint, "easyOutput")
# animint2gist(viz.duration.time, description = "Freedom in the World'")
