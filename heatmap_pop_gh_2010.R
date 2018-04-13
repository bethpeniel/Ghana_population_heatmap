library(tidyverse)
library(ggmap)
library(mapdata)
library(maps)
library(raster)
library(rgeos)
library(maptools)


########
# visualising regional population distribution in Ghana from wikipedia data
#https://en.wikipedia.org/wiki/List_of_Ghanaian_regions_by_population
########


#load webpage

gh_pop_reg <- read_html('https://en.wikipedia.org/wiki/List_of_Ghanaian_regions_by_population')

gh_pop_reg

#scrape table data
df.gh_pop_reg <- gh_pop_reg %>%
	html_nodes('table') %>% 
	.[[1]] %>%
	html_table()

df.gh_pop_reg
head(df.gh_pop_reg,10)
names(df.gh_pop_reg)
dim(df.gh_pop_reg)

#write data to csv to see colums clearly
#write.csv(df.gh_pop_reg, file='gh_pop_reg.csv')

#rename columns
colnames(df.gh_pop_reg) <- c('Rank',
							'id', # use id in order to correspond with map data
							'Population_2010',
							'Population_2000',
							'Population_1984',
							'Population_1970',
							'Population_1960',
							'Percentage_2010'	
							)
## remove All Regions row
df.gh_pop_reg <- df.gh_pop_reg %>% filter(id != 'All Regions')

# inspect data
df.gh_pop_reg %>% glimpse()
head(df.gh_pop_reg)

#keep on id and Population_2010 columns
#df.gh_pop_reg1 <- df.gh_pop_reg[c('id', 'Population_2010')]
#head(df.gh_pop_reg1)

##get map data

##or direct from GADM, level 1 has regions
gh.shp <- getData("GADM", country = "GHA", level = 1)
class(gh.shp)

names(gh.shp)
gh.shp$NAME_1

## rename Brong-Ahafo as Brong Ahafo to match map names
df.gh_pop_reg[6,2] = "Brong Ahafo" 

## fortify shape file to dataframe format
gh.shp.df <- fortify(gh.shp, region = 'NAME_1')
class(gh.shp.df)
head(gh.shp.df)

## merge shape file with data/population
gh.shp.pop <- merge(gh.shp.df, df.gh_pop_reg, by = "id")
head(gh.shp.pop)
tail(gh.shp.pop)
names(gh.shp.pop)
dim(gh.shp.pop)

## rename id column as Region
#via base R
colnames(gh.shp.pop)[colnames(gh.shp.pop) == 'id'] <- 'Region'

# vai dplyr
rename(gh.shp.pop, id = Region) 

##read 2016 population data
pop16 <- read.csv("gh_pop_2016.csv", header = T)
head(pop16)

##rename column 
colnames(pop16)[colnames(pop16) == 'Population'] <- 'Population_2016'
colnames(pop16)[colnames(pop16) == 'id'] <- 'Region'

## add Population_2016 to gh.shp.pop
gh.shp.pop16 <- merge(gh.shp.pop, pop16, by = "Region")
head(gh.shp.pop16)

## basic plot
ggplot() + geom_polygon(data = gh.shp.pop16, aes(x = long, y = lat, group = group, 
    fill = Region), color = "green", size = 0.25) + coord_map()+
ggtitle('Regional Distribution of Population in 2010 - Ghana')+
labs(x = 'Latitude', y = 'Longitude') +
annotate("text", x = -2.2, y = 10.5, label = "Upper West",size=3)+
annotate("text", x = -1, y = 10.7, label = "Upper East",size=3)+
annotate("text", x = -1, y = 9.3, label = "Northern",size=3)+
annotate("text", x = -1.5, y = 7.8, label = "Brong-Ahafo",size=3)+
annotate("text", x = -1.5, y = 6.8, label = "Ashanti",size=3)+
annotate("text", x = -.5, y = 6.3, label = "Eastern",size=3)+
annotate("text", x = -1, y = 5.5, label = "Central",size=3)+
annotate("text", x = -2.2, y = 5.5, label = "Western",size=3)+
annotate("text", x = 0, y = 5.7, label = "Greater Accra",size=3)+
annotate("text", x = .3, y = 8, label = "Volta",size=3)+
annotate("text", x = 0, y = 5, label = "Total Population: 24,658,823", 
size=3, col = 'black')
#last_plot()
ggsave('pop_map-region.png')

options(scipen=9999)

## basic plot
ggplot() + geom_polygon(data = gh.shp.pop16, aes(x = long, y = lat, group = group, 
    fill = Rank), color = "black", size = 0.25) + coord_map()+
ggtitle('Regional Distribution of Population in 2016 - Ghana')+
labs(x = 'Latitude', y = 'Longitude') +
annotate("text", x = -2.2, y = 10.5, label = "Upper West",size=3, col = 'red')+
annotate("text", x = -1, y = 10.7, label = "Upper East",size=3, col = 'red')+
annotate("text", x = -1, y = 9.3, label = "Northern",size=3, col = 'red')+
annotate("text", x = -1.5, y = 7.8, label = "Brong-Ahafo",size=3, col = 'red')+
annotate("text", x = -1.5, y = 6.8, label = "Ashanti",size=3, col = 'red')+
annotate("text", x = -.5, y = 6.3, label = "Eastern",size=3, col = 'red')+
annotate("text", x = -1, y = 5.5, label = "Central",size=3, col = 'red')+
annotate("text", x = -2.2, y = 5.5, label = "Western",size=3, col = 'red')+
annotate("text", x = 0, y = 5.7, label = "Greater Accra",size=3, col = 'red')+
annotate("text", x = .3, y = 8, label = "Volta",size=3, col = 'red')+
annotate("text", x = 0, y = 5, label = "Total Population: 28,308,301", 
size=3, col = 'black')
#last_plot()
ggsave('pop_map0.png')

## annotate graph

annotate("text", x = -.375, y=-.125, label = "Region 4: Infinite support",size=5)+
theme(legend.position = "none")

#g <- get_map('Ghana', zoom=7)
#ggmap(g)

# Total_2016 = 28,308,301
# Total_ 2010 = 24,658,823






