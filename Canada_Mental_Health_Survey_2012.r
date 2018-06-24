# Databricks notebook source
# DATA PREPARATION STARTS HERE

# COMMAND ----------

## Load relevant libraries for data cleaning, manipulation and visualization

# COMMAND ----------

library(RColorBrewer) # useful when making maps and other visualizations

# COMMAND ----------

install.packages("ggmap")

# COMMAND ----------

library(ggmap) # load GIS mapping libary from google, includes geocoding functions

# COMMAND ----------

install.packages("tidyverse")

# COMMAND ----------

library(tidyverse) # load tidy data libraries for cleaning and visualization data

# COMMAND ----------

library(data.table) # load high performance libraries for reading and manipulating data

# COMMAND ----------

library(stringr) # Load package to mainupulate text easier

# COMMAND ----------

sessionInfo() # document technical details for the environment where R is running

# COMMAND ----------

## Load dataset for data cleaning

# COMMAND ----------

df <- fread("https://raw.githubusercontent.com/tarjin/Canada_Open_Data/master/Mental_Health_CCHS_01051101-eng.csv")

# COMMAND ----------

glimpse(df)

# COMMAND ----------

options(digits=15) # set the visible number of digits to 15, useful when displaying GIS coordinates in dataframes

# COMMAND ----------

options("digits")

# COMMAND ----------

str(df)

# COMMAND ----------

df$GEO <- factor(df$GEO)
df$AGE <- factor(df$AGE)
df$SEX <- factor(df$SEX)
df$HRPROF <- factor(df$HRPROF)
df$UNIT <- factor(df$UNIT)

# COMMAND ----------

df %>%
  summarise(na_count = sum(!is.na(Value))) # This is the number of rows with non-missing (real) values for the column 'Value'

# COMMAND ----------

df <- df %>% 
  filter(!is.na(Value))  # filter out the rows with non-missing values

# COMMAND ----------

df <- df %>%
  filter(Value > 0)

# COMMAND ----------

str(df)

# COMMAND ----------

df$Value <- as.numeric(df$Value)

# COMMAND ----------

str(df)

# COMMAND ----------

# Add lat and lon gps coordinates for geos
# First create columns and set values to 0

# COMMAND ----------

df$Lat <- 0
df$Lon <- 0

# COMMAND ----------

str(df)

# COMMAND ----------

levels(factor(df$GEO))

# COMMAND ----------

df$Lat[df$GEO == 
                          "Newfoundland and Labrador"] <- 53.1355091
df$Lon[df$GEO == 
                          "Newfoundland and Labrador"] <- -57.66043639999998

# COMMAND ----------

df$Lat[df$GEO == 
                          "Nova Scotia"] <- 44.68198659999999
df$Lon[df$GEO == 
                          "Nova Scotia"] <- -63.74431100000004

# COMMAND ----------

df$Lat[df$GEO == 
                          "New Brunswick"] <- 46.5653163
df$Lon[df$GEO == 
                          "New Brunswick"] <- -66.4619164

# COMMAND ----------

df$Lat[df$GEO == 
                          "Prince Edward Island"] <- 46.510712
df$Lon[df$GEO == 
                          "Prince Edward Island"] <- -63.416813599999955

# COMMAND ----------

df$Lat[df$GEO == 
                          "Quebec"] <- 52.9399159
df$Lon[df$GEO == 
                          "Quebec"] <- -73.5491361

# COMMAND ----------

df$Lat[df$GEO == 
                          "Ontario"] <- 51.253775
df$Lon[df$GEO == 
                          "Ontario"] <- -85.32321400000001

# COMMAND ----------

df$Lat[df$GEO == 
                          "Manitoba"] <- 53.7608608
df$Lon[df$GEO == 
                          "Manitoba"] <- -98.81387619999998

# COMMAND ----------

df$Lat[df$GEO == 
                          "Saskatchewan"] <- 52.9399159
df$Lon[df$GEO == 
                          "Saskatchewan"] <- -106.4508639

# COMMAND ----------

df$Lat[df$GEO == 
                          "Alberta"] <- 53.9332706
df$Lon[df$GEO == 
                          "Alberta"] <- -116.5765035

# COMMAND ----------

df$Lat[df$GEO == 
                          "British Columbia"] <- 53.7266683
df$Lon[df$GEO == 
                          "British Columbia"] <- -127.64762050000002

# COMMAND ----------

## Finalize and Save Data Frame to csv

# COMMAND ----------

# write.csv(df, file = "NS_Crime_Stats.csv")

# COMMAND ----------

# EXPLORATORY DATA ANALYSIS STARTS HERE

# COMMAND ----------

# GIS Exploratory Visualizations

# COMMAND ----------

## Using ggmap library with Google Maps

# COMMAND ----------

CANADAMAP <-get_map(location=c(-89,63),zoom=3, source="google", maptype="terrain", color="bw")

# COMMAND ----------

ggmap(CANADAMAP) #To see the map you grabbed use the command ggmap followed by the name of the map.

# COMMAND ----------

levels(factor(df$HRPROF))

# COMMAND ----------

levels(factor(df$UNIT))

# COMMAND ----------

levels(factor(df$AGE))

# COMMAND ----------

df_filtered <- df %>%
  filter(!Lat == 0) %>%
  filter(SEX == "Both sexes") %>%
  filter(AGE == "25 to 64 years") %>%
  filter(HRPROF == "Self-rated work stress, days quite a bit or extremely stressful") %>%
  filter(UNIT == "Percent") %>%
  filter(Ref_Date == 2012)

# COMMAND ----------

ggmap(CANADAMAP) + #To see the map you grabbed use the command ggmap followed by the name of the map.

geom_point(data = df_filtered, 
           aes(x = Lon, y = Lat, color = Value, size = Value)) +
scale_colour_gradient(low = "white",  high = "red") +
theme(legend.position = "bottom") +
labs(size ="Percent population", colour="Percent population") +
ggtitle("Percent population in Canada, 25 to 64 years\nwho were highly stressed, 2012") + 
guides(size = FALSE) + # remove size legend
theme(plot.title = element_text(hjust=0.5)) # center plot title with hjust


# COMMAND ----------

glimpse(df_filtered)

# COMMAND ----------

df_filtered

# COMMAND ----------

df_filtered %>%
  arrange(desc(Value))

# COMMAND ----------

head(df_filtered)
