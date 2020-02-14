if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(choroplethr)){install.packages("choroplethr")}
if(!require(bea.R)){install.packages("bea.R")}

# Download data from BEA
bea_api_key <- "96E71B5B-C630-46C3-A2B7-C922788EE10D" # Register for your own API key here: https://apps.bea.gov/api/signup/
userSpecList <- list("UserID" = bea_api_key, "Method" = "GetData", "datasetname" = "Regional", "TableName" = "CAGDP9","LineCode" = "1", "IndustryId" = "1","GeoFIPS" = "COUNTY", "Year" = "ALL")

BEA_county_RGDP = beaGet(userSpecList, asWide = F)  # this downloads annual real gdp data from the "regional" dataset

BEA_county_RGDP = BEA_county_RGDP[str_detect(BEA_county_RGDP$GeoName,"AK", negate = T),] # I remove this because I had trouble updating the AK and HI colors in Choroplethr. You can skip these lines if you'd like to see what happened in these states.
BEA_county_RGDP = BEA_county_RGDP[str_detect(BEA_county_RGDP$GeoName,"HI", negate = T),]

df = BEA_county_RGDP %>%  # Prepares the data for mapping.
  rename(region = GeoFips,
         value = DataValue,
         year = TimePeriod) %>%
  select(year, region, value) %>%
  mutate(region = as.numeric(region)) %>%
  arrange(region,year) %>%
  group_by(region) %>%
  mutate(value = 100*(value/lag(value)-1)) %>%
  filter(!is.na(value))
symmetric_cuts = function(x, levels = 4) { # This function creates interesting legend cutoffs for the entire data range (2002-2018).
  lt0 = x[x < 0]
  gt0 = x[x > 0]
  lt0_breaks = quantile(lt0, probs = (0:levels) / levels)
  lt0_breaks[levels + 1] = 0
  gt0_breaks = quantile(gt0, probs = (1:levels) / levels)
  all_breaks = unique(c(lt0_breaks, gt0_breaks))
  x = cut(x, breaks = all_breaks)
  return(x)
}
df$value = symmetric_cuts(df$value)

years = sort(unique(df$year))
figs = list()
for(i in seq_along(years)){ 
  temp = df %>% 
    filter(year == years[i])
  figs[[i]] = county_choropleth(temp) +
    ggtitle(paste0("County GDP Growth ", years[i])) + 
    scale_fill_brewer(palette = "RdYlBu",
      na.value="grey")+
    theme(plot.title = element_text(size = 40, face = "bold"))
  ggsave(paste0("cgdp_",years[i],".png"), plot = figs[[i]]) # This saves the images to the working directory.
}

# While you can make an animation using imagemagick from within R, I find it easier to upload the images to: https://ezgif.com/maker .