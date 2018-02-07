# Walkthrough -  Drug Crime DeepDive

library(magrittr) # %<>%
library(tidyverse) # ggplot2 2.2.1.9000

#### Geocode ----------------------------------------------------------------

library(geojsonio)

# get raw crime reports from ODP
crime_raw <- geojson_read("https://opendata.arcgis.com/datasets/d1877e350fad45d192d233d2b2600156_7.geojson",
                           parse = TRUE) %>% # 2 element list
    .$features %>% # 3 column data frame, table of interest is nested in column 2
    .$properties # 8 column data frame, got it

head(crime_raw)

# check date range
crime_raw$DateReported %>% as.POSIXct() %>% range()
# "2013-02-05 EST" "2017-12-11 EST"

# build a mailing address to send to the Google GeoCoder
crime_raw %<>% mutate(address = paste(BlockNumber, StreetName, "Charlottesville VA"))

library(ggmap)

lon_lat <- geocode(head(crime_raw$address)) # failed geocodes happen, most are successful on repeat queries
lon_lat
crime_coded <- bind_cols(head(crime_raw), lon_lat) # bolt on coordinates so now we could plot

# Read in data thats been all clean up
crime <- read_csv("https://raw.githubusercontent.com/NathanCDay/cville_crime/master/crime_geocode.csv")

# not all address can be coded
sum(complete.cases(crime)) / nrow(crime) # 0.9999373, most can
crime %<>% filter(complete.cases(crime)) 

# re-check date range
crime$DateReported %>% as.POSIXct() %>% range()
# "2012-12-04 00:00:00 EST" "2017-12-02 01:48:00 EST"

#### Plotting ----------------------------------------------------------------

# first pass for plotting
ggplot(crime, aes(lon, lat)) +
    geom_point(shape = 1, size = 3)

# get census shape files from ODP
census <- geojson_read("https://opendata.arcgis.com/datasets/e60c072dbb734454a849d21d3814cc5a_14.geojson",
                       what = "sp") # SpatialPolygonsDataFrame object
class(census)

# convert to sf object
library(sf)

census %<>% st_as_sf()
class(census)

# plot it
ggplot(census) +
    geom_sf()

head(census)
names(census) %<>% tolower()
census %<>% select(geometry, starts_with("h"), objectid:other) # keep some additional variables to go with the shaeps

#### Spatial Filter --------------------------------------------------------

# convert crime to sf too
class(crime)
crime %<>% st_as_sf(coords = c("lon", "lat"), # specify vars with lon/lat info
                    crs = st_crs(census)) # use the same coordinate reference system as census
class(crime)

# plot both together
ggplot(sample_frac(crime, .1)) +
    geom_sf(alpha = .1) +  # ~3,000 points take a second, 30,000 takes too long
    geom_sf(data = census, fill = "blue", alpha = .5) ->

# filter to only cases within the census polygons
crime %<>% mutate(within = st_within(crime, census) %>% # returns the objectid for the block it falls within
                      as.numeric()) # returns NA for those outside

crime %<>% filter(!is.na(within)) # drop the outsiders

# replot for visual confirmation
ggplot(sample_frac(crime, .1)) +
    geom_sf(alpha = .1) +
    geom_sf(data = census, fill = "green", alpha = .5)


#### Frequent Addresses -----------------------------------------------------

# build a drug crime flag
crime %<>% mutate(drug_flag = ifelse(grepl("drug", Offense, ignore.case = TRUE),
                                     "drugs", "not_drugs"))

# check offenses involved
filter(crime, drug_flag == "drugs") %>% with(table(Offense))

# drop DARE class
crime %<>% filter(Offense != "DRUG AWARENESS PRESENTATION")

# summarise by address

# for the presentation
# saveRDS(census, "data/census_sf.RDS")
# saveRDS(crime, "data/crime_sf.RDS")

crime_counts <- crime %>%
    group_by(address, drug_flag) %>% # geometry column automatically comes with :)
    count() %>%
    ungroup() %>% # geom_sf() doesn't like groupped dfs
    arrange(n) # so the biggest dots get plotted last

saveRDS(crime_counts, "data/crime_counts.RDS")

library(viridis) # bc it's pretty

ggplot(crime_counts) +
    geom_sf(data = census) +
    geom_sf(aes(size = n, color = n, alpha = n)) +
    scale_color_viridis() +
    facet_wrap(~drug_flag)

# for presentation
ggplot(crime_counts) +
    geom_sf(data = census) +
    geom_sf(aes(size = n, color = n, alpha = n)) +
    scale_color_viridis() +
    facet_wrap(~drug_flag) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = "none")

saveRDS(crime_counts, "data/crime_counts_sf.RDS")

# check the highest addresss for both flags


# check if proportions are equal
station_props <- arrange(crime_counts, -n) %>%
    group_by(drug_flag) %>%
    add_count(wt = n) %>%
    slice(1)

summarise(station_props, prop_total = n / nn)


with(station_props, prop.test(n, nn)) # confirms difference

crime %<>% filter(address != "600 E MARKET ST Charlottesville VA")

#### Group Summaries -----------------------------------------------------

crime_block <- st_set_geometry(crime, NULL) %>% # need to remove sf geometry property for spread to work
    group_by(within, drug_flag) %>%
    count() %>%
    spread(drug_flag, n) %>%
    # rowwise() %>%
    mutate(frac_drugs = drugs / sum(drugs + not_drugs)) %>% ungroup()

# join back into census
census %<>% inner_join(crime_block, by = c("objectid" = "within"))

hist(census$frac_drugs)

ggplot(census) +
    geom_sf(aes(fill = frac_drugs)) +
    scale_fill_viridis()

### Spatial correlation ---------------------------------------------------

library(spdep) # requires sp objects

census_sp <- as(census, "Spatial") # covert sf to sp

block_nb <- poly2nb(census_sp) # build a neighbor network

plot(census_sp); plot(block_nb, coordinates(census_sp), add = TRUE) # see the network

# see the var
ggplot(census) +
    geom_sf(aes(fill = frac_drugs)) +
    scale_fill_viridis()

# test the var
moran.mc(census_sp$frac_drugs, nb2listw(block_nb), nsim = 999) # confirmed clustering

# repeat for population
ggplot(census) +
    geom_sf(aes(fill = population)) +
    scale_fill_viridis()

moran.mc(census_sp$population, nb2listw(block_nb), nsim = 999) # not
# because the county land prevents all of the UVA centric blocks from touching are not conne

# GLMs ------

library(tidycensus)

# get age and income variables to join with census
cvl <- get_acs(geography = "block group", county = "Charlottesville", state = "VA",
               variables = c("B19013_001", "B01002_001") )

# clean up variabl names
decode <- c("income", "age") %>% set_names(c("B19013_001", "B01002_001"))
cvl$variable %<>% decode[.]

# format the data for joining
cvl %<>% select(GEOID, variable, estimate) %>%
    spread(variable, estimate)

# viz check
ggplot(cvl, aes(age, income)) +
    geom_point() # missing values
# let's impute them as average of neighbors once joined to census

cvl %<>% rename(blockgroup = GEOID)
census %<>% full_join(cvl)

# sequester the missing values value
miss <- census %>% filter(is.na(income))

# calculate the mean its neightbors
miss$income <- st_touches(miss, census) %>% # return the row_ids for adjacent polygons
    map_dbl(~ census[., ] %>% with(mean(income))) # calculate the means per missing block

# builder decoder
dc <- miss$income %>% set_names(miss$objectid) 
    
# back together again
census$income %<>% ifelse(is.na(.), dc[as.character(census$OBJECTID)], .)

# bc drug laws are racist
census %<>% mutate(frac_black = black / pop)

# pred column positions for ggpairs()
pred_cols <- match(c("frac_drugs", "frac_vacant", "age", "income", "frac_black"), names(census))

ggpairs(census, columns = pred_cols)

# model
mod <- glm(frac_drugs ~  frac_black + income, data = census, family = quasibinomial())
summary(mod)
resid(mod) %>% hist()