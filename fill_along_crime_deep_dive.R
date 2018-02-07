# Fillalong -  Drug Crime DeepDive

library(magrittr) # %<>%
library(tidyverse) # ggplot2 2.2.1.9000

### Data in ----------------------------------------------------------------

library(geojsonio)

# get crime reports from ODP
crime_raw <- ___("https://opendata.arcgis.com/datasets/d1877e350fad45d192d233d2b2600156_7.geojson",
                           parse = ___) %>% # 2 element list
    .$features %>% # 3 column data frame, table of interest is nested in column 2
    .$properties # 8 column data frame, got it

head(crime_raw)

# check date range for fun
crime_raw$DateReported %>% as.POSIXct() %>% ___()

# build a mailing address to send to the Google GeoCoder
crime_raw %<>% mutate(address = paste(___, ___, "Charlottesville VA"))

#### Geocode ----------------------------------------------------------------

library(ggmap)

lon_lat <- ___(head(crime_raw$address))
lon_lat # failed geocodes happen, not sure why

geocode(head(crime_raw$address)) # randomly successful on repeat queries

# example bolt on at the end, so we can plot spatially
crime_coded <- ___(head(crime_raw), lon_lat)

# Read in clean data from GitHub url
crime <- ___("https://raw.githubusercontent.com/NathanCDay/cville_crime/master/crime_geocode.csv")

# not all address can be coded
___(complete.cases(crime)) / ___(crime) # 0.9999373, most can
crime %<>% filter(complete.cases(crime)) 

# re-check date range
crime$DateReported %>% ___() %>% range()
# "2012-12-04 00:00:00 EST" "2017-12-02 01:48:00 EST"

#### Plotting ----------------------------------------------------------------

# first pass for plotting
ggplot(crime, aes(___, ___)) +
    geom_point(shape = 1, size = 3)

# get census shape files from ODP
census <- ___("https://opendata.arcgis.com/datasets/e60c072dbb734454a849d21d3814cc5a_14.geojson",
                       what = "sp") # SpatialPolygonsDataFrame object
class(census)


library(sf)

census_sf %<>% ___(census) # convert to sf
class(census)

# plot it sf style
ggplot(census) +
    ___()

head(census)

names(census) %<>% tolower() # preference for easier typing

census %<>% ___(geometry, starts_with("h"), objectid:other) # keep some housing and demographic vars

#### Spatial Filter --------------------------------------------------------

# convert crime to sf too
class(crime)
crime %<>% st_as_sf(coords = ___, # chr vector to name vars with point data
                    crs = ___(census)) # use the same coordinate reference system as census
class(crime)

# plot both together
ggplot(___(crime, .1)) + # ~3,000 points take a second, 30,000 takes way too long
    geom_sf(alpha = .1) +  
    geom_sf(data = census, fill = "blue", alpha = .5)

# filter to only cases within the census polygons
crime %<>% mutate(within = ___(crime, census) %>% # returns block each point is within
                      as.numeric()) # returns NA for those outside

crime %<>% filter(!is.na(within)) # drop the outsiders

# replot for visual confirmation
ggplot(sample_frac(crime, .1)) +
    geom_sf(alpha = .1) +
    ___(data = census, fill = "green", alpha = .5)


#### Frequent Addresses -----------------------------------------------------

# build a drug crime flag
crime %<>% mutate(drug_flag = ___(grepl("drug", Offense, ignore.case = TRUE),
                                     "drugs", "not_drugs"))

# check offenses involved via %>% with(.,base_fxn()) 
filter(crime, drug_flag == "drugs") %>% with(___(Offense))

# drop DARE class
crime %<>% filter(Offense != "DRUG AWARENESS PRESENTATION")

# summarise by address

crime_counts <- crime %>%
    group_by(address, drug_flag) %>% # geometry column automatically comes with :)
    ___() %>%
    ungroup() %>% # geom_sf() doesn't like groupped dfs
    arrange(n) # so the biggest dots get plotted last

library(viridis) # bc it's pretty and to show the biggest you need viridis

ggplot(crime_counts) + # weird patten but it seems to help if you put the largest object first
    geom_sf(data = census) +
    geom_sf(aes(size = n, color = n, alpha = n)) +
    ___() +
    facet_wrap(~drug_flag)

# check the highest addresss for both flags
arrange(crime_counts, -n) %>% group_by(drug_flag) %>% ___(1:5)

# check if proportions are equal
station_props <- arrange(crime_counts, -n) %>%
    group_by(drug_flag) %>%
    ___(wt = n) %>% # shortcut for mutate(nn = n())
    slice(1)

summarise(station_props, prop_total = n / nn)

with(station_props, ___(n, nn)) # test confirms difference

crime %<>% filter(address != "600 E MARKET ST Charlottesville VA")

#### Spatial Grouping -----------------------------------------------------

crime_block <- st_set_geometry(___, NULL) %>% # need to remove sf geometry property for spread to work
    group_by(within, drug_flag) %>%
    count() %>%
    spread(drug_flag, n) %>%
    mutate(frac_drugs = drugs / sum(drugs + not_drugs)) %>% ungroup()

# join back into census
census %<>% inner_join(___, by = c("objectid" = "within"))

hist(census$frac_drugs)

ggplot(census) +
    geom_sf(aes(fill = ___)) +
    scale_fill_viridis()

### Spatial correlation ---------------------------------------------------

library(spdep) # requires sp objects

census_sp <- as(census, "Spatial") # covert sf to sp

block_nb <- poly2nb(census_sp) # build a neighbor network

___(census_sp); ___(block_nb, coordinates(census_sp), add = TRUE) # see the network with baseR

# see the var
ggplot(census) +
    ___(aes(fill = frac_drugs)) +
    scale_fill_viridis()

# test the var
moran.mc(census_sp$frac_drugs, nb2listw(block_nb), nsim = ___) # confirmed clustering

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