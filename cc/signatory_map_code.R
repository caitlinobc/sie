
# color palettes

funs = brewer.pal(9, 'RdYlBu')
blues = brewer.pal(9, 'Blues')
dists = c(funs, blues)

# 21st and not the 21st
pal21 = c('#addd8e', '#f0f0f0')

# ----------------------------------------------
# MAKE SOME MAPS
# ----------------------------------------------

# ------------------------
# read in the March 2022 congressional district shape file
# source: https://gis.ny.gov/gisdata/inventories/details.cfm?DSID=1360
dist_map = st_read(paste0(dir, '/nys_cong_dist/NYS-Congressional-Districts.shp'))

# convert to standard lat/long coordinates
dist_map = st_transform(dist_map, "+proj=longlat +ellps=WGS84 +datum=WGS84")
coord = data.table(st_coordinates(dist_map))
coord[ , L2:=NULL] # drop this value - not sure what it represents
setnames(coord, c('long', 'lat', 'state', 'district'))

# add a 21st district binary
coord[district==21, tfirst:='21st District']
coord[district!=21, tfirst:='Other Congressional District']
coord[ , tfirst:=as.factor(tfirst)]

# ------------------------
# Locations of signatiories by March 2022 Congressional District



ggplot(coord, aes(x=long, y=lat, group = district, fill=tfirst)) + 
  geom_polygon() + 
  geom_path(size=0.01) +
  scale_fill_manual(values = pal21, na.value = '#f0f0f0') +
  geom_point(data = dt, aes(x = lon, y = lat, group = volunteer), 
       size = 2, colour = '#feb24c', alpha= 0.2, inherit.aes = F) +
  theme_void()




# merge in the names of the counties to match the data 
names = data.table(cbind(county = c(dist_map@data$DISTRICT), id =  c(0, seq(1:26))))
dist_coord = merge(dist_coord, names, by = 'id')



# ------------------------

# ------------------------

nys = get_googlemap(center = c(lon = -74.2179, lat = 43.2994), zoom = 6, 
                    maptype = "roadmap")

# ------------------------



# ------------------------
# read in the county shape file
# source: https://gis.ny.gov/gisdata/inventories/details.cfm?DSID=927
county_map = shapefile(paste0(dir, '/nys_civil_counties/Counties.shp'))
plot(county_map)
coord = data.table(fortify(county_map))

# merge in the names of the counties to match the data 
names = data.table(cbind(county = c(county_map@data$NAME), id =  c(0, seq(1:61))))
coord = merge(coord, names, by = 'id')

# ------------------------





ggplot(dist_coord, aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  geom_point(data = dt, 
             aes(x = lon, y = lat, group = volunteer), 
             size = 3, colour = '#feb24c', alpha= 0.9)+
  theme_void(base_size =16) +
  theme( text=element_text(size=18))



dt[ , lat2:=lat*100000]





ggplot(coord, aes(x=long2, y=lat2, group=group, fill=1)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  geom_point(data = dt, 
             aes(x = lon, y = lat, group = volunteer), 
             size = 3, colour = '#feb24c', alpha= 0.9)+
  theme_void(base_size =16) +
  theme( text=element_text(size=18))

ggplot(dt, aes(x=lon, y=lat)) + 
  geom_point()


ggplot(codes, aes(x = lon, y =lat))+
  geom_point()


write.csv(dt, paste0(dir, 'test_coords.csv'))


















