library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)


# Create a boundary box
longitudes <- c(-76.6516, -77.8683)
latitudes <- c(27.1007, 25.7232)
bounding_box <- matrix(c(longitudes, latitudes), nrow = 2, byrow = TRUE,
                       dimnames = list(NULL, c("min", "max")))

# Create a boundary box
longitudes <- c(-76.65, -77.86)
latitudes <- c(27.10, 25.72)
bounding <- as.data.frame(cbind(longitudes, latitudes))
bounding_box <- matrix(c(longitudes, latitudes), nrow = 2, byrow = TRUE, dimnames = list(NULL, c("min", "max")))
bounding.obj <-  SpatialPointsDataFrame(bounding_box, bounding, proj4string = CRS("+init=epsg:4326" ))

## Creating a spatial Point data frame with coordinates
abaco <- SpatialPointsDataFrame(data[ ,c("C_103_lon","C_102_lat",)], ## Coord
                                as.data.frame(this.indic.matrix.norm22), ## Composite with different algo
                                proj4string = CRS("+init=epsg:4326"), ## Definte project syst
                                bbox = bbox(bounding_box)) # Add bounding box larger than data to get sufficient padding around the map

plot(abaco)

# download osm tiles
abaco.osm <- getTiles(
  x = abaco,
  type = "hotstyle", # "osm", "hotstyle", "hikebike", "osmgrayscale", "stamenbw", "stamenwatercolor", "cartodark", "cartolight".
  zoom = 9,
  crop = TRUE
)

writeRaster(abaco.osm,
            filename = 'abaco_osm.grd',
            format = "GTiff",
            overwrite = TRUE,
            options = c("INTERLEAVE=BAND","COMPRESS=LZW"))

if (file.exists("abaco_osm.rds")) {

  abaco_osm <- readRDS("abaco_osm.rds")
} else {

  saveRDS(abaco.osm, "abaco_osm.rds")
}

tilesLayer(x = abaco.osm)
abaco_osm <- readRDS("abaco_osm.rds")
tilesLayer(x = abaco_osm)

## Data and map tiles sources:
## © OpenStreetMap contributors. Tiles style under CC BY-SA, www.openstreetmap.org/copyright.
# plot osm tiles
tilesLayer(x = abaco.osm)
# plot municipalities (only borders are plotted)
plot(st_geometry(mtq), col = NA, border = "grey", add=TRUE)
# plot population
propSymbolsLayer(
  x = mtq,
  var = "POP",
  inches = 0.4,
  col = "brown4",
  legend.pos = "topright",
  legend.title.txt = "Total population"
)
# layout
layoutLayer(title = "Population Distribution in Martinique",
            sources = "Sources: Insee and IGN, 2018\n© OpenStreetMap contributors.\nTiles style under CC BY-SA, www.openstreetmap.org/copyright.",
            author = paste0("cartography ", packageVersion("cartography")),
            frame = FALSE, north = FALSE, tabtitle = TRUE)
# north arrow
north(pos = "topleft")


##########

plot(st_geometry(mtq), col="darkseagreen3", border="darkseagreen4",
     bg = "lightblue1", lwd = 0.5)
# Plot symbols with choropleth coloration
propSymbolsChoroLayer(
  x = mtq,
  var = "POP",
  inches = 0.4,
  border = "grey50",
  lwd = 1,
  legend.var.pos = "topright",
  legend.var.title.txt = "Population",
  var2 = "MED",
  method = "equal",
  nclass = 4,
  col = carto.pal(pal1 = "sand.pal", n1 = 4),
  legend.var2.values.rnd = -2,
  legend.var2.pos = "left",
  legend.var2.title.txt = "Median\nIncome\n(in euros)"
)
# layout
layoutLayer(title="Population & Wealth in Martinique, 2015",
            author = "cartography 2.1.3",
            sources = "Sources: Insee and IGN, 2018",
            scale = 5, tabtitle = TRUE, frame = FALSE)
# north arrow
north(pos = "topleft")



abaco

h <- 1
this.var <- as.character(All.method2[ h, c("var")])
this.label <- as.character(All.method2[ h, c("label")])
# get Map Background
tilesLayer(x = abaco.osm)
#Plot symbols with choropleth coloration
propSymbolsChoroLayer(
  spdf = abaco,
  var = "D_102_inds",
  inches = 0.15, ## size of the biggest symbol (radius for circles, width for squares, height for bars) in inches.
  border = "grey50",
  lwd = 1, #width of symbols borders.
  legend.var.pos = "topright",
  legend.var.title.txt = "Population Size\n(# of Individual)",
  #legendCirclesSymbols(var = c(10,100), inches = 0.3),
  var2 = this.var ,
  #classification method; one of "sd", "equal", "quantile", "fisher-jenks","q6", "geom", "arith", "em" or "msd"
  method = "quantile",
  nclass = 5,
  col = carto.pal(pal1 = "sand.pal", n1 = 6),
  #legend.var2.values.rnd = -2,
  legend.var2.pos = "left",
  legend.var2.title.txt = "Index Value\n (scale 0 to 100)",
  legend.var.style = "e"
)
# plot labels
labelLayer(
  spdf = abaco,
  txt = "C_101_name",
  col = "black",
  # numeric character expansion factor;
  #multiplied by par("cex") yields the final character size.
  cex = 0.9,
  font = 4,
  halo = TRUE,
  bg = "white",
  r = 0.05,
  overlap = FALSE,
  show.lines = FALSE
)
# Layout
layoutLayer(title = paste0("Protection Index - based on ",this.label ),
            author = "Protection Working Group, Abaco - The Bahamas, November 2019",
            sources = "Source: Key Informant Interview - HDX/DTM/IOM",
            tabtitle = TRUE,
            frame = FALSE ,
            bg = "#aad3df",
            scale = "auto")
# North arrow
north(pos = "topleft")

##str(abaco)

#############################3
## load abaco mask
library(rgdal)

abacomask <- readOGR( "content/post/test/abaco.geojson")

abacopoly <- dismo::voronoi(data[ ,c("C_103_lon","C_102_lat")],  eps = 1e-09)

# Using the union of points instead of the original sf object works:
voronoi <- st_voronoi(st_union(st_as_sf(abaco)))
plot(voronoi, col = 0)

# Clid boundary instead of the original bounding box.
# clip to smaller box

abacopoly <- st_intersection(st_union(st_as_sf(abacomask)),
                             st_cast(voronoi)
                             )

abacopoly2 <- as(abacopoly, "Spatial")

# Extract polygon ID's
pid <- sapply(slot(abacopoly2, "polygons"), function(x) slot(x, "ID"))

# Create dataframe with correct rownames
p.df <- data.frame( ID=1:length(abacopoly2), row.names = pid)

# Try coersion again and check class
p <- SpatialPolygonsDataFrame(abacopoly2, p.df)
class(p)
plot(p)

rownames(abaco@data) <- NULL
abaco@data$ID <- rownames(abaco@data)
abacodata <- as.data.frame(abaco@data)
rownames(abacodata) <- paste0("ID",abaco$ID)
abacopoly3 <- SpatialPolygonsDataFrame(abacopoly2, abacodata)

# ## Return this to a spatial object
# types <- vapply(sf::st_geometry(abacopoly), function(x) {
#   class(x)[2]
# }, "")
#
# unique(types)
# polys <- abacopoly[ grepl("*POLYGON", types), ]
# spPolys <- as(polys, "Spatial")
#
# abacopoly <- sf::st_join(st_union(st_as_sf(abacomask)),
#                          st_cast(voronoi),
#                          join = st_intersects)
#
# abacopoly2 <- SpatialPolygonsDataFrame(abacopoly)

plot(abacopoly2)





# get Map Background
tilesLayer(x = abaco.osm)
# plot isopleth map
smoothLayer(
  spdf = abacopoly3,
  var = this.var ,
  typefct = "pareto", # "pareto" - "exponential"
  span = 4000,
  beta = 2,
  nclass = 12,
  col = carto.pal(pal1 = 'brown.pal', n1 = 12),
  border = "grey",
  lwd = 0.1,
  mask = abacopoly,
  legend.values.rnd = -3,
  legend.title.txt = "Severity\nIndex",
  legend.pos = "topright",
  add = TRUE
)
# annotation on the map
text(x = 692582, y = 1611478, cex = 0.8, adj = 0, font = 3,  labels =
       "Distance function:\n- type = exponential\n- beta = 2\n- span = 4 km")
# Layout
layoutLayer(title = paste0("Protection Index - based on ",this.label ),
            author = "Protection Working Group, Abaco - The Bahamas, November 2019",
            sources = "Source: Key Informant Interview - HDX/DTM/IOM",
            tabtitle = TRUE,
            frame = FALSE ,
            bg = "#aad3df",
            scale = "auto")
# North arrow
north(pos = "topleft")

vignette(topic = "StewartExample")
vignette(topic = "SpatialPosition")
