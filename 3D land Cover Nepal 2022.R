# 1. PACKAGES
setwd("D:\\R Programming\\3D Landcover Nepal 2022") 
libs <- c(
  "terra",
  "giscoR",
  "sf",
  "tidyverse",
  "ggtern",

  "elevatr",
  "png",
  "rayshader",
  "magick"
)

installed_libraries <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libraries == F)){
  install.packages(
      libs[!installed_libraries]
  )
}

invisible(
  lapply(
      libs, library, character.only = T
  )
)

# 2. COUNTRY BORDERS

country_sf <- read_sf("D:/R Programming/3D Landcover Nepal 2022/Nepal_Outline.shp")
plot(sf::st_geometry(country_sf))


# 3 DOWNLOAD ESRI LAND COVER TILES

# # 4 LOAD TILES

raster_file <- "E:/GIS DATA/GIS_Datasets_NP_1-250,000_ICIMOD/Landcover of Nepal FRTC & ICIMOD/lc2022.tif"
r <- rast(raster_file)
print(r)
# country_sf <- st_read("C:\\Users\\B Shorea robusta\\Downloads\\Compressed\\nepal_outline.shp")
crs(r) <- "EPSG:4326"
r <- project(r, "EPSG:4326")
country_sf <- st_transform(country_sf, crs(r))
r_cropped <- crop(r, country_sf)
r_masked <- mask(r_cropped, country_sf)
output_file <- "D:\\R Programming\\3D Landcover Nepal 2022/landcover_2022.tif"
writeRaster(r_masked, output_file, overwrite = TRUE)
print(output_file)

# 5 LOAD VIRTUAL LAYER

r_list <- list.files(
  path = getwd(),
  pattern = "_2022",
  full.names = T
)
print(r_list)
land_cover_vrt <- terra::vrt(
  r_list,
  "nepal_land_cover_vrt.vrt",
  overwrite = T
)
print(land_cover_vrt)



# # 6 FETCH ORIGINAL COLORS
# Load the raster file
ras <- terra::rast(raster_file[[1]])

# Assign specific colors to land cover types
land_cover_colors <- c(
  Waterbody = "#0000FF",
  Glacier = "#BEE8FF",
  Snow = "#F2FAFF",
  Forest = "#006400",
  Riverbed = "#FFEBAF",
  Builtuparea = "#FF0000",
  Cropland = "#FFFF00",
  Baresoil = "#D7C29E",
  Barerock = "#9C9C9C",
  Grassland = "#55FF00",
  OWL = "#00FFC5"
)

# Convert hex colors to RGB matrix
rgb_colors <- t(col2rgb(land_cover_colors))

# Create a reclassification matrix (from original values to RGB components)
rcl_r <- cbind(from = 1:11, to = rgb_colors[, 1])
rcl_g <- cbind(from = 1:11, to = rgb_colors[, 2])
rcl_b <- cbind(from = 1:11, to = rgb_colors[, 3])

# Create separate layers for R, G, B channels by reclassifying
ras_r <- terra::classify(ras, rcl = rcl_r)
ras_g <- terra::classify(ras, rcl = rcl_g)
ras_b <- terra::classify(ras, rcl = rcl_b)

# Combine R, G, B layers into a single multi-layer raster
land_cover_rgb <- c(ras_r, ras_g, ras_b)
land_cover_res <- res(land_cover_rgb)
# Plot the raster using RGB channels
terra::plotRGB(land_cover_rgb)

# 8 DIGITAL ELEVATION MODEL

elev <- elevatr::get_elev_raster(
  locations = country_sf,
  z = 7, clip = "locations"
)
elev_res <- res(elev)
crs_epsg4326 <- "+proj=longlat +datum=WGS84 +no_defs"

land_cover_nepal_resampled <- terra::resample(
  x = land_cover_rgb,
  y = terra::rast(elev),
  method = "near"
) |>
terra::project(crs_epsg4326)

terra::plotRGB(land_cover_nepal_resampled)

img_file <- "land_cover_nepal.png"

terra::writeRaster(
  land_cover_nepal_resampled,
  img_file,
  overwrite = T,
  NAflag = 255
)

img <- png::readPNG(img_file)

# 9. RENDER SCENE
#----------------

elev_lambert <- elev |>
  terra::rast() |>
  terra::project(crs_epsg4326)

elmat <- rayshader::raster_to_matrix(
  elev_lambert
)
h <- 3500
w <- 5000
# h <- nrow(elev_lambert)
# w <- ncol(elev_lambert)
cols <- c(
  Waterbody = "#0000FF",
  Glacier = "#BEE8FF",
  Snow = "#F2FAFF",
  Forest = "#006400",
  Riverbed = "#FFEBAF",
  Builtuparea = "#FF0000",
  Cropland = "#FFFF00",
  Baresoil = "#D7C29E",
  Barerock = "#9C9C9C",
  Grassland = "#55FF00",
  OWL = "#00FFC5"
)
elmat |>
  rayshader::height_shade(
      texture = colorRampPalette(
          cols
      )(256)
  ) |>
  rayshader::add_overlay(
      img,
      alphalayer = 1,
      rescale_original = T
  ) |>
  rayshader::plot_3d(
      elmat,
      zscale = 50,
      solid = F,
      shadow = T,
      background = "white",
      windowsize = c(
          w / 5, h / 5
      ),
      zoom = .5,
      phi = 85,
      theta = 0
  )

rayshader::render_camera(
  # phi = 80,
  zoom = .58,
  # theta = 0
)

# 10. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/air_museum_playground_4k.hdr"
hdri_file <- basename(u)
options(timeout = 300)
download.file(
  url = u,
  destfile = hdri_file,
  mode = "wb"
)

filename <- "3d_land_cover_nepal-dark.png"

rayshader::render_highquality(
  filename = filename,
  preview = T,
  light = T,
  environment_light = hdri_file,
  intensity_env = 0.9,
  rotate_env = 180,
  interactive = F,
  parallel = T,
  width = w,
  height = h
)

# 11. PUT EVERYTHING TOGETHER
# Updated color codes

cols<- c(
  "#0000FF", "#BEE8FF", "#F2FAFF",
  "#006400", "#FFEBAF", "#FF0000",
  "#FFFF00", "#D7C29E", "#9C9C9C",
  "#55FF00", "#00FFC5"
)
legend_name <- "land_cover_legend.png"
png(legend_name)
par(family = "mono")

plot(
  NULL,
  xaxt = "n",
  yaxt = "n",
  bty = "n",
  ylab = "",
  xlab = "",
  xlim = 0:1,
  ylim = 0:1,
  xaxs = "i",
  yaxs = "i"
)
legend(
  "center",
  legend = c(
    "Waterbody",
    "Glacier",
    "Snow",
    "Forest",
    "Riverbed",
    "Built-up area",
    "Cropland",
    "Bare soil",
    "Bare rock",
    "Grassland",
    "OWL"
  ),
  pch = 15,
  cex = 2,
  pt.cex = 2,
  bty = "n",
  col = cols,
  fill = cols,
  border = "black"
)
dev.off()

# Load the magick package
library(magick)

# Read the initial 3D map image
map1 <- magick::image_read("3d_land_cover_nepal-dark.png")

# Set text color
title_color <- "#226600"
text_color <- "grey20"

# Add title to the image
map2 <- magick::image_annotate(
  map1, "3D Land Cover Map of Nepal 2022",
  font = "Georgia",
  color = "black",
  size = 150, gravity = "northwest",
  location = "+1500+400"
)

# Add author caption
map3 <- magick::image_annotate(
  map2, "©2025 B Shorea robusta (https://bishalrayamajhi.com.np)",
  font = "Georgia",
  color = "black",
  size = 60, gravity = "southeast",
  location = "+100+400"
)

# Add data source caption
map4 <- magick::image_annotate(
  map3, "Data : FRTC. (2022). Land cover of Nepal",
  font = "Georgia",
  color = "black",
  size = 50, gravity = "southeast",
  location = "+500+350"
)

# Resize the image to increase resolution
map4_high_res <- magick::image_resize(map4, geometry = "6000x4000")

# Read the legend image
my_legend <- magick::image_read("land_cover_legend.png")

# Scale the legend
my_legend_scaled <- magick::image_scale(
  magick::image_background(my_legend, "none"), "2500"
)

# Composite the legend onto the resized image
final_map <- magick::image_composite(
  magick::image_scale(map4_high_res, "x7000"),
  my_legend_scaled,
  gravity = "southwest",
  offset = "+60+500"
)

# Export the final high-resolution image with annotations and legend
magick::image_write(final_map, path = "3d_nepal_land_cover_final1.png", format = "png")
