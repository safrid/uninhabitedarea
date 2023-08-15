###############################################################################
#                     Mapping Uninhabited Area of Indonesia
#         The data used are GHSL (GLOBAL HUMAN SETTLEMENT LAYER) 30 Arsec
#                     Source code based on Milos Agathon's repo 
# link to repo : https://github.com/milos-agathon/map-uninhabited-areas/tree/main
###############################################################################

#Install library used : 
libs <- c(
  "tidyverse", "terra", "giscoR"
)

#install only uninstalled libraries
installed_libs <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libs == FALSE)){
  install.packages(
    libs[!installed_libs]
  )
}

invisible(
  lapply(
    libs, library, character.only = TRUE
  )
)

# 1. DOWNLOAD GHSL DATA
# data used are GHSL Population 30 arsec
# --------------------------------------

url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.zip"

file_name <- "GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.zip"

download.file(
  url = url,
  path = getwd(),
  destfile = file_name
)

# 2. LOAD GHSL DATA
# ------------------

unzip(file_name)
raster_name <- gsub(
  ".zip", ".tif",
  file_name
)

pop <- terra::rast(raster_name)

# 3. INDONESIA SHAPEFILE
# Data downloaded with giscoR
# ---------------------------

get_country_borders <- function(){
  country <- giscoR::gisco_get_countries(
    country = "ID",
    resolution = "3"
  )
  return(country)
}

country <- get_country_borders()

# 4. CROP GHSL DATA USING INDONESIA SHP
#--------------------------------------

indonesia_pop <- terra::crop(
  pop,
  terra::vect(country),
  snap = "in",
  mask = TRUE
)

# 5. CONVERT RASTER TO DATA FRAME
# -------------------------------

indonesia_pop_df <- as.data.frame(
  indonesia_pop,
  xy = TRUE, na.rm = TRUE
)

head(indonesia_pop_df)

names(indonesia_pop_df)[3] <- "Val"
indonesia_pop_df <- indonesia_pop_df |>
  dplyr::mutate(
    cat = dplyr::if_else(
      Val > 0, "Yes", "No"
    )
  )

# 6. TIME TO PUT IT IN MAP
# ------------------------

cols <- c("#252B48", "#F7E987")

p <- ggplot() +
  geom_raster(
    data = indonesia_pop_df,
    aes(x = x,
        y = y,
        fill = cat
        )
  ) + 
  scale_fill_manual(
    name = "Are There any People?",
    values = cols,
    na.value = "#252B48"
  ) + 
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(10, "mm"),
      keywidth = unit(25, "mm"),
      label.position = "bottom",
      label.hjust = .5,
      nrow = 1,
      byrow = TRUE
    )
  ) + 
  theme_void() +
  theme(
    legend.position = "top",
    legend.title = element_text(
      size = 16, color = "grey10"
    ),
    plot.caption = element_text(
      size = 10, color = "grey10",
      hjust = .25, vjust = 20
    ),
    plot.margin = unit(
      c(
        t = 0, b= -1,
        l = -1, r = -1
      ), "lines"
    )
  )  + 
  labs(
    title = "",
    caption = "Data : Global Human Settlement Layer at 30 arsec"
  )
print(p)
