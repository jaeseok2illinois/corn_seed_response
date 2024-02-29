#/*=================================================*/
#' # Extract input information from field data
#/*=================================================*/
#--- field data ---#
field_data <- jsonlite::fromJSON(
  file.path(
    here("Data", "CommonData"),
    "field_parameter.json"
  ),
  flatten = TRUE
) %>%
data.table() %>%
.[, farm_field := paste(farm, field, sep = "_")] %>% 
.[, field_year := paste(farm_field, year, sep = "_")] 

#--- get field parameters for the field-year ---#
w_field_data <- field_data[field_year == ffy, ]

w_farm_field <- w_field_data$farm_field
w_year <- w_field_data$year

# find field location for soils #
# use harvest data point
address <- here("Data/Growers", ffy, "Raw/yield.shp") %>%
  st_read() %>%
  st_transform(., 4326) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  .[1,] %>%
  reverse_geocode(., lat = Y, long = X) %>%
  pull(address) 

soil_source <- if(str_detect(address, "Québec") == TRUE){
  "Quebec"
}else if (str_detect(address, "United States") == TRUE){
  "SSURGO"
}else{
  "No soil"
}

#--- get input data ---#
input_data <- dplyr::select(w_field_data, starts_with(
  "input")) %>%  map(1) %>% 
  rbindlist(fill = TRUE)

#--- input price data ---#
input_prices <- read.csv(here("Data/CommonData/InputPrice.csv"))

#--- crop price data ---#
crop_prices <- read.csv(here("Data/CommonData/CropPrice.csv"))

#/*----------------------------------*/
#' ## Crop information
#/*----------------------------------*/
crop <- tolower(w_field_data[, crop])
crop_unit <- w_field_data[, crop_unit] 
crop_price <- as.numeric(w_field_data[, crop_price]) 
if(is.na(crop_price) == TRUE) {
  crop_price <- crop_prices %>%
    filter(Crop == crop & Type == "current") %>%
    pull(Price)
}
land_unit <- w_field_data[, land_unit] 
reporting_unit <- w_field_data[, reporting_unit] 
harvester_width <- w_field_data[, h_width][[1]]
crop_unit <- w_field_data[, crop_unit] 

#/*----------------------------------*/
#' ## trial_info table
#/*----------------------------------*/

trial_type <- input_data %>%
  filter(strategy == "trial") %>%
  pull(form) %>%
  paste0(., collapse = "_")

trial_info <- 
  tibble(
    crop = crop, 
    input_type = input_data$form,
    form = input_data$form,
    unit = input_data$unit,
    convert = input_data$convert,
    section_num = input_data$section_num,
    process = ifelse(input_data$strategy == "trial", TRUE, FALSE),
    use_td =  input_data$use_target_rate_instead,
    machine_width = input_data$machine_width,
    trial_width = input_data$input_plot_width,
    price = input_data$price,
    gc_rate = input_data$sq_rate,
    gc_type = ifelse(is.numeric(input_data$sq_rate), "uniform", "Rx")
  ) %>% 
  filter(process == TRUE) %>%
  rowwise() %>%
  mutate(price = get_input_price(price, crop, input_type)) %>%
  mutate(base_rate = get_base_rate(input_data, input_type)) %>%
  mutate(gc_rate = get_gc_rate(gc_rate, input_type, form, unit, convert, base_rate)) 

#/*=================================================*/
#' # Dictionary
#/*=================================================*/
#/*----------------------------------*/
#' ## Variable name
#/*----------------------------------*/
dictionary <- jsonlite::fromJSON(
  file.path(
    here("Data", "CommonData"),
    "variable_name_dictionary.json"
  ),
  flatten = TRUE
) %>% 
data.table()

