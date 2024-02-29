# /*=================================================*/
#' # Vertical reduce (reduce in the direction the machine moves)
# /*=================================================*/
# data_sf <- aa_input_grouped
# nobs_per_group <- num_obs_rg
# var_interest <- "input_rate"
# by_var <- "new_group"
reduce_points_v <- function(data_sf, nobs_per_group, var_interest, by_var = NA) {
  
  if (!is.na(by_var)) {
    data_dt <- data_sf %>%
      cbind(., st_coordinates(.)) %>%
      data.table() %>%
      setnames(var_interest, "var_i") %>%
      setnames(by_var, "group_var") %>%
      .[, dummy := 1] %>%
      .[, id_in_group := (cumsum(dummy) - 1) %/% nobs_per_group + 1, by = group_var] %>%
      #--- aggregate, but not using points that are flagged "bad" ---#
      .[, .(
        X = mean(X),
        Y = mean(Y),
        angle = mean(angle),
        width = mean(width),
        var_i = mean(var_i),
        # min_distance = min(distance),
        # max_distance = max(distance),
        # speed = mean(speed),
        flag_bad = mean(flag_bad)
      ),
      by = .(id_in_group, group_var)
      ] %>%
      .[, point_id := 1:nrow(.)] %>%
      .[, n_group := .N, group_var] %>%
      .[n_group > 1, ] %>%
      .[, n_group := NULL] %>%
      setnames("group_var", by_var) %>%
      setnames("var_i", var_interest)
  } else {
    data_dt <- data_sf %>%
      cbind(., st_coordinates(.)) %>%
      data.table() %>%
      setnames(var_interest, "var_i") %>%
      .[, dummy := 1] %>%
      .[, id_in_group := (cumsum(dummy) - 1) %/% nobs_per_group + 1] %>%
      .[flag_bad == 0, .(
        X = mean(X),
        Y = mean(Y),
        angle = mean(angle),
        width = mean(width),
        var_i = mean(var_i),
        # speed = mean(speed)
      ),
      by = .(id_in_group)
      ] %>%
      .[, point_id := 1:nrow(.)] %>%
      setnames("var_i", var_interest)
  }

  return(data_dt)
}

# /*=================================================*/
#' # Point grouping
# /*=================================================*/
# angle_vec <- group_dt_y_1$angle

get_angle_break <- function(angle_vec) {

  # ggplot(angle_data) +
  #   geom_point(aes(x = angle_id, y = angle))

  angle_data <- data.table(
    angle_id = 1:100,
    angle = quantile(angle_vec, prob = seq(0, 1, length = 100))
  ) %>%
    .[, d_angle := c(0, diff(angle))] %>%
    .[, med_d_angle := median(d_angle)] %>%
    .[, angle_change := d_angle > 3] %>%
    .[, angle_change_lag_1 := data.table::shift(angle_change, type = "lag", fill = FALSE)] %>%
    .[, d_angle_change := angle_change - angle_change_lag_1] %>%
    .[, angle_group := cumsum(abs(d_angle_change)) + 1] %>%
    .[, angle_change_lead_1 := data.table::shift(angle_change, type = "lead", fill = FALSE)] %>%
    .[angle_change == TRUE & angle_change_lead_1 == FALSE, angle_group := angle_group + 1]

  angle_min_max <- angle_data[, .(min_angle = min(angle), max_angle = max(angle)), by = angle_group] %>%
    setorder(min_angle)

  angle_breaks <- c(
    angle_min_max[, min(min_angle)],
    angle_min_max[1:(nrow(angle_min_max) - 1), max_angle + 0.1],
    angle_min_max[, max(max_angle)]
  )

  return(angle_breaks)
}

# data_sf <- asp
# by_var <- "sectionid"

drop_group_points_sc <- function(data_sf, by_var = NA) {
  if (!is.na(by_var)) {
    setup_dt <- data_sf %>%
      cbind(., st_coordinates(.)) %>%
      data.table() %>%
      .[, original_order_id := 1:nrow(.)] %>%
      setnames(by_var, "group_var")
  } else {
    by_var <- "group_var"
    setup_dt <- data_sf %>%
      cbind(., st_coordinates(.)) %>%
      data.table() %>%
      .[, original_order_id := 1:nrow(.)] %>%
      .[, group_var := 1]
  }


  angle_dt <- setup_dt %>%
    setorder(group_var, original_order_id) %>%
    .[, d_X := c(0, diff(X)), by = group_var] %>%
    .[, d_Y := c(0, diff(Y)), by = group_var] %>%
    .[, distance := sqrt(d_X^2 + d_Y^2)] %>%
    .[, angle := acos(abs(d_X) / distance) / pi * 180]

  # /*----------------------------------*/
  #' ## strong when going north-south
  # /*----------------------------------*/
  # update angle based on the direction of Y

  group_dt_y_1 <- copy(angle_dt) %>%
    .[d_Y < 0, angle := angle + 180] %>%
    .[, angle := data.table::shift(angle, type = "lead"), by = group_var] %>%
    .[, d_angle := c(NA, diff(angle)), by = group_var] %>%
    .[, med_d_angle := median(abs(d_angle), na.rm = TRUE) * 5, by = group_var] %>%
    #--- flag if angle is too sharp ---#
    .[, drop_too_sharp_turn := FALSE] %>%
    .[abs(d_angle) > med_d_angle * 10, drop_too_sharp_turn := TRUE]

  #--- create angle groups ---#
  angle_breaks_y <- get_angle_break(group_dt_y_1[!is.na(angle), angle])

  group_dt_y <- group_dt_y_1 %>%
    .[, angle_group := cut(angle, angle_breaks_y)] %>%
    .[!is.na(angle_group), ] %>%
    .[, dif_angle_group := c(1, diff(angle_group)), by = group_var] %>%
    .[, distance_gap := distance > (5 * median(distance)), ] %>%
    .[, group := cumsum(dif_angle_group != 0 | distance_gap == TRUE) + 1, by = group_var] %>%
    setnames("group_var", by_var)

  # /*----------------------------------*/
  #' ## strong when going east-west
  # /*----------------------------------*/
  # update angle based on the direction of X
  group_dt_x_1 <- copy(angle_dt) %>%
    .[d_Y < 0, angle := angle + 180] %>%
    .[, angle := data.table::shift(angle, type = "lead"), by = group_var] %>%
    .[, d_angle := c(NA, diff(angle)), by = group_var] %>%
    .[, med_d_angle := median(abs(d_angle), na.rm = TRUE) * 5, by = group_var] %>%
    #--- flag if angle is too sharp ---#
    .[, drop_too_sharp_turn := FALSE] %>%
    .[abs(d_angle) > med_d_angle * 10, drop_too_sharp_turn := TRUE]

  #--- create angle groups ---#
  angle_breaks_x <- get_angle_break(group_dt_x_1[!is.na(angle), angle])

  group_dt_x <- group_dt_x_1 %>%
    .[, angle_group := cut(angle, angle_breaks_x)] %>%
    .[!is.na(angle_group), ] %>%
    .[, dif_angle_group := c(1, diff(angle_group)), by = group_var] %>%
    .[, distance_gap := distance > (5 * median(distance)), ] %>%
    .[, group := cumsum(dif_angle_group != 0 | distance_gap == TRUE) + 1, by = group_var] %>%
    setnames("group_var", by_var)

  #--- pick the better one (smaller number of groups) ---#
  if (sum(group_dt_x$group, na.rm = TRUE) > sum(group_dt_y$group, na.rm = TRUE)) {
    group_dt <- group_dt_y
  } else {
    group_dt <- group_dt_x
  }

  group_dt[, `:=`(
    d_X = NULL,
    d_Y = NULL,
    d_angle = NULL,
    dif_angle_group = NULL,
    original_order_id = NULL
  )]

  if (all(group_dt$group_var == 1)) {
    group_dt[, group_var := NULL]
  }
  return(st_as_sf(group_dt))
}


# data_sf <- yield

group_points_sc <- function(data_sf, by_var = NA, angle_threshold) {

  if (!is.na(by_var)) {
    setup_dt <- data_sf %>%
      cbind(., st_coordinates(.)) %>%
      data.table() %>%
      .[, original_order_id := 1:nrow(.)] %>%
      setnames(by_var, "group_var")
  } else {
    by_var <- "group_var"
    setup_dt <- data_sf %>%
      cbind(., st_coordinates(.)) %>%
      data.table() %>%
      .[, original_order_id := 1:nrow(.)] %>%
      .[, group_var := 1]
  }

  # plot(1:39127, angle_dt[!is.na(angle), angle])

  group_dt <- setup_dt %>%
    setorder(group_var, original_order_id) %>%
    .[, d_X := c(0, diff(X)), by = group_var] %>%
    .[, d_Y := c(0, diff(Y)), by = group_var] %>%
    .[, distance := sqrt(d_X^2 + d_Y^2)] %>%
    #--- if distance is 0, then it means the consecutive points are duplicates ---#
    .[distance != 0, ] %>%
    .[, d_X2 := data.table::shift(d_X, type = "lag", fill = NA), by = group_var] %>%
    .[, d_Y2 := data.table::shift(d_Y, type = "lag", fill = NA), by = group_var] %>%
    .[, distance2 := data.table::shift(distance, type = "lag", fill = NA), by = group_var] %>%
    .[, vec_ip_d := (d_X * d_X2 + d_Y * d_Y2) / (distance * distance2)] %>%
    #--- get the angle of three consecutive points ---#
    .[, angle := acos(vec_ip_d) / pi * 180] %>%
    .[0.99 < vec_ip_d, angle := 0] %>%
    #--- 15 is the magic number (may not work) ---#
    .[, change_group := angle >= angle_threshold] %>%
    .[is.na(change_group), change_group := TRUE] %>%
    .[1, change_group := TRUE] %>%
    .[, group := cumsum(change_group), by = group_var] %>%
    .[, obs_per_group := .N, by = group] %>%
    .[obs_per_group > 1, ]


  if (all(group_dt$group_var == 1)) {
    group_dt[, `:=`(
      group_var = NULL,
      vec_ip_d = NULL,
      d_X = NULL,
      d_Y = NULL,
      d_X2 = NULL,
      d_Y2 = NULL,
      distance2 = NULL
    )]
  } else {
    group_dt[, `:=`(
      vec_ip_d = NULL,
      d_X = NULL,
      d_Y = NULL,
      d_X2 = NULL,
      d_Y2 = NULL,
      distance2 = NULL
    )] %>%
      setnames("group_var", by_var)
  }

  return(st_as_sf(group_dt))
}

# /*=================================================*/
#' # Get medium distance between points
# /*=================================================*/
get_med_dist <- function(data_sf) {
  geom_type <- st_geometry(data_sf)[[1]] %>% class()

  if ("POLYGON" %in% geom_type) {
    med_distance <- data_sf %>%
      cbind(., st_coordinates(st_centroid(.))) %>%
      data.table() %>%
      .[, dif_X := c(0, diff(X))] %>%
      .[, dif_Y := c(0, diff(Y))] %>%
      .[, distance := sqrt(dif_X^2 + dif_Y^2)] %>%
      .[, median(distance)]
  } else {
    med_distance <- data_sf %>%
      cbind(., st_coordinates(.)) %>%
      data.table() %>%
      .[, dif_X := c(0, diff(X))] %>%
      .[, dif_Y := c(0, diff(Y))] %>%
      .[, distance := sqrt(dif_X^2 + dif_Y^2)] %>%
      .[, median(distance)]
  }

  return(med_distance)
}

get_med_dist_sec <- function(data_sf) {
  med_distance <- data_sf %>%
    cbind(., st_coordinates(.)) %>%
    data.table() %>%
    .[, dif_X := X - data.table::shift(X, type = "lag"), by = sectionid] %>%
    .[, dif_Y := Y - data.table::shift(Y, type = "lag"), by = sectionid] %>%
    .[, distance := sqrt(dif_X^2 + dif_Y^2)] %>%
    .[, median(distance, na.rm = TRUE)]

  return(med_distance)
}

# /*=================================================*/
#' # Flag unreliable data points
# /*=================================================*/

# data <- yield
# var_name <- "yield_vol"

flag_bad_points <- function(data, var_name, sd_factor) {
  temp_data <- data.table(data) %>%
    setnames(var_name, "var")

  var_sd <- temp_data[
    var >= quantile(var, prob = 0.05, na.rm = TRUE) &
      var <= quantile(var, prob = 0.95, na.rm = TRUE),
    .(median = median(var, na.rm = TRUE), sd = sd(var, na.rm = TRUE))
  ]

  temp_data <- temp_data %>%
    .[, flag_bad := 0] %>%
    .[
      var < var_sd$median - sd_factor * var_sd$sd,
      flag_bad := 1
    ] %>%
    .[
      var > var_sd$median + sd_factor * var_sd$sd,
      flag_bad := 1
    ] %>%
    setnames("var", var_name)

  return(st_as_sf(temp_data))
}

# /*=================================================*/
#' # Find the non-overlapping parts of polygons
# /*=================================================*/
get_non_ol_parts <- function(w_id) {
  w_ol_polygon <- filter(ol_polygons, asp_id == w_id)
  w_polygon <- filter(asp_sf, asp_id == w_id) %>%
    dplyr::select(asp_id, seed_rate)

  if (nrow(w_ol_polygon) == 0) {
    return(w_polygon)
  } else {
    nol_polygon <- st_union(w_ol_polygon) %>%
      st_difference(w_polygon, .)
    return(nol_polygon)
  }
}

# /*=================================================*/
#' # Download DEM (elevation data)
# /*=================================================*/
# sf <- boundary

get_dem_usgs <- function(sf, res = 3) {

  # /*----------------------------------*/
  #' ## Get the list of DEM urls
  # /*----------------------------------*/
  link <- paste0(
    "https://viewer.nationalmap.gov/tnmaccess/api/products?",
    "datasets=Original%20Product%20Resolution%20(OPR)%20",
    "Digital%20Elevation%20Model%20(DEM)",
    "&outputFormat=JSON",
    "&bbox=", paste(sf::st_bbox(sf::st_transform(sf, 4326)), collapse = ",")
  )

  df <- rjson::fromJSON(httr::content(httr::GET(link), as = "text"))

  if (df$messages == "Retrieved 0 item(s).  Unable to view record 1") {
    print("Unable to retrieve anything")
    return(NA)
  }

  url_ls <- lapply(df$items, function(x) x$downloadURL) %>% unlist()

  # /*----------------------------------*/
  #' ## Download all the DEM files
  # /*----------------------------------*/
  ras_tmp <- list()

  # temp_url <- url_ls[[1]]
  for (temp_url in url_ls) {
    temp_file <- file.path("Raw", basename(temp_url))

    download.file(temp_url, temp_file, mode = "wb")

    if (str_detect(temp_file, "zip")) {
      unzip(temp_file, exdir = "Raw")
      unlink(temp_file)
      temp_file <- str_replace(temp_file, "zip", "tif")
    }

    ras_tmp[[paste(temp_url)]] <- raster(temp_file)
  }

  # /*----------------------------------*/
  #' ## Combine
  # /*----------------------------------*/
  if (length(ras_tmp) == 1) {
    dem <- ras_tmp[[1]]
    names(dem)[1] <- "elevation"
  } else {
    #--- combine if more than one ---#
    names(ras_tmp) <- NULL
    ras_tmp$fun <- mean
    dem <- do.call(raster::mosaic, ras_tmp)
    names(dem)[1] <- "elevation"
  }

  #--- remove all the individual dem files ---#
  unlink(file.path("Raw", basename(url_ls)))

  return(dem)
}

get_dem_ilhmp <- function(obj, res = 1) {
  obj <- sf::st_buffer(obj, 10)
  obj_merc <- sf::st_transform(obj, 3857)
  b <- sf::st_bbox(obj_merc)
  img_size <- as.integer(abs(c(b[1] - b[3], b[2] - b[4])))
  img_size <- paste(img_size, collapse = "%2C")
  bb <- paste(b, collapse = "%2C")

  link <- paste0(
    "http://data.isgs.illinois.edu/arcgis/rest/services/",
    "Elevation/IL_Statewide_Lidar_DEM_WGS/ImageServer/",
    "exportImage?bbox=", bb, "&bboxSR=102100&imageSR=102100",
    "&format=tiff&pixelType=F32&f=pjson&size=", img_size
  )

  df <- rjson::fromJSON(httr::content(httr::GET(link), as = "text"))

  if ("error" %in% names(df)) {
    print("Unable to retrieve anything")
    return(NA)
  }

  tmp_dir <- tempdir()
  temp_file <- file.path(tmp_dir, "DEM.tif")
  download.file(df$href, temp_file, mode = "wb")
  dem <- raster::stack(temp_file)

  orig <- floor(sf::st_bbox(obj)[1:2])
  obj_sp <- as(obj, "Spatial")
  frst <- raster::raster(obj_sp, orig, res = res)
  dem <- raster::projectRaster(dem, frst)
  return(dem)
}

# /*=================================================*/
#' # SSURGO data processing
# /*=================================================*/

#' # Get ssurgo data for a fiel
# /*===========================================================
get_ssurgo_props <- function(field, vars, summarize = FALSE) {
  # Get SSURGO mukeys for polygon intersection
  ssurgo_geom <-
    SDA_spatialQuery(
      field,
      what = 'geom',
      db = 'SSURGO',
      geomIntersection = T
    ) %>%
    st_as_sf() %>%
    mutate(
      area = as.numeric(st_area(.)),
      area_weight = area / sum(area)
    )
  # Get soil properties for each mukey
  mukeydata <-
    get_SDA_property(
      property = vars,
      method = 'Weighted Average',
      mukeys = ssurgo_geom$mukey,
      top_depth = 0,
      bottom_depth = 150
    )
  ssurgo_data <- left_join(ssurgo_geom, mukeydata, by = 'mukey')
  if (summarize == TRUE) {
    ssurgo_data_sum <-
      ssurgo_data %>%
      data.table() %>%
      .[,
        lapply(.SD, weighted.mean, w = area_weight),
        .SDcols = vars
      ]
    return(ssurgo_data_sum)
  } else {
    return(ssurgo_data)
  }
}


# data <- soil_char_data
# key <- "mukey"
# weight <- "comppct.r"

get_aggregated <- function(data, key, weight, awc_sum = TRUE) {
  temp_data <- data %>%
    setnames(c(key, weight), c("key", "weight"))

  clay_temp <- temp_data[!is.na(claytotal.r) & sum(weight) != 0, .(claytotal.r = sum(weight * claytotal.r) / sum(weight)), by = key]
  silt_temp <- temp_data[!is.na(silttotal.r) & sum(weight) != 0, .(silttotal.r = sum(weight * silttotal.r) / sum(weight)), by = key]
  sand_temp <- temp_data[!is.na(sandtotal.r) & sum(weight) != 0, .(sandtotal.r = sum(weight * sandtotal.r) / sum(weight)), by = key]

  if (awc_sum == TRUE) {
    water_storage_temp <- temp_data[!is.na(awc.r), .(awc.r = sum(weight * awc.r)), by = key]
  } else {
    water_storage_temp <- temp_data[!is.na(awc.r), .(awc.r = sum(weight * awc.r) / sum(weight)), by = key]
  }

  return_data <- clay_temp[silt_temp, on = "key"][sand_temp, on = "key"][water_storage_temp, on = "key"] %>%
    .[order(key), ] %>%
    setnames("key", key)
}

# /*=================================================*/
#' # Create polygons
# /*=================================================*/

make_polygons <- function(data_dt) {

  #--- points data ---#
  temp_data <- data_dt[, .(X, Y, width)] %>%
    #--- get a vector of point to point ---#
    .[, `:=`(
      d_X = c(0, diff(X)),
      d_Y = c(0, diff(Y))
    )] %>%
    #--- distance ---#
    .[, vec_dist := sqrt(d_X^2 + d_Y^2)] %>%
    #--- normalized and adjust to half of the swidth ---#
    .[, d_X_norm := d_X * width / 2 / vec_dist] %>%
    .[, d_Y_norm := d_Y * width / 2 / vec_dist] %>%
    #--- fill NA at the top ---#
    .[is.na(d_X_norm), d_X_norm := 0] %>%
    .[is.na(d_Y_norm), d_Y_norm := 0]

  #--- matrix to rotate by 90 ---#
  rotate_mat_1 <- matrix(
    c(
      cos(pi / 2),
      sin(pi / 2),
      -sin(pi / 2),
      cos(pi / 2)
    ),
    nrow = 2
  )

  #--- line that is perpendicular to the line connecting two consecutive points ---#
  vec_add_p90 <- as.matrix(temp_data[, .(d_X_norm, d_Y_norm)]) %*% rotate_mat_1

  temp_data <- cbind(temp_data, vec_add_p90) %>%
    setnames(c("V1", "V2"), c("X_add_p90", "Y_add_p90")) %>%
    .[, `:=`(
      X_add_n90 = -X_add_p90,
      Y_add_n90 = -Y_add_p90
    )]

  #--- base points ---#
  XY <- temp_data[, .(X, Y)] %>% as.matrix()
  #--- vector to move in the direction to the next point ---#
  d_XY <- temp_data[, .(d_X, d_Y)] %>% as.matrix()
  #--- line that is perpendicular to the vector to the next point ---#
  add_p90_XY <- temp_data[, .(X_add_p90, Y_add_p90)] %>% as.matrix()
  add_p90_XY[1, ] <- add_p90_XY[2, ]

  b_p90 <- XY - d_XY / 2 + add_p90_XY
  b_n90 <- XY - d_XY / 2 - add_p90_XY
  m_p90 <- XY + add_p90_XY
  m_n90 <- XY - add_p90_XY
  f_p90 <- XY + rbind((d_XY / 2)[-1, ], matrix(0, 1, 2)) + rbind((add_p90_XY)[-1, ], matrix(0, 1, 2))
  f_n90 <- XY + rbind((d_XY / 2)[-1, ], matrix(0, 1, 2)) - rbind((add_p90_XY)[-1, ], matrix(0, 1, 2))

  # i <- 1
  make_polygon <- function(i) {
    temp_poly <- list(
      st_point(f_p90[i, ]),
      st_point(m_p90[i, ]),
      st_point(b_p90[i, ]),
      st_point(b_n90[i, ]),
      st_point(m_n90[i, ]),
      st_point(f_n90[i, ]),
      st_point(f_p90[i, ])
    ) %>%
      st_as_sfc() %>%
      st_union() %>%
      st_convex_hull() %>%
      st_geometry() %>%
      #--- revert to sfg ---#
      .[[1]]

    return(temp_poly)
  }

  all_polygons <- lapply(1:nrow(temp_data), make_polygon)

  return(all_polygons)
}

# /*----------------------------------*/
#' ## Make polygons when no section control (e.g., yield)
# /*----------------------------------*/
# sf_point <- yield_dt
# group_id <- 1

# This code works only if there is only one observation per time!!
# This works for yield data, but you need to be careful with as-planted
# and as-applied data where section control is available.
# For example, as-planted for CLC_JoeB_2020 will fail with this code.

get_polygons_by_group <- function(group_id, sf_point) {
  temp_sf_point <- sf_point[group == group_id, ]

  #--- points data ---#
  temp_data <- temp_sf_point[, .(X, Y, width)] %>%
    #--- get a vector of point to point ---#
    .[, `:=`(
      d_X = c(0, diff(X)),
      d_Y = c(0, diff(Y))
    )] %>%
    #--- distance ---#
    .[, vec_dist := sqrt(d_X^2 + d_Y^2)] %>%
    #--- normalized and adjust to half of the swidth ---#
    .[, d_X_norm := d_X * width / 2 / vec_dist] %>%
    .[, d_Y_norm := d_Y * width / 2 / vec_dist] %>%
    #--- fill NA at the top ---#
    .[is.na(d_X_norm), d_X_norm := 0] %>%
    .[is.na(d_Y_norm), d_Y_norm := 0]

  #--- matrix to rotate by 90 ---#
  rotate_mat_1 <- matrix(
    c(
      cos(pi / 2),
      sin(pi / 2),
      -sin(pi / 2),
      cos(pi / 2)
    ),
    nrow = 2
  )

  #--- line that is perpendicular to the line connecting two consecutive points ---#
  vec_add_p90 <- as.matrix(temp_data[, .(d_X_norm, d_Y_norm)]) %*% rotate_mat_1

  temp_data <- cbind(temp_data, vec_add_p90) %>%
    setnames(c("V1", "V2"), c("X_add_p90", "Y_add_p90")) %>%
    .[, `:=`(
      X_add_n90 = -X_add_p90,
      Y_add_n90 = -Y_add_p90
    )]

  #--- base points ---#
  XY <- temp_data[, .(X, Y)] %>% as.matrix()
  #--- vector to move in the direction to the next point ---#
  d_XY <- temp_data[, .(d_X, d_Y)] %>% as.matrix()
  #--- line that is perpendicular to the vector to the next point ---#
  add_p90_XY <- temp_data[, .(X_add_p90, Y_add_p90)] %>% as.matrix()

  b_p90 <- XY - d_XY / 2 + add_p90_XY
  b_n90 <- XY - d_XY / 2 - add_p90_XY
  m_p90 <- XY + add_p90_XY
  m_n90 <- XY - add_p90_XY
  f_p90 <- XY + rbind((d_XY / 2)[-1, ], matrix(0, 1, 2)) + rbind((add_p90_XY)[-1, ], matrix(0, 1, 2))
  f_n90 <- XY + rbind((d_XY / 2)[-1, ], matrix(0, 1, 2)) - rbind((add_p90_XY)[-1, ], matrix(0, 1, 2))

  make_polygon <- function(i) {
    temp_poly <- list(
      st_point(f_p90[i, ]),
      st_point(m_p90[i, ]),
      st_point(b_p90[i, ]),
      st_point(b_n90[i, ]),
      st_point(m_n90[i, ]),
      st_point(f_n90[i, ]),
      st_point(f_p90[i, ])
    ) %>%
      st_as_sfc() %>%
      st_union() %>%
      st_convex_hull() %>%
      st_geometry() %>%
      #--- revert to sfg ---#
      .[[1]]

    return(temp_poly)
  }

  all_polygons <- lapply(1:nrow(temp_data), make_polygon) %>%
    st_as_sfc() %>%
    st_as_sf()

  return(all_polygons)
}

#/*=================================================*/
#' # Recover sectionid
#/*=================================================*/
# data <- aa_input 

group_and_recover_section_id <- function(data) {

  #=== define section group ===#
  aa_dt <- data %>% 
    cbind(., st_coordinates(.)) %>% 
    data.table() %>% 
    .[, dif_X := c(0, diff(X))] %>% 
    .[, dif_Y := c(0, diff(Y))] %>%  
    #--- distance in meter ---#
    .[, dist := sqrt(dif_X ^ 2 + dif_Y ^ 2)] %>% 
    .[, dist_sec := median(dist)] %>% 
    .[, change_in_group := !(dist < dist_sec * 1.1 & dist > dist_sec * 0.9)] %>% 
    .[1, change_in_group := FALSE] %>% 
    .[, section_group := cumsum(change_in_group) + 1] %>% 
    .[, `:=`(
      dif_X = NULL,
      dif_Y = NULL,
      change_in_group = NULL,
      dist = NULL
    )]  

  dist_sec <- aa_dt[, dist_sec] %>% unique()
  angle_threshold <- 30

  #=== strip grouping by group centroid ===# 
  aa_strip_group <- aa_dt[, .(X = mean(X), Y = mean(Y)), by = section_group] %>% 
    .[, d_X := c(0, diff(X))] %>%
    .[, d_Y := c(0, diff(Y))] %>%
    .[, distance := sqrt(d_X^2 + d_Y^2)] %>%
    #--- if distance is 0, then it means the consecutive points are duplicates ---#
    .[distance != 0, ] %>%
    .[, d_X2 := data.table::shift(d_X, type = "lag", fill = NA)] %>%
    .[, d_Y2 := data.table::shift(d_Y, type = "lag", fill = NA)] %>%
    .[, distance2 := data.table::shift(distance, type = "lag", fill = NA)] %>%
    .[, vec_ip_d := (d_X * d_X2 + d_Y * d_Y2) / (distance * distance2)] %>%
    #--- get the angle of three consecutive points ---#
    .[, angle := acos(vec_ip_d) / pi * 180] %>%
    .[0.99 < vec_ip_d, angle := 0] %>%
    #--- 15 is the magic number (may not work) ---#
    .[, change_group := angle >= angle_threshold] %>%
    .[is.na(change_group), change_group := TRUE] %>%
    .[1, change_group := TRUE] %>%
    .[, strip_group := cumsum(change_group)] %>%
    .[, obs_per_group := .N, by = strip_group] %>%
    .[obs_per_group > 1, ] %>% 
    .[, .(section_group, strip_group, change_group)]

  aa_sf_section_unidentified <- aa_strip_group[aa_dt, on = "section_group"] %>% 
    .[section_group == 1, strip_group := 1] %>% 
    .[!is.na(strip_group),] %>% 
    setnames("dist_sec", "width")

  strip_groups_ls <- aa_sf_section_unidentified$strip_group %>% unique()

  plan(multiprocess, workers = detectCores() - 2)

  section_id_data <- future_lapply(
    strip_groups_ls,
    function(x) {
      recover_section_id_by_group(
        aa_sf_section_unidentified, 
        x
      )
    }
  ) %>% 
  rbindlist()

  data_to_return <- section_id_data[aa_sf_section_unidentified, on = "id"] %>% 
    .[, new_group := paste(strip_group, sectionid, sep = "_")] 

  return(data_to_return)
 
}


#/*=================================================*/
#' # Recover section id
#/*=================================================*/

recover_section_id_by_group <- function(data, group_id) {

  temp_data <- data[strip_group == group_id, ] %>% 
    .[, .(id, section_group, X, Y)]

  section_group_ls <- temp_data[, section_group] %>% unique()
  section_len <- length(section_group_ls)

  dist_data <- temp_data %>% 
    rename(section_group_id = section_group) %>% 
    rowwise() %>% 
    mutate(n_points = list(
      temp_data[section_group == section_group_id + 1, ] %>% 
        setnames(names(.), paste0(names(.), "_n"))
    )) %>% 
  unnest(n_points) %>% 
  data.table() %>% 
  .[, dist := sqrt((X - X_n) ^ 2 + (Y - Y_n) ^ 2)] %>% 
  .[, .SD[which.min(dist), ], by = id]

  max_id <- temp_data$id %>% max()

 # get_section_id(start_id = 13261, max_id)

  get_section_id <- function(start_id, max_id) {

    temp_id <- start_id
    id_ls <- temp_id  

    while (temp_id < max_id) {
      temp_id <- dist_data[id == temp_id, id_n]
      if (length(temp_id) == 0){
        break
      } else {
        id_ls <- c(id_ls, temp_id)
      }
    }

    section_id_data <- data.table(
      sectionid = start_id, 
      id = id_ls
    )

    return(section_id_data)
  }

  min_section_id <- dist_data$section_group_id %>% min()

  section_id_data <- lapply(
    dist_data[section_group_id == min_section_id, id],
    function(x) get_section_id(x, max_id)
  ) %>% 
  rbindlist()

  return(section_id_data)

}
  
# /*----------------------------------*/
#' ## make polygons when section control is available
# /*----------------------------------*/
# group_id <- 7
# sf_point <- as_planted_dt

# st_as_sf(temp_sf_point[1:100]) %>%
#   ggplot() +
#     geom_sf(aes(color = section_group), size = 0.5)

# the same reduce group shares the same angle

make_polygons_by_group_with_sc <- function(sf_point, group_id) {
  temp_sf_point <- sf_point[group == group_id, ]

  reduce_group_ls <- temp_sf_point[, reduce_group] %>% unique()
  rg_len <- length(reduce_group_ls)

  rg_points_ls <- vector(mode = "list", length = rg_len)

  for (i in 1:(rg_len - 1)) {

    # the last data point do not have a next point

    #--- working points ---#
    w_points <- temp_sf_point[reduce_group == reduce_group_ls[i], ] %>%
      st_as_sf()

    w_dist <- pull(w_points, distance) %>% unique()

    #--- next points ---#
    n_points <- temp_sf_point[reduce_group == reduce_group_ls[i + 1], ] %>%
      st_as_sf()

    dist_mat <- st_distance(w_points, n_points)

    point_data_ls <- vector(mode = "list", length = nrow(w_points))

    # j <- 1
    for (j in 1:nrow(w_points)) {
      temp_w_point <- w_points[j, ]

      too_far <- as.numeric(min(dist_mat[j, ])) > w_dist * 1.5


      if (!too_far) { # if the closest point is not too far

        temp_n_point <- n_points[which.min(dist_mat[j, ]), ]

        points_ls <- make_fmb_points(temp_w_point, temp_n_point)

        point_data_ls[[j]] <- rbind(
          tribble(
            ~asp_id, ~points, ~type,
            temp_w_point$asp_id, points_ls[[1]], "fm"
          ),
          tribble(
            ~asp_id, ~points, ~type,
            temp_n_point$asp_id, points_ls[[2]], "bm"
          )
        ) %>% data.table()
      } else { # if the closest point is too far (the last)

        point_data_ls[[j]] <- NULL
      }
    } #### loop over points done

    rg_points_ls[[i]] <- rbindlist(point_data_ls)
  } ### loop over reduce_group done

  all_rg_points <- rbindlist(rg_points_ls)

  # /*----------------------------------*/
  #' ## Create polygons for each of the points
  # /*----------------------------------*/
  asp_ls <- temp_sf_point$asp_id

  all_polygons_ls <- vector(mode = "list", length = length(asp_ls))

  for (i in 1:length(asp_ls)) {
    temp_point <- all_rg_points[asp_id == asp_ls[i], ]

    both_available <- all(c("bm", "fm") %in% temp_point$type)
    bm_only <- "bm" %in% temp_point$type & !both_available
    fm_only <- "fm" %in% temp_point$type & !both_available

    if (both_available) { # if both points are available

      temp_fm <- temp_point[type == "fm", points][[1]]
      temp_bm <- temp_point[type == "bm", points][[1]]

      temp_poly <- list(
        st_point(temp_fm$f_p90),
        st_point(temp_fm$m_p90),
        st_point(temp_bm$b_p90),
        st_point(temp_bm$b_n90),
        st_point(temp_fm$m_n90),
        st_point(temp_fm$f_n90),
        st_point(temp_fm$f_p90)
      ) %>%
        st_as_sfc() %>%
        st_union() %>%
        st_convex_hull() %>%
        st_geometry() %>%
        #--- revert to sfg ---#
        .[[1]]
    } else if (fm_only) {
      temp_fm <- temp_point[type == "fm", points][[1]]

      temp_poly <- list(
        st_point(temp_fm$f_p90),
        st_point(temp_fm$m_p90),
        st_point(temp_fm$m_n90),
        st_point(temp_fm$f_n90),
        st_point(temp_fm$f_p90)
      ) %>%
        st_as_sfc() %>%
        st_union() %>%
        st_convex_hull() %>%
        st_geometry() %>%
        #--- revert to sfg ---#
        .[[1]]
    } else if (bm_only) {
      temp_bm <- temp_point[type == "bm", points][[1]]

      temp_poly <- list(
        st_point(temp_bm$m_p90),
        st_point(temp_bm$b_p90),
        st_point(temp_bm$b_n90),
        st_point(temp_bm$m_n90),
        st_point(temp_bm$m_p90)
      ) %>%
        st_as_sfc() %>%
        st_union() %>%
        st_convex_hull() %>%
        st_geometry() %>%
        #--- revert to sfg ---#
        .[[1]]
    } else {
      temp_poly <- st_point(c(0, 0))
    }

    all_polygons_ls[[i]] <- temp_poly
  }

  all_polygons <- all_polygons_ls %>%
    st_as_sfc() %>%
    st_as_sf() %>%
    mutate(asp_id = asp_ls) %>%
    mutate(area = as.numeric(st_area(.))) %>%
    filter(area > 0)

  return(all_polygons)
}


make_fmb_points <- function(sfp_1, sfp_2, sec_width) {

  #--- points data ---#
  temp_data <- rbind(sfp_1, sfp_2) %>%
    data.table() %>%
    .[, .(X, Y, width = sec_width)] %>%
    #--- get a vector of point to point ---#
    .[, `:=`(
      d_X = c(0, diff(X)),
      d_Y = c(0, diff(Y))
    )] %>%
    #--- distance ---#
    .[, vec_dist := sqrt(d_X^2 + d_Y^2)] %>%
    #--- normalized and adjust to half of the swidth ---#
    .[, d_X_norm := d_X * width / 2 / vec_dist] %>%
    .[, d_Y_norm := d_Y * width / 2 / vec_dist]

  #--- matrix to rotate by 90 ---#
  rotate_mat_1 <- matrix(
    c(
      cos(pi / 2),
      sin(pi / 2),
      -sin(pi / 2),
      cos(pi / 2)
    ),
    nrow = 2
  )

  #--- line that is perpendicular to the line connecting two consecutive points ---#
  vec_add_p90 <- as.matrix(temp_data[2, .(d_X_norm, d_Y_norm)]) %*% rotate_mat_1

  #--- base points ---#
  w_XY <- temp_data[1, .(X, Y)] %>% as.matrix()
  n_XY <- temp_data[2, .(X, Y)] %>% as.matrix()

  #--- vector to move in the direction to the next point ---#
  d_XY <- temp_data[2, .(d_X, d_Y)] %>% as.matrix()

  #--- forward and mid points for the current point ---#
  f_p90 <- w_XY + d_XY / 2 + vec_add_p90
  f_n90 <- w_XY + d_XY / 2 - vec_add_p90
  #--- current point back and middle ---#
  w_m_p90 <- w_XY + vec_add_p90
  w_m_n90 <- w_XY - vec_add_p90
  #--- next point back and middle ---#
  n_m_p90 <- n_XY + vec_add_p90
  n_m_n90 <- n_XY - vec_add_p90

  w_points_return <- list(
    f_p90 = f_p90,
    f_n90 = f_n90,
    m_p90 = w_m_p90,
    m_n90 = w_m_n90
  )

  #--- backward points for the next point ---#
  n_points_return <- list(
    b_p90 = f_p90,
    b_n90 = f_n90,
    m_p90 = n_m_p90,
    m_n90 = n_m_n90
  )

  return(list(w_points_return, n_points_return))
}

# /*=================================================*/
#' # Used in combine_all_datasets.Rmd
# /*=================================================*/
stars_to_stack <- function(stars) {
  stack <- lapply(1:length(stars), function(x) as(stars[x], "Raster")) %>%
    do.call(raster::stack, .)
  return(stack)
}

# /*=================================================*/
#' # for EC data
# /*=================================================*/
st_interpolate <- function(obj, v, rst, type = "idw") {
  fml <- as.formula(paste0(v, " ~ 1"))

  if (!(type %in% c("idw", "nng", "pol"))) {
    stop(paste0(
      "Type not implemented! ",
      "Choose between idw (inverse distance weighted), ",
      "and nng (nearest neighbor)."
    ))
  }

  if (type == "pol") {
    w <- matrix(1, 5, 5)
    x <- fasterize::fasterize(obj, raster::raster(rst), v)
    x <- raster::focal(x, w = w, fun = raster::modal, NAonly = TRUE, na.rm = TRUE)
  } else {
    obj_sp <- as(obj, "Spatial")

    if (type == "idw") {
      # Setting nmax to 5 and idp to 1 is an
      #  inverse weighted interpolation:
      gs <- gstat::gstat(
        formula = fml, locations = obj_sp,
        nmax = 5, set = list(idp = 1)
      )
    } else {
      # Setting nmax to 1 and idp to 0 is equivalent to
      #  nearest neighbor interpolation:
      gs <- gstat::gstat(
        formula = fml, locations = obj_sp,
        nmax = 1, set = list(idp = 0)
      )
    }
    x <- raster::interpolate(raster::raster(rst), gs)
  }

  return(x)
}

# /*=================================================*/
#' # Used in yield_aggregate.R
# /*=================================================*/
make_point_chull <- function(geometry) {
  point_ls <- list()
  
  for (i in 1:length(geometry)) {
    temp_polygon <- geometry[[i]][[1]]
    
    point_ls[[i]] <- lapply(1:nrow(temp_polygon), function(x) st_point(temp_polygon[x, ])) %>%
      st_as_sfc() %>%
      st_as_sf()
  }
  
  all_points <- do.call("rbind", point_ls)
  
  convex_hull <- all_points %>%
    st_union() %>%
    st_convex_hull() %>%
    st_as_sf()
  
  return(convex_hull)
}

make_convex_hull <- function(sf, wid) {
  temp_geometry <- filter(sf, obs_id == wid) %>%
    st_geometry()

  return_sf <- make_point_chull(geometry = temp_geometry) %>%
    mutate(obs_id = wid)

  return(return_sf)
}

# /*=================================================*/
#' # Making variable names consistent
# /*=================================================*/

# data <- trial_design
# dictionary <- dict_td
# data <- aa_input
# type = input_type

make_var_name_consistent <- function(data, dictionary, type) {
  if (type %in% c("NH3", "urea", "uan32", "uan28", "1_2_1(36)", "LAN(26)", "MAP", "1_0_0", "1_0_1", "2_3_2(22)",
                  "15_10_6", "3_0_1", "2_3_4(32)", "4_3_4(33)", "5_1_5", "Sp", "N_equiv", "24-0-0-3 UAN","chicken_manure")){
    dict_type <- dictionary[type == "N", ]
  }else if (type %in% c("seeds", "seed")){
    dict_type <- dictionary[type == "S", ]
  }else if (type %in% c("K", "potassium")){
    dict_type <- dictionary[type == "K", ]
  }else if (type %in% c("Sulfur", "sulfur")){
    dict_type <- dictionary[type == "sulfur", ]
  }else if(type == "yield"){
    dict_type <- dictionary[type == "yield", ]
  }else if(type == "td"){
    dict_type <- dictionary[type == "trial_design", ]
  }else if(type == "ec"){
    dict_type <- dictionary[type == "ec", ]
  }else if(type == "soil"){
    dict_type <- dictionary[type == "soil", ]
  }else{
    dict_type <- dictionary[type == "other", ]
  }
  
  col_list <- dict_type[, column]

  for (col in col_list) {
    temp_names_ls <- dict_type[column == col, names][[1]]

    matches <- temp_names_ls %in% names(data)

    if (any(matches)) { # if there is a match

      data <- setnames(data, temp_names_ls[matches][1], col)
    } else {
      data <- mutate(data, !!col := NA)
    }
  }
  
  data <- dplyr::select(data, matches(c(col_list, "sectionid", "id")))

  return(data)
}

# /*=================================================*/
#' # Convert nitrogen units to N-equivalent
# /*=================================================*/
# form <- "N_equiv"
# unit <- "lbs"
# rate <- 33

convert_N_unit <- function(
  form,
  unit,
  rate,
  reporting_unit,
  conversion_type = "to_n_equiv"
) {
  
  conv_table <- 
  fromJSON(
    here("Data", "CommonData", "nitrogen_conversion.json"), 
    flatten = TRUE
  ) %>%
  data.table() %>%
  .[, conv_factor := as.numeric(conv_factor)] %>%
  .[, form_unit := paste(type, unit, sep = "_")] %>%
  as.data.frame()

  if (form == "N_equiv") {
    conv_factor_n <- 1
  } else {
    conv_factor_n <- which(conv_table[, "form_unit"] %in% paste(form, unit, sep = "_")) %>%
      conv_table[., "conv_factor"]
  }

  if (reporting_unit == "metric") {
    conv_factor_n <- conv_factor_n * conv_unit(1, "lbs", "kg") * conv_unit(1, "hectare", "acre")
  }

  if (conversion_type == "to_n_equiv") {
    converted_rate <- (conv_factor_n)*rate
  } else {
    converted_rate <- (1/conv_factor_n)*rate
  }

  return(as.numeric(converted_rate))
}

# /*=================================================*/
#' # Others
# /*=================================================*/
tm_layout_to_add <- tm_layout(
  legend.outside = "TRUE",
  frame = FALSE,
  legend.title.size = 2,
  legend.text.size = 1.5
)

# /*=================================================*/
#' # Close the unclosed polygons
# /*=================================================*/
# Case: Boeckner_134n_2017

# data_sf <- aan
# i <- 1393
close_unclosed <- function(data_sf) {
  return_closed_polygon <- function(i) {

    # print(i)

    temp_geom <- st_geometry(data_sf[i, ])[[1]][[1]]

    is_closed <- identical(temp_geom[1, ], temp_geom[nrow(temp_geom), ])

    if (!is_closed) {
      temp_geom <- rbind(temp_geom, temp_geom[1, ]) %>%
        list(.) %>%
        st_polygon()
    } else {
      temp_geom <- st_polygon(list(temp_geom))
    }

    return(temp_geom)
  }

  geoms <- future_lapply(seq_len(nrow(data_sf)), return_closed_polygon) %>%
    st_as_sfc() %>%
    st_set_crs(st_crs(data_sf))

  data_sf$geometry <- geoms

  return(data_sf)
}

# /*=================================================*/
#' # set crs to 4326
# /*=================================================*/

st_set_4326 <- function(data_sf) {
  if (is.na(st_crs(data_sf))) {
    data_sf <- st_set_crs(data_sf, 4326)
    cat("Warning: valid crs was not set for this data. Check carefully if this has caused any problems below.")
  }

  return(data_sf)
}

# /*=================================================*/
#' # Intersect yield and input polygons (overlap test)
# /*=================================================*/

# input_polygons <- input_polygons$input_polygons[[2]]
# ggplot() +
#   geom_sf(data = input_polygons[yield_polygons[5000, ], ], col = "blue") +
#   geom_sf(data = yield_polygons[5000, ], fill = "red", alpha = 0.3)

intersect_yield_input <- function(yield_polygons, input_polygons) {
  pct_int <- st_intersection(
    # dplyr::select(yield_polygons[50, ], yield_id, yield_area),
    dplyr::select(yield_polygons, yield_id, yield_area),
    dplyr::select(input_polygons, input_rate)
  ) %>%
    #--- percentage overlapped ---#
    mutate(
      sub_pct = as.numeric(st_area(.)) / yield_area
    ) %>%
    data.table() %>%
    #--- total sub_pct by yield polygon ---#
    .[, tot_sub_pct := sum(sub_pct), by = yield_id] %>%
    #--- calculate sub_pct-weighted MEAN of applied rate ---#
    .[, wm_input_rate := sum(sub_pct * input_rate) / tot_sub_pct, by = yield_id] %>%
    #--- weighted deviation from the mean ---#
    .[, dev_input_rate := sum(abs(sub_pct * (input_rate - wm_input_rate) / tot_sub_pct)), by = yield_id] %>%
    #--- order by yield_id ---#
    .[order(yield_id), ] %>%
    .[, .(
      yield_id,
      tot_sub_pct,
      wm_input_rate,
      dev_input_rate
    )] %>%
    setnames("wm_input_rate", "input_rate") %>%
    unique(by = "yield_id")

  return(pct_int)
}

# /*=================================================*/
#' # Table of input rate deviation allowed for a single yield polygon
# /*=================================================*/
crop <- c("corn","whitecorn", "soy", "wheat", "soft red winter wheat", "hard red spring wheat", "cotton","milo","canola")
input_types <- c("seed", "N", "K", "Sulfur")
max_dev_table <- expand.grid(crop = crop, input_types = input_types) %>%
  data.table() %>%
  .[, max_dev_allowed := c(
    4, # corn-seed
    4, # whitecorn-seed
    10, # soy-seed
    NA, # wheat-seed
    NA, # srwwheat-seed
    NA, # hrswheat-seed
    NA, # cotton-seed
    NA, # milo-seed
    NA, # canola-seed
    40, # corn-nitrogen
    40, # whitecorn-nitrogen
    NA, # soy-nitrogen
    40, # wheat-nitrogen
    20, # srwwheat-nitrogen
    20, # hrswheat-nitrogen
    NA, # cotton-nitrogen
    40, # milo-nitrogen
    40, # canola-nitrogen
    NA, # corn-K
    NA, # whitecorn-K
    NA, # soy-K
    NA, # wheat-K
    NA, # srwwheat-K
    NA, # hrswheat-K
    2, # cotton-K
    NA, # milo-nitrogen
    NA, # canola-nitrogen
    50, # corn-Sulfur
    NA, # whitecorn-Sulfur
    NA, # soy-Sulfur
    NA, # wheat-Sulfur
    NA, # srwwheat-Sulfur
    NA, # hrswheat-Sulfur
    NA, # cotton-Sulfur
    NA, # milo-S,
    NA # canola-S
  )]

# /*=================================================*/
#' # Table of maximum jump allowed from one yield polygon to the next
# /*=================================================*/
crop <- c("corn","whitecorn", "soy", "wheat", "soft red winter wheat", "hard red spring wheat", "cotton","milo","canola")
input_types <- c("seed", "N", "K", "Sulfur")
max_jump_table <- expand.grid(crop = crop, input_types = input_types) %>%
  data.table() %>%
  .[, max_jump_allowed := c(
    .5, # corn-seed
    .5, # whitecorn-seed
    1, # soy-seed
    NA, # wheat-seed
    NA, # srwwheat-seed
    NA, # hrswheat-seed
    NA, # cotton-seed
    NA, # milo-S,
    NA, # canola-S
    10, # corn-nitrogen
    10, # whitecorn-nitrogen
    NA, # soy-nitrogen
    10, # wheat-nitrogen
    5, # srwwheat-nitrogen
    5, # hrswheat-nitrogen
    NA, # cotton-nitrogen
    5, # milo-S,
    5, # canola-S
    NA, # corn-K
    NA, # whitecorn-K
    NA, # soy-K
    NA, # wheat-K
    NA, # srwwheat-K
    NA, # hrswheat-K
    2, # cotton-K
    NA, # milo-S,
    NA, # canola-S
    25, # corn-Sulfur
    NA, # whitecorn-Sulfur
    NA, # soy-Sulfur
    NA, # wheat-Sulfur
    NA, # srwwheat-Sulfur
    NA, # hrswheat-Sulfur
    NA, # cotton-Sulfur
    NA, # milo-S,
    NA # canola-S
  )]

# /*=================================================*/
#' # twi function
# /*=================================================*/
upslope.area <- function(dem, log = TRUE, atb = TRUE, deg = 0.1, fill.sinks = T) {
  # check
  if (fill.sinks) {
    # use capture.output to supress the function console output
    capture.output(dem <- invisible(raster::setValues(dem, topmodel::sinkfill(raster::as.matrix(dem), res = xres(dem), degree = deg))))
  }
  topidx <- topmodel::topidx(raster::as.matrix(dem), res = xres(dem))

  a <- raster::setValues(dem, topidx$area)
  if (log) {
    a <- log(a)
  }
  if (atb) {
    atb <- raster::setValues(dem, topidx$atb)
    # add the topographic index ln(a/tanB)
    a <- addLayer(a, atb)
    names(a) <- c("a", "atb")
  }
  return(a)
}


#### st_utm function to change the extent from decimal degrees to meter ####

st_utm <- function(sf_obj) {
  # Function to get UTM Zone from mean longitude:
  long2UTM <- function(long) {
    (floor((long + 180) / 6) %% 60) + 1
  }

  # Check if the object class is 'sf':
  obj_c <- class(sf_obj)[1]
  if (obj_c == "sf") {
    # In case the object has no projectin assigned,
    #  assume it to geographic WGS84 :
    if (is.na(sf::st_crs(sf_obj))) {
      sf::st_crs(sf_obj) <- sf::st_crs(4326)
    }

    # Get the center longitude in degrees:
    bb <- sf::st_as_sfc(sf::st_bbox(sf_obj))
    bb <- sf::st_transform(bb, sf::st_crs(4326))

    # Get UTM Zone from mean longitude:
    utmzone <- long2UTM(mean(sf::st_bbox(bb)[c(1, 3)]))

    # Get the hemisphere based on the latitude:
    NS <- 100 * (6 + (mean(sf::st_bbox(bb)[c(2, 4)]) < 0))

    # Add all toghether to get the EPSG code:
    projutm <- sf::st_crs(32000 + NS + utmzone)

    # Reproject data:
    sf_obj <- sf::st_transform(sf_obj, projutm)
    return(sf_obj)
  } else {
    options(error = NULL)
    stop("Object class is not 'sf', please insert a sf object!")
  }
}

curvature <- function(x, type = c("planform", "profile", "total", "mcnab", "bolstad"), ...) {
  if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")
  m <- matrix(1, nrow = 3, ncol = 3)
  type <- type[1]
  if (!any(c("planform", "profile", "total", "mcnab", "bolstad") %in% type)) {
    stop("Not a valid curvature option")
  }
  zt.crv <- function(m, method = type, res = raster::res(x)[1], ...) {
    p <- (m[6] - m[4]) / (2 * res)
    q <- (m[2] - m[8]) / (2 * res)
    r <- (m[4] + m[6] - 2 * m[5]) / (2 * (res^2))
    s <- (m[3] + m[7] - m[1] - m[9]) / (4 * (res^2))
    tx <- (m[2] + m[8] - 2 * m[5]) / (2 * (res^2))
    if (type == "planform") {
      return(round(-(q^2 * r - 2 * p * q * s + p^2 * tx) / ((p^2 + q^2) * sqrt(1 + p^2 + q^2)), 6))
    } else if (type == "profile") {
      return(round(-(p^2 * r + 2 * p * q * s + q^2 * tx) / ((p^2 + q^2) * sqrt(1 + p^2 + q^2)^3), 6))
    } else if (type == "total") {
      return(round(-(q^2 * r - 2 * p * q * s + p^2 * tx) / ((p^2 + q^2) * sqrt(1 + p^2 + q^2)), 6) +
        round(-(p^2 * r + 2 * p * q * s + q^2 * tx) / ((p^2 + q^2) * sqrt(1 + p^2 + q^2)^3), 6))
    }
  }
  if (type == "bolstad") {
    return(10000 * ((x - raster::focal(x, w = m, fun = mean)) / 1000 / 36.2))
  } else if (type == "mcnab") {
    mcnab <- function(x, ...) (((x[5] - x) + (x[5] - x)) / 4)
    return(raster::focal(x, w = m, fun = mcnab, ...))
  } else {
    return(raster::focal(x,
      w = m, fun = function(x) {
        zt.crv(m = x, type = type)
      }, pad = TRUE,
      padValue = 0, ...
    ))
  }
}


tpi <- function(x, scale = 3, win = "rectangle", normalize = FALSE,
                zero.correct = FALSE) {
  if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")
  if (win == "circle") {
    if (scale < raster::res(x)[1] * 2) {
      stop("Scale is too small for a circular window")
    }
    m <- raster::focalWeight(x, scale, type = "circle")
    m[m > 0] <- 1
  } else {
    if (scale %% 2 == 0) {
      stop("Scale for a rectangular window must be an odd number")
    }
    m <- matrix(1, nrow = scale, ncol = scale)
  }
  if (zero.correct) {
    tp <- x - raster::focal(x, w = m, fun = function(x, ...) {
      sum(x) / sum(m)
    })
  } else {
    tp <- x - raster::focal(x, w = m, fun = mean)
  }
  if (normalize == TRUE) {
    if (zero.correct) {
      tp.sd <- raster::focal(x, w = m, fun = function(x, ...) {
        sqrt(sum(x - sum(x) / sum(m))^2 / (sum(m) - 1))
      })
    } else {
      tp.sd <- raster::focal(x, w = m, fun = stats::sd)
    }
    tp <- tp / tp.sd
  }
  return(tp)
}
                            
upslope <- function (dem, log = TRUE, atb = FALSE, deg = 0.12, fill.sinks = TRUE) 
{
  if (!all.equal(raster::xres(dem), raster::yres(dem))) {
    stop("Raster has differing x and y cell resolutions. Check that it is in a projected coordinate system (e.g. UTM) and use raster::projectRaster to reproject to one if not. Otherwise consider using raster::resample")
  }
  if (fill.sinks) {
    capture.output(dem <- invisible(raster::setValues(dem, topmodel::sinkfill(raster::as.matrix(dem), res = raster::xres(dem), degree = deg))))
  }
  topidx <- topmodel::topidx(raster::as.matrix(dem), res = raster::xres(dem))
  a <- raster::setValues(dem, topidx$area)
  if (log) {
    a <- log(a)
  }
  if (atb) {
    atb <- raster::setValues(dem, topidx$atb)
    a <- addLayer(a, atb)
    names(a) <- c("a", "atb")
  }
  return(a)
}

create_layers <- function (dem, fill.sinks = TRUE, deg = 0.1) 
{
  layers <- stack(dem)
  message("Building upslope areas...")
  a.atb <- upslope(dem, atb = TRUE, fill.sinks = fill.sinks, deg = deg)
  layers <- addLayer(layers, a.atb)
  names(layers) <- c("filled.elevations", "upslope.area", "twi")
  return(layers)
}

# input_type = input_type_sec
# trial_design
id_zero_plots <- function(input_type) {
  plot_no <-  here("Data/Growers", ffy, "TrialDesign") %>% 
    list.files(recursive = FALSE, full.names = TRUE, include.dirs = FALSE) %>%
    .[str_detect(., "shp")] %>%
    .[!str_detect(., "xml")] %>%
    .[str_detect(., paste0("trial-design-", tolower(input_type)))] %>%
    st_read(.) %>%
  dplyr::rename("tgti" = paste0("tgt_", input_type)) %>%
    filter(tgti == 0) %>%
    nrow(.)
  if(plot_no > 0){
    zero_plots = TRUE
  }else{
    zero_plots = FALSE
  }
  return(zero_plots)
}

# crop_name = crop
# input_type_name = input_type
# ffy

get_max_dev <- function(crop_name, input_type_name, ffy){
  if(input_type_name %in% c("NH3", "urea", "uan32", "uan28", "1_2_1(36)", "LAN(26)", "MAP", "1_0_0", "1_0_1", "2_3_2(22)",
                            "15_10_6", "3_0_1", "2_3_4(32)", "4_3_4(33)", "5_1_5", "Sp", "N_equiv", "24-0-0-3 UAN","chicken_manure")){
    max_dev = max_dev_table %>%
      filter(crop == crop_name & input_types == "N") %>%
      pull(max_dev_allowed)
  }else if(input_type_name %in% max_dev_table$input_type){
    max_dev = max_dev_table %>%
      filter(crop == crop_name & input_types == input_type_name) %>%
      pull(max_dev_allowed)
  }else{
    #--- trial design ---#
    td_file_name <- here("Data/Growers", ffy, "TrialDesign") %>% 
      list.files(recursive = FALSE, full.names = TRUE, include.dirs = FALSE) %>%
      .[str_detect(., "shp")] %>%
      .[!str_detect(., "xml")] %>%
      .[str_detect(., "trial-design")] 
    
    if (length(td_file_name) > 1) {
      td_file_name <- td_file_name %>% 
        .[str_detect(., paste0("trial-design-", tolower(input_type_name)))]
    }
    
    td <- st_read(td_file_name) %>%
      st_transform_utm() %>%
      setnames(names(.), tolower(names(.)))
    
    tgt_name <- data.table(name = names(td),
                           con_tgt = str_detect(names(td), "tgt")) %>%
      filter(con_tgt == TRUE) %>%
      dplyr::pull(name)
    
    max_dev <- td %>%
      pull(eval(parse(text = tgt_name))) %>%
      unique() %>%
      sort() %>%
      diff() %>%
      median(.)*0.4
  }
  
  return(max_dev)
}

get_max_jump <- function(crop_name, input_type_name, ffy){
  if(input_type_name %in% c("NH3", "urea", "uan32", "uan28", "1_2_1(36)", "LAN(26)", "MAP", "1_0_0", "1_0_1", "2_3_2(22)",
                            "15_10_6", "3_0_1", "2_3_4(32)", "4_3_4(33)", "5_1_5", "Sp", "N_equiv", "24-0-0-3 UAN","chicken_manure")){
    max_jump = max_jump_table %>%
      filter(crop == crop_name & input_types == "N") %>%
      pull(max_jump_allowed)
  }else if(input_type_name %in% max_jump_table$input_type){
    max_jump = max_jump_table %>%
      filter(crop == crop_name & input_types == input_type_name) %>%
      pull(max_jump_allowed)
  }else{
    #--- trial design ---#
    td_file_name <- here("Data/Growers", ffy, "TrialDesign") %>% 
      list.files(recursive = FALSE, full.names = TRUE, include.dirs = FALSE) %>%
      .[str_detect(., "shp")] %>%
      .[!str_detect(., "xml")] %>%
      .[str_detect(., "trial-design")] 
    
    if (length(td_file_name) > 1) {
      td_file_name <- td_file_name %>% 
        .[str_detect(., paste0("trial-design-", tolower(input_type_name)))]
    }
    
    td <- st_read(td_file_name) %>%
      st_transform_utm() %>%
      setnames(names(.), tolower(names(.)))
    
    tgt_name <- data.table(name = names(td),
                       con_tgt = str_detect(names(td), "tgt")) %>%
      filter(con_tgt == TRUE) %>%
      dplyr::pull(name)
    
    max_jump <- td %>%
      pull(eval(parse(text = tgt_name))) %>%
      unique() %>%
      sort() %>%
      diff() %>%
      median(.)*0.1
  }
  
  return(max_jump)
}


get_td <- function(ffy, input_type_name){
  #--- trial design ---#
  td_file_name <- here("Data/Growers", ffy, "TrialDesign") %>% 
    list.files(recursive = FALSE, full.names = TRUE, include.dirs = FALSE) %>%
    .[str_detect(., "shp")] %>%
    .[!str_detect(., "xml")] %>%
    .[str_detect(., "trial-design")] 
  
  if (length(td_file_name) > 1) {
    td_file_name <- td_file_name %>% 
      .[str_detect(., paste0("trial-design-", tolower(input_type_name)))]
  }
  
  td <- st_read(td_file_name) %>%
    st_transform_utm() %>%
    setnames(names(.), tolower(names(.)))
  
  tgt_name <- data.table(name = names(td),
                         con_tgt = str_detect(names(td), "tgt")) %>%
    filter(con_tgt == TRUE) %>%
    dplyr::pull(name)
  
  #td <- td %>%
   # dplyr::rename(tgt_name = paste0("tgt_", input_type))
  
  input_data_current = trial_info %>%
    filter(input_type == input_type_name)

  if ((input_type_name %in% c("NH3", "urea", "uan32", "uan28", "1_2_1(36)", "LAN(26)", "MAP", "1_0_0", "1_0_1", "2_3_2(22)",
                        "15_10_6", "3_0_1", "2_3_4(32)", "4_3_4(33)", "5_1_5", "Sp", "N_equiv", "24-0-0-3 UAN","chicken_manure"))
      & input_data_current$convert == TRUE) {
    
    n_base_rate = input_data_current %>%
      pull(base_rate)
    
    td <- mutate(td, 
                 !!as.symbol(paste0("tgt_", tolower(input_type_name))) := convert_N_unit(
                   input_data_current$form, 
                   input_data_current$unit, 
                   eval(parse(text = paste0("tgt_", tolower(input_type_name)))), 
                   w_field_data$reporting_unit
                 ) + n_base_rate
    )
  } else if (input_type_name == "seed") {
    #--- seed rate conversion ---#
    if (any(td$tgt_seed > 10000)){
      #--- convert to K ---#
      td <- mutate(td, tgt_seed = tgt_seed / 1000)
    }
  }
  return(td)
}

# data = final_data
# i = 1
add_tgt_rates <- function(data, trial_info, ffy){
  
  input_list <- trial_info %>%
    #filter(process == "TRUE") %>%
    pull(input_type) %>%
    unlist() 
  
  for (i in 1:length(input_list)){
    
    data <- get_td(ffy, input_list[i]) %>%
      st_intersection(., st_centroid(data)) %>%
      pull(eval(parse(text = paste0("tgt_", tolower(input_list[i]))))) %>%
      cbind(data, .) 
    names(data)[ncol(data) - 1] = paste0("tgt_", tolower(input_list[i]))
    
    # calculate the difference between targets in the trial design
    tgt_diffs <- unique(data.frame(data)[, paste0("tgt_", tolower(input_list[i]))]) %>%
      sort() %>%
      diff()
    
    decision_rule <- max(max(abs(mean(tgt_diffs) - tgt_diffs)/median(tgt_diffs))*0.9, 0.4) # find what it would be if the rates were perfectly applied
    
    for (j in length(unique(data.frame(data)[, paste0("tgt_", tolower(input_list[i]))])):1){
      
      km <- kmeans(eval(parse(text = paste0('data$',tolower(input_list[i]),'_rate'))), centers = j, nstart = 20)
      
      diffs <- km$centers %>%
        unique() %>%
        sort() %>%
        diff() 
      
      if(TRUE %in% (diffs < median(diffs)*decision_rule)){
        print("Not enough variation in applied rates. Reducing number of target rates.")
      }else{
        data <- data %>%
          mutate(treat = round(km$centers[km$cluster], 2))
        
        names(data)[ncol(data)] = paste0("trt_", tolower(input_list[i]))
        break
      }
    }
  }
  
  return(data)
}

get_angle <- function(data){
  angle <- data %>%
    st_centroid(.) %>%
    cbind(., st_coordinates(.)) %>%
    data.table() %>%
    .[, original_order_id := 1:nrow(.)] %>%
    setorder(original_order_id) %>%
    .[, d_X := c(0, diff(X))] %>%
    .[, d_Y := c(0, diff(Y))] %>%
    .[, distance := sqrt(d_X^2 + d_Y^2)] %>%
    #--- if distance is 0, then it means the consecutive points are duplicates ---#
    .[distance != 0, ] %>%
    .[, d_X2 := data.table::shift(d_X, type = "lag", fill = NA)] %>%
    .[, d_Y2 := data.table::shift(d_Y, type = "lag", fill = NA)] %>%
    .[, distance2 := data.table::shift(distance, type = "lag", fill = NA)] %>%
    .[, vec_ip_d := (d_X * d_X2 + d_Y * d_Y2) / (distance * distance2)] %>%
    #--- get the angle of three consecutive points ---#
    .[, angle := acos(vec_ip_d) / pi * 180] %>%
    .[0.99 < vec_ip_d, angle := 0] %>%
    pull(angle)
  return(angle)
}

                            
                            
