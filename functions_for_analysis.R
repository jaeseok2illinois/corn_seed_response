st_transform_utm <- function(sfobject){               
  crs <- st_crs(sfobject)         
  if (str_detect(crs$wkt, "longitude") != TRUE){                    
    print("Not in lat/long. Returning original object.")            
    return(sfobject)              
  }
  else {
    utmzone <- utm_zone(mean(st_bbox(sfobject)[c(1,3)]))
    projutm <- as.numeric(paste0("326", utmzone))
    newobj <- st_transform(sfobject, projutm)
    return(newobj)
  }
}

utm_zone <- function(long){                   
  utm <- (floor((long + 180)/6) %% 60) + 1                          
  return(utm)                     
}


get_ttest_text <- function(test_results, zone){

  t <- test_results[zone_txt == paste0("Zone ", zone), t]

  if (t < 1.30){
    temp_text <- "The data and model provide negligible evidence that the estimated optimal rate of `r get_seed(\"Optimal\", zone)`K does not provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(zone)`)."
  } else if (1.30 <= t & t < 1.64){
    temp_text <- "The data and model provide only limited evidence that the estimated optimal rate of `r get_seed(\"Optimal\", zone)`K did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(zone)`)."
  } else if (1.64 <= t & t < 1.96){
    temp_text <- "The data and model provide moderate evidence that the estimated optimal rate of `r get_seed(\"Optimal\", zone)`K did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(zone)`)."
  } else {
    temp_text <- "The data and model provide strong evidence that the estimated optimal rate of `r get_seed(\"Optimal\", zone)`K did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(zone)`)." 
  }

  return(gsub("zone", zone, temp_text))
}

#/*=================================================*/
#' # Yield and Profit Predictions
#/*=================================================*/

# est <- data_analysis_s$gam_res[[1]]
# data <- data_analysis_s$data[[1]]
# var_name <- "seed_rate"

# data_dt[, n_rate := seq(1:nrow(data_dt))]

# all_var_names_ls_ls <- c(all_var_names_ls_ls, "n_rate")
# est$model$n_rate <- 1

make_data_for_eval <- function(data, est) {

# data <- analysis_res_gcr$data[[1]]
# est <- analysis_res_gcr$gam_res[[1]]

  data_dt <- data.table(data)

  var_names_ls <- est$model %>% 
    data.table() %>% 
    dplyr::select(- any_of(c("input_rate", "yield"))) %>%
    names() 

  data_for_eval <- data_dt[, ..var_names_ls] %>% 
    .[, lapply(.SD, mean), by = zone_txt]

  return(data_for_eval)

}

make_eval_data_gc <- function(data, data_for_eval, gc_type, w_gc_rate) {

  if (gc_type == "Rx") {
    #=== individual plot data ===#
    data_for_eval <- data %>% 
      .[, input_rate := NULL] %>% 
      #=== designate gc_rate as input_rate for prediction ===#
      .[, input_rate := gc_rate]  
  } else {
    #=== data by zone ===#
    data_for_eval <- data_for_eval %>% 
      #=== designate gc_rate as input_rate for prediction ===#
      .[, gc_rate := w_gc_rate]
  }

  return(data_for_eval)
}

# input_rate_seq <- analysis_res_p$input_rate_seq[[1]]
# data_for_eval <- analysis_res_p$data[[1]]

predict_yield_range <- function(data_for_eval, input_rate_seq, est) {

  eval_data <- data_for_eval[input_rate_seq, on = "zone_txt"]  

  #--- predict yield ---#
  yield_prediction <- predict(est, newdata = eval_data, se = TRUE)

  eval_data <- eval_data %>% 
    .[, `:=`(
      yield_hat = yield_prediction$fit,
      yield_hat_se = yield_prediction$se.fit
    )] 

  return(eval_data)

}

predict_yield <- function(data, est, var_name) {

  eval_data <- data.table::copy(data) %>% 
    dplyr::select(-input_rate) %>% 
    setnames(var_name, "input_rate")

  yield_prediction <- predict(est, newdata = eval_data, se = TRUE)

  #--- predict yield ---#
  eval_data <- eval_data %>% 
  mutate(
    yield_hat = yield_prediction$fit,
    yield_hat_se = yield_prediction$se.fit
  )
   
  return(eval_data)

}

#/*=================================================*/
#' # Profit-differential test 
#/*=================================================*/
# Notes: test if the profit associated with the optimal and grower-chosen
# rates are statistically significantly different from zero 

get_dif_stat <- function(data, test_var, opt_var, gc_var, gam_res, crop_price, input_price){
    
  if ("scam" %in% class(gam_res)) {
    gam_coef <- gam_res$coefficients.t
    gam_V <- gam_res$Ve.t
  } else {
    gam_coef <- gam_res$coefficients
    gam_V <- gam_res$Ve
  }

  base_data <- data.table::copy(data) %>% 
    .[, (test_var) := get(gc_var)]

  comp_data <- data.table::copy(data) %>% 
    .[, (test_var) := get(opt_var)]

  #/*----------------------------------*/
  #' ## Profit (gc)
  #/*----------------------------------*/
  Xmat_base <- predict(gam_res, newdata = base_data, type = "lpmatrix") 
  # predict(gam_res, newdata = base_data) %>% mean

  #--- vector of 1s for summation divided by the number of observations for averaging ---#
  ones <- matrix(1 / dim(Xmat_base)[1], 1, dim(Xmat_base)[1])

  #--- average yield ---#
  yhat_base <- ones %*% Xmat_base %*% gam_coef

  #--- point estimate of profit differential ---#
  pi_gc <- crop_price * yhat_base - (input_price * ones %*% base_data$input_rate)  

  big_mat_base <- ones %*% Xmat_base

  #--- se of the profit differential  ---# 
  pi_gc_se <- crop_price * sqrt(big_mat_base %*% gam_V %*% t(big_mat_base))

  #/*----------------------------------*/
  #' ## Profit (optimal)
  #/*----------------------------------*/
  Xmat_comp <- predict(gam_res, newdata = comp_data, type = "lpmatrix") 

  #--- vector of 1s for summation divided by the number of observations for averaging ---#
  ones <- matrix(1 / dim(Xmat_comp)[1], 1, dim(Xmat_comp)[1])

  #--- average yield ---#
  yhat_comp <- ones %*% Xmat_comp %*% gam_coef

  #--- point estimate of profit differential ---#
  pi_opt <- crop_price * yhat_comp - (input_price * ones %*% comp_data$input_rate)  

  big_mat_comp <- ones %*% Xmat_comp

  #--- se of the profit differential  ---# 
  pi_opt_se <- crop_price * sqrt(big_mat_comp %*% gam_V %*% t(big_mat_comp))

  #/*----------------------------------*/
  #' ## Profit differential
  #/*----------------------------------*/
  #--- difference in X mat ---#
  X_dif_mat <- Xmat_comp - Xmat_base

  #--- vector of 1s for summation divided by the number of observations for averaging ---#
  ones <- matrix(1 / dim(X_dif_mat)[1], 1, dim(X_dif_mat)[1])

  #--- X_dif_mat summed ---#
  big_mat_dif <- ones %*% X_dif_mat

  #--- point estimate of profit differential ---#
  pi_dif <- ones %*% ((crop_price * X_dif_mat %*% gam_coef) - input_price * (comp_data$input_rate - base_data$input_rate))  

  #--- se of the profit differential  ---# 
  pi_dif_se <- crop_price * sqrt(big_mat_dif %*% gam_V %*% t(big_mat_dif))

  #--- t-stat ---#
  t_stat <- (pi_dif/pi_dif_se) %>% round(digits = 2)  

  return_data <- data.table(
    yhat_est_gc = yhat_base[1, 1],
    point_est_gc = pi_gc[1, 1],
    point_est_gc_se = pi_gc_se[1, 1],
    yhat_est_opt = yhat_comp[1, 1],
    point_est_opt = pi_opt[1, 1],
    point_est_opt_se = pi_opt_se[1, 1],
    point_est_dif = pi_dif[1, 1],
    point_est_dif_se = pi_dif_se[1, 1],
    t = t_stat[1, 1]
  )

  return(return_data)

}

# data = analysis_res_o$data[[1]] %>% data.table()
# gam_res = analysis_res_o$gam_res[[1]]
# input_price = analysis_res_o$price[[1]]

get_dif_stat_zone <- function(data, test_var, opt_var, gc_var, gam_res, by_var, crop_price, input_price){

  zone_ls <- data[, ..by_var] %>% 
    unique() %>% 
    unlist()

  temp_data <- setnames(data.table::copy(data), by_var, "by_var")

  # y <- zone_ls[1]

  return_data <- lapply(
    zone_ls,
    function(y) 
    get_dif_stat(
      data = temp_data[by_var == y, ] %>% 
        setnames("by_var", by_var),
      test_var,
      opt_var,
      gc_var,
      gam_res,
      crop_price,
      input_price
    ) %>% 
    mutate(by_var = y)
  ) %>% 
  rbindlist() %>% 
  setnames("by_var", by_var)

  return(return_data)

}

# data <- analysis_res_o$data[[1]]
# gc_type <- analysis_res_o$gc_type[[1]]
# gam_res <- analysis_res_o$gam_res[[1]]
# crop_price <- analysis_res_o$crop_price[[1]]
# input_price <- analysis_res_o$price[[1]]

get_pi_dif_test_zone <- function(data, gc_type, gam_res, crop_price, input_price) {

  if (gc_type == "uniform") {

    data_for_test <- data.table(data)

    pi_dif_test_zone <- get_dif_stat_zone(
      data = data_for_test, 
      test_var = "input_rate", 
      opt_var = "opt_input",
      gc_var = "gc_rate",
      gam_res = gam_res,
      by_var = "zone_txt",
      crop_price = crop_price,
      input_price = input_price
    )

  } else {

    pi_dif_test_zone <- get_dif_stat_zone(
      data = data.table(data), 
      test_var = "input_rate", 
      opt_var = "opt_input",
      gc_var = "gc_rate",
      gam_res = gam_res,
      by_var = "zone_txt",
      crop_price = crop_price,
      input_price = input_price
    )

  }

  return(pi_dif_test_zone)
}

find_opt_u <- function(data, gam_res, crop_price, input_price) {

  data_dt <- data.table(data)

  input_ls <- seq(
    quantile(data_dt$input_rate, prob = 0.025), 
    quantile(data_dt$input_rate, prob = 0.975), 
    length = 100
  )

  opt_input_u <- data_dt %>% 
  # this is okay because each zone has the same
  # number of observations
  # unique(by = "zone_txt") %>% 
  .[rep(1:nrow(.), length(input_ls)), ] %>% 
  .[, input_rate := rep(input_ls, each = nrow(.)/length(input_ls))] %>% 
  .[, yield_hat := predict(gam_res, newdata = .)] %>% 
  .[, profit_hat := crop_price * yield_hat - input_price * input_rate] %>% 
  .[, .(profit_hat = mean(profit_hat)), by = input_rate] %>% 
  .[order(profit_hat), ] %>% 
  .[.N, input_rate]

  return(opt_input_u)

} 

#/*=================================================*/
#' # filter data 
#/*=================================================*/

# data_sf <- analysis_res$data[[1]]
# field_var_ls <- analysis_res$field_vars[[1]]

gen_y_res <- function (data_sf, field_var_ls){

  if (length(field_var_ls) == 0) {
    data_sf <- mutate(data_sf, res_y = yield)
    return(data_sf)
  } else {
    #/*----------------------------------*/
    #' ## Regress yield on characteristics 
    #/*----------------------------------*/
    field_vars_f <- field_var_ls %>% 
    .[. %in% names(data_sf)] %>% 
    paste(., collapse = " + ")

    #--- regression formula ---#
    y_field_formula <- formula(
      paste(
        #=== main ===#
        "yield ~ ",
        #=== field/soil char ===#
        field_vars_f
      )
    )

    data_sf <- data_sf %>% 
      .[, c("obs_id", "yield", "input_rate", field_var_ls, "X", "Y")] %>% 
      .[complete.cases(st_drop_geometry(.)), ] %>% 
      mutate(
        res_y = lm(y_field_formula, data = data_sf)$res
      )

    return(data_sf)
  }
}

#/*=================================================*/
#' # Run scam or gam
#/*=================================================*/
# data <- analysis_res_g$data[[1]]
# field_vars <- analysis_res_m$field_vars[[1]]

run_scam_gam <- function(data, field_vars){

  results <- NULL
  
  # results <- tryCatch(
  #     {
  #       withTimeout(
  #         {
  #           formula <- paste0(
  #             "yield ~ s(input_rate, bs = \"cv\", by = zone_txt) + s(X, k = 5) + s(Y, k = 5) + te(X, Y, k = c(5, 5))",
  #             ifelse(
  #               length(field_vars) > 0,
  #               paste0(" + ", paste0(field_vars, collapse = " + ")),
  #               ""
  #             )
  #           ) %>% formula()

  #           scam(formula, data = data)
  #         },
  #         timeout = 20, # 20 seconds,
  #         onTimeout = "silent"
  #       )
  #     }, 
  #     error = function(cond){
  #       return(NULL)
  #     }
  #   )

  # #=== start with 6 knots ===#
  # start_k <- 6

  # while (is.null(results) & start_k >= 5) {

  #   results <- tryCatch(
  #     {
  #       withTimeout(
  #         {
  #           formula <- paste0(
  #             "yield ~ s(input_rate, k = ",
  #             start_k,
  #             ", bs = \"cv\", by = zone_txt) + s(X, k = 5) + s(Y, k = 5) + te(X, Y, k = c(5, 5))",
  #             ifelse(
  #               length(field_vars) > 0,
  #               paste0(" + ", paste0(field_vars, collapse = " + ")),
  #               ""
  #             )
  #           ) %>% formula()

  #           scam(formula, data = data)
  #         },
  #         timeout = 20, # 20 seconds,
  #         onTimeout = "silent"
  #       )
  #     }, 
  #     error = function(cond){
  #       return(NULL)
  #     }
  #   )

  #   start_k <- start_k - 1 

  # }

  if (is.null(results)) {

    formula <- paste0(
      "yield ~ s(input_rate, k = 4, by = zone_txt) + s(X, k = 5) + s(Y, k = 5) + te(X, Y, k = c(5, 5))",
      ifelse(
        length(field_vars) > 0,
        paste0(" + ", paste0(field_vars, collapse = " + ")),
        ""
      )
    ) %>% formula()

    results <- gam(formula, data = data)

  }

 return(results) 

}

#/*=================================================*/
#' # GWR-analysis
#/*=================================================*/

# var_name <- "seed_rate"
# data_sf <- rename(data_sf, input_rate = s_rate)

run_gwr <- function(data_sf, var_name) {

  reg_data_sp <- data_sf %>%
    as("Spatial")

  # library(matrixcalc)
  # is.singular.matrix(dMat)

  #--- find optimal bandwidth ---#
  dMat <- data_sf %>% 
    st_centroid() %>% 
    st_coordinates() %>% 
    as.matrix() %>% 
    gw.dist()

  reg_formula <- formula("res_y ~ log(input_rate)")

  # obw <- bw.gwr(
  #   reg_formula,
  #   data = reg_data_sp,
  #   approach = "AICc",
  #   kernel = "gaussian",
  #   adaptive = T,
  #   dMat = dMat
  # )

  #--- gwr estimation with optimal bw ---#
  gwr_est <- gwr.basic(
    reg_formula,
    data = reg_data_sp,
    bw = 100,
    # bw = obw,
    kernel = "gaussian",
    adaptive = T
  )

  #--- join the coefficients to the sf ---#
  data_sf <- data.table(
    obs_id = data_sf$obs_id,
    b_int = gwr_est$SDF$Intercept,
    b_slope = gwr_est$SDF@data[, paste0("log(", var_name, ")")]
  ) %>%
  left_join(data_sf, ., by = "obs_id") 

  # ggplot(data = data_sf) +
  #   geom_histogram(aes(x = b_slope))

  return(data_sf)

}

define_mz <- function(data_sf, max_num_zones, min_obs) {

  #--- number of zones ---#
  num_zones <- min(floor(nrow(data_sf) / min_obs), max_num_zones)

  #--- grouping the data into 5 groups based on beta ---#
  data_sf <- data_sf %>% 
  mutate(
    zone = cut(
      b_slope, 
      breaks = quantile(b_slope, prob = seq(0, 1, length = num_zones + 1)),
      include.lowest = TRUE
    )
  ) %>% 
  mutate(zone_txt = factor(paste0("Zone ", as.numeric(zone))))

  return(data_sf)

}

#/*=================================================*/
#' # Create yield response functions by characteristic
#/*=================================================*/

# data_sf <- analysis_res$data[[1]]
# field_vars <- analysis_res$data[[1]]

make_ys_by_chars <- function(data_sf){

  #/*----------------------------------*/
  #' ## Get correlation table
  #/*----------------------------------*/
  vars_all <- names(data_sf) %>% 
    .[!str_detect(., "id")] %>% 
    .[!str_detect(., "yield")] %>% 
    .[!str_detect(., "ID")] %>% 
    .[!str_detect(., "geometry")] %>% 
    .[!str_detect(., "b_int")] %>% 
    .[!str_detect(., "gc_rate")] %>% 
    .[!str_detect(., "zone")] %>% 
    .[!str_detect(., "_rate")] %>% 
    .[!str_detect(., "opt_")] %>% 
    .[!str_detect(., "x")] %>% 
    .[. != "X"] %>% 
    .[. != "Y"]  

  drop_vars <- data_sf[, vars_all] %>% 
    st_drop_geometry() %>% 
    .[, lapply(.SD, function(x) all(is.na(x)))] %>% 
    as.matrix() %>% 
    which()

  cor_data <- data_sf[, c(vars_all, "input_rate")] %>% 
    st_drop_geometry() %>% 
    tibble() %>% 
    .[, - drop_vars] %>% 
    dplyr::select(where(is.numeric)) 

  if(ncol(cor_data) <= 1) {
    return(NULL)
  }

  cor_tab <- cor_data %>% 
    cor(use = "complete.obs") %>% 
    .[, "b_slope", drop = FALSE] %>% 
    .[!(rownames(.) %in% c("b_slope")), , drop = FALSE]

  # g_cor <- cor_tab %>%  
  #   ggcorrplot(
  #     lab = TRUE,
  #     lab_size = 7
  #   ) +
  #   theme(
  #     axis.text.x = element_text(size = 16),
  #     axis.text.y = element_text(size = 16),
  #     legend.text = element_text(size = 16)
  #   )

  #/*----------------------------------*/
  #' ## Create maps and yield response figures
  #/*----------------------------------*/
  vars_plot_ls <- rownames(cor_tab)[abs(cor_tab) >= 0.2] %>% 
    .[!str_detect(., "yield")]

  vars_plot_ls_len <- length(vars_plot_ls)

  if (vars_plot_ls_len > 0) { # if any variable > 0.2
    
    plot_ls <- list()
    map_ls <- list()

    # var_p <- "elevation"

    for (var_p in vars_plot_ls){

      temp_data <- data_sf[, c("yield", "input_rate", var_p)] %>% 
        setnames(var_p, "var_temp") %>% 
        filter(!is.na(var_temp))

      if (!str_detect(var_p, "ss")){

        temp_data <- temp_data %>% 
          mutate(
            temp_cat = cut(
              var_temp, 
              breaks = quantile(
                var_temp, 
                prob = seq(0, 1, length = 4)
              ),
              include.lowest = TRUE
            )
          )

        g_map <- ggplot(data_sf) +
          geom_sf(aes_string(fill = var_p), color = NA) +
          theme_void() +
          scale_fill_distiller(
            palette = "YlOrRd",
            direction = -1
          ) + 
          theme(
            legend.position = "bottom",
            legend.text = element_text(size = 9),
            legend.title = element_text(size = 12),
            legend.key.width =  unit(1, "cm"),
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
            plot.margin = unit(c(0, 2, 0, 0), "cm") 
          )

        map_ls[[var_p]] <- g_map

      } else {

        temp_data <- temp_data %>% 
          mutate(
            temp_cat = ifelse(var_temp < 0.5, 0, 1)
          )

      }

      plot_ls[[var_p]] <- ggplot(data = temp_data) +
        geom_point(aes(y = yield, x = input_rate, color = factor(temp_cat)), size = 0.3) +
        geom_smooth(
          aes(
            y = yield, 
            x = input_rate, 
            color = factor(temp_cat)
          ),
          method = "gam",
          formula = y ~ s(x, k = 3)
        ) +
        theme_bw() +
        scale_color_discrete(name = var_p) +
        ylab("Yield (bu/acre)") +
        xlab("Seed Rate (1000 seeds)") +
        theme_bw() +
        theme(
          legend.position = c(0.3, 0.2),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9)
        ) 

    }

    if(str_detect(var_p, "ss")){
      ssurgo_data <- readRDS("Intermediate/ssurgo.rds") %>%
        setnames(names(.), tolower(names(.)))

      ss_var <- gsub("ss_", "", var_p)

      g_map <- ggplot() +
        geom_sf(
          data = ssurgo_data,
          aes(fill = musym)
        ) + 
        theme_void()

      map_ls[["ssurgo"]] <- g_map
    }
    

    if (vars_plot_ls_len == 1){
      g_all <- plot_ls[[1]]
    } else if (vars_plot_ls_len == 2){
      g_all <- plot_ls[[1]] | plot_ls[[2]]
    } else if (vars_plot_ls_len == 3){
      g_all <- (plot_ls[[1]] | plot_ls[[2]]) / (plot_ls[[3]] | plot_spacer())
    } else if (vars_plot_ls_len >= 4){
      g_all <- (plot_ls[[1]] | plot_ls[[2]]) / (plot_ls[[3]] | plot_ls[[4]])
    }   

    if (length(map_ls) == 1){
      g_all_map <- map_ls[[1]]
    } else if (length(map_ls) == 2){
      g_all_map <- map_ls[[1]] | map_ls[[2]]
    } else if (length(map_ls) == 3){
      g_all_map <- (map_ls[[1]] | map_ls[[2]]) / (map_ls[[3]] | (ggplot() + theme_void()))
    } else if (length(map_ls) >= 4){
      g_all_map <- (map_ls[[1]] | map_ls[[2]]) / (map_ls[[3]] | map_ls[[4]])
    } 


  } else {
    g_all <- NULL
    g_all_map <- NULL
  }

  return(list(g_all, g_all_map))

}

expand_grid_df <- function(data_1, data_2) {

  expanded_data <- expand.grid(
    index_1 = seq_len(nrow(data_1)),
    index_2 = seq_len(nrow(data_2))
  ) %>% 
  tibble() %>% 
  rowwise() %>% 
  mutate(
    data = list(
      cbind(
        slice(data.table(data_1), index_1),
        slice(data.table(data_2), index_2)
      )
    )
  ) %>% 
  dplyr::select(data) %>% 
  ungroup() %>% 
  .$data %>% 
  rbindlist() %>% 
  tibble()

  return(expanded_data)

}

get_seed <- function(opt_data, c_type, w_zone){
  opt_data[type == c_type & zone_txt == paste0("Zone ", w_zone), seed_rate] %>% round(digits = 0)
}

get_pi <- function(opt_data, c_type, w_zone){
  opt_data[type == c_type & zone_txt == paste0("Zone ", w_zone), profit_hat] %>% round(digits = 2)
}

get_t_value <- function(test_data, w_zone){
  test_data[zone_txt == paste0("Zone ", w_zone), t] %>% 
    round(digits = 2)
}  

#/*=================================================*/
#' # Assign grower-chosen rate
#/*=================================================*/ 
# data <-  analysis_res$data[[1]]
# input_type <-  analysis_res$input_type[[1]]
# gc_type <-  analysis_res$gc_type[[1]]
# gc_rate <-  analysis_res$gc_rate[[1]]

assign_gc_rate <- function(data, input_type, gc_type, gc_rate) {

  if (gc_type == "uniform") {

    data$gc_rate <- gc_rate 

  } else if (gc_type == "Rx") {

    #--------------------------
    # Read Rx data
    #--------------------------
    Rx <- st_read(gc_rate) %>% 
      st_set_crs(4326) %>% 
      st_transform(st_crs(data)) %>%
      st_make_valid() %>%
      setnames(names(.), tolower(names(.)))
    

    dict_input <- dictionary[type == paste0("Rx-", tolower(input_type)), ]
    col_list <- dict_input[, column]

    Rx <- make_var_name_consistent(
      Rx, 
      dict_input 
    )

    #/*----------------------------------*/
    #' ## Unit conversion
    #/*----------------------------------*/
    if (input_type == "N") {
      Rx <- mutate(Rx, 
        tgti = convert_N_unit(
          input_data_n$form, 
          input_data_n$unit, 
          tgti, 
          field_data$reporting_unit
        ) 
        # + n_base_rate # add base N rate
      )
    } else if (input_type == "S") {
      #--- seed rate conversion ---#
      if (any(Rx$tgti > 10000)){
        #--- convert to K ---#
        Rx <- mutate(Rx, tgti = tgti / 1000)
      }
    }

    #=== map ===#
    # tm_shape(Rx) +
    #   tm_fill(col = "tgti")

    #--------------------------
    # Identify grower-chosen rate by observation
    #--------------------------
    obs_tgti <- st_intersection(data, Rx) %>% 
      mutate(area = as.numeric(st_area(.))) %>% 
      data.table() %>% 
      .[, .SD[area == max(area)], by = obs_id] %>% 
      .[, num_obs_per_zone := .N, tgti] %>% 
      .[, analyze := FALSE] %>% 
      .[num_obs_per_zone >= 200, analyze := TRUE] %>% 
      .[, .(obs_id, tgti, analyze)] 

    data <- left_join(data, obs_tgti, by = "obs_id") %>% 
      rename(gc_rate = tgti)

  }

  return(data)

}

#/*=================================================*/
#' # optimal-grower-chosen data
#/*=================================================*/ 
# data <-  analysis_res_o$data[[1]]
# pi_dif_test_zone <-  analysis_res_o$pi_dif_test_zone[[1]]
# opt_input_data <-  analysis_res_o$opt_input_data[[1]]

get_opt_gc_data <- function(data, pi_dif_test_zone, opt_input_data) {

  mean_gc_rate_by_zone <- data.table(data) %>% 
    .[, .(input_rate = mean(gc_rate)), by = zone_txt]

  gc_data <- pi_dif_test_zone %>% 
    .[, .(yhat_est_gc, point_est_gc, point_est_gc_se, zone_txt)] %>% 
    mean_gc_rate_by_zone[., on = "zone_txt"] %>% 
    setnames(
      c("yhat_est_gc", "point_est_gc", "point_est_gc_se"), 
      c("yield_hat", "profit_hat", "profit_hat_se")
    ) %>% 
    .[, `:=`(
      pi_upper = profit_hat + 1.96 * profit_hat_se,
      pi_lower = profit_hat - 1.96 * profit_hat_se
    )] %>% 
    .[, type := "gc"]

  opt_data <- pi_dif_test_zone %>% 
    .[, .(yhat_est_opt, point_est_opt, point_est_opt_se, zone_txt)] %>% 
    opt_input_data[, .(zone_txt, opt_input)][., on = "zone_txt"] %>% 
    setnames(
      c("yhat_est_opt", "point_est_opt", "point_est_opt_se", "opt_input"), 
      c("yield_hat", "profit_hat", "profit_hat_se", "input_rate")
    ) %>% 
    .[, `:=`(
      pi_upper = profit_hat + 1.96 * profit_hat_se,
      pi_lower = profit_hat - 1.96 * profit_hat_se
    )] %>% 
    .[, type := "opt_v"]

  opt_gc_data <- rbind(opt_data, gc_data)

  return(opt_gc_data)

}
    
get_whole_pi_test <- function(data, gam_res, crop_price, input_price) {

  test_data <- data.table(data) 

  whole_profits_test <- rbind(
    #=== opt (V) vs gc ===#
    get_dif_stat(
      test_data, 
      "input_rate", 
      "opt_input", 
      "gc_rate",
      gam_res,
      crop_price,
      input_price = input_price 
    ) %>% 
    .[, type := "optimal site-specific rate strategy \n vs \n grower-chosen strategy"] %>% 
    .[, type_short := "ovg"],

    #=== opt (u) vs gc ===#
    get_dif_stat(
      test_data, 
      "input_rate", 
      "opt_input", 
      "opt_input_u",
      gam_res,
      crop_price,
      input_price = input_price 
    ) %>% 
    .[, type := "optimal site-specific rate strategy \n vs \n optimal uniform rate strategy"] %>% 
    .[, type_short := "ovou"],

    #=== opt (u) vs gc ===#
    get_dif_stat(
      test_data, 
      "input_rate", 
      "opt_input_u", 
      "gc_rate",
      gam_res,
      crop_price,
      input_price = input_price 
    ) %>% 
    .[, type := "optimal uniform rate strategy \n vs \n grower-chosen strategy"] %>% 
    .[, type_short := "oug"]
  )

  return(whole_profits_test)

}

#/*=================================================*/
#' # Interactions
#/*=================================================*/

get_field_int <- function(data_sf, field_vars) {

  #=== find correlation coefs of b_slope and field vars ===#
  cor_tab <- data_sf[, c("b_slope", field_vars)] %>% 
    st_drop_geometry() %>% 
    cor(use = "complete.obs") %>% 
    .[, "b_slope", drop = FALSE] %>% 
    .[!(rownames(.) %in% c("b_slope")), , drop = FALSE] %>% 
    data.frame() %>% 
    arrange(desc(abs(b_slope)))

  #=== find variables that are correlated with b_slope higher than 0.2 ===#
  interacting_vars <- rownames(cor_tab)[abs(cor_tab) >= 0.2] %>% 
    .[!str_detect(., "yield")]

  return(list(
    cor_tab = cor_tab, 
    interacting_vars = interacting_vars
  ))

}

#/*=================================================*/
#' # Interactions and illustrative figures
#/*=================================================*/
#=== find correlation coefs of b_slope and field vars ===#

# get_inteactions_maps_ys(data, input_type, field_interactions)$g_ys_char

get_inteactions_maps_ys <- function(data, input_type, field_interactions) {

# field_interactions <- analysis_res_w$field_interactions[[1]]
# data <- analysis_res_w$data[[1]]

  cor_tab <- field_interactions$cor_tab
  interacting_vars <- field_interactions$interacting_vars

  unit_txt = case_when(
    input_type == "S" ~ "K seeds",
    input_type == "N" ~ "lbs",
    input_type == "K" ~ "lbs"
  )

  input_full_name = case_when(
    input_type == "S" ~ "Seed",
    input_type == "N" ~ "Nitrogen",
    input_type == "K" ~ "Potassium"
  )

  if (length(interacting_vars) == 0) {
    field_plots <- NULL
  } else {

# field_plots$data_plot_dt[[4]]$temp_cat %>% unique()
# field_plots$g_ys_char[[4]]

    field_plots <- tibble(
      ch_var = interacting_vars,
      data_plot = list(data)
    ) %>% 
    left_join(., field_var_data, by = c("ch_var" = "field_vars")) %>% 
    rowwise() %>% 
    mutate(g_map =
      list(
        ggplot(data_plot) +
        geom_sf(aes_string(fill = ch_var), color = NA) +
        theme_void() +
        scale_fill_distiller(
          palette = "YlGn",
          direction = -1,
          name = ""
        ) + 
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 12),
          legend.key.width =  unit(1, "cm"),
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.margin = unit(c(0, 2, 0, 0), "cm") 
        ) +
        ggtitle(str_to_title(var_txt_in_report))
      )
    ) %>% 
    mutate(data_plot_dt = 
      list(
        data_plot[, c("yield", "input_rate", ch_var)] %>% 
          st_drop_geometry() %>% 
          setnames(ch_var, "temp_var") %>% 
          data.table()  
      )
    ) %>% 
    mutate(breaks = list(
      quantile(
        data_plot_dt$temp_var, 
        prob = seq(0, 1, length = 4)
      ) %>% 
      data.table(breaks = .) %>% 
      .[1, breaks := floor(breaks)] %>% 
      .[.N, breaks := ceiling(breaks)] %>% 
      .[, breaks] %>% 
      unique()
    )) %>% 
    mutate(data_plot_dt = 
      list(
        mutate(
          data_plot_dt,
          temp_cat = cut(
            data_plot_dt$temp_var, 
            breaks = breaks,
            include.lowest = TRUE
          )
        )
      )
    ) %>% 
    mutate(g_ys_char = list(
      ggplot(data = data_plot_dt) +
      geom_point(aes(y = yield, x = input_rate, color = factor(temp_cat)), size = 0.3) +
      geom_smooth(
        aes(
          y = yield, 
          x = input_rate, 
          color = factor(temp_cat)
        ),
        method = "gam",
        formula = y ~ s(x, k = 3)
      ) +
      theme_bw() +
      scale_color_discrete(name = str_to_title(var_txt_in_report)) +
      ylab("Yield (bu/acre)") +
      xlab(
        paste0(
          input_full_name, 
          " Rate (",
          unit_txt,
          ")"
        )
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9)
      )
    )) %>% 
    dplyr::select(- data_plot, - data_plot_dt, - breaks)

  }

  return(field_plots)

}

# data <- analysis_res$data[[1]]
# data_sf <- analysis_res$data_sf[[1]]
find_field_vars <- function(data, data_sf) {
  
  #/*----------------------------------*/
  #' ## pick field vars
  #/*----------------------------------*/
  #=== keep only the ones that are available ===#
  field_var_ls <- c(
    #=== topography ===#
    # "twi", 
    "tpi", "elev", "slope", 
    "aspect_N", "aspect_NE", "aspect_E", "aspect_SE", "aspect_S", "aspect_SW", "aspect_W", "aspect_NW",
    #=== ssurgo ===#
    "curv",
    #=== ec ===#
    "ecs", "OM", "om","cec", "ec02",
    #soil_sampling"
    "P_soil","K_soil","lime","pH","ph","k_soil","p_soil","soil_p","soil_P","soil_k","soil_ca"
  ) %>% 
    .[. %in% names(data)]
  
  get_obs_true <- function(data, col){
    data %>%
      filter(eval(parse(text = paste0("`", col, "`"))) == 1) %>%
      nrow()
  }
  
  if("soil_name" %in% colnames(data)){
    soil_vars <- data %>%
      dplyr::select(colnames(.)[which(colnames(.) %in% unique(data_sf$soil_name))]) %>%
      colnames() %>%
      sapply(., get_obs_true, data = data) %>%
      as.data.frame() %>%
      setnames(., ".", "n_obs") %>%
      mutate(perc = n_obs/nrow(data)) %>%
      filter(perc > 0.1) %>%
      rownames()
    
    field_var_ls <- c(field_var_ls, soil_vars)
  }
  
  #=== find variables to keep ===#
  keep_vars <- data[, ..field_var_ls] %>% 
    # st_drop_geometry() %>% 
    #=== if missing for more than 10%, then drop ===#
    data.table() %>% 
    .[, lapply(
      .SD, 
      function(x) {
        (sum(is.na(x))/nrow(data)) < 0.1
      }
    )] %>% 
    as.matrix() %>% 
    as.vector()
  
  field_var_ls <- field_var_ls[keep_vars]
  
  return(field_var_ls)
}

# data <- analysis_res$data[[1]]
# field_vars <- analysis_res$field_vars[[1]]

train_cf_ma <- function(data, field_vars){
  macf_tau <-
    grf::multi_arm_causal_forest(
      X = eval(parse(text = paste0("data[, .(", paste0(field_vars, collapse = ", "), ")]"))) %>%
        as.matrix(.),
      Y = data[, yield] %>% as.matrix(.),
      W = data[, treat] 
      # num.threads = 1
    )
}

# trained_model <- analysis_res$cf_model[[1]]
# data <- analysis_res$data[[1]]
# field_vars <- analysis_res$field_vars[[1]]
# control <- analysis_res$control[[1]]
# treat_ls <- analysis_res$treat_ls[[1]]
# crop_price
# input_price <- analysis_res$price[[1]]
# input_type <- analysis_res$input_type[[1]]

predict_theta_cf <- function(trained_model, data, field_vars, control, treat_ls, crop_price, input_price) {
  data_new <- eval(parse(text = paste0("data[, .(", paste0(field_vars, collapse = ", "), ")]"))) %>%
      na.omit() %>%
      as.matrix()
    
  pred <- cbind(obs_id = rep(data$obs_id, times = length(treat_ls)),
                treat = rep(treat_ls, each = nrow(data)),
                prediction = predict(trained_model, data_new)[[1]][, , 1] %>%
                  as.vector()) %>%
    data.table() %>%
    mutate(profit_diff = prediction*crop_price - (treat - control)*input_price)
  
  return(pred)
}

var_import <- function(cf_model, data, field_vars){
  var_importance <- variable_importance(cf_model) %>%
    t(.) %>%
    data.frame(.) %>%
    setNames(., eval(parse(text = paste0("data[, .(", paste0(field_vars, collapse = ", "), ")]"))) %>% names(.))
  
  return(var_importance)
}

# predictions = pred
find_opt_u <- function(predictions, control, control_profit){
  opt_ur <- predictions %>%
    .[ , .(sum_prof = sum(profit_diff)), by = treat] %>%
    .[ , .SD[which.max(sum_prof)]] %>%
    .[ , treat := case_when(sum_prof <= 0 ~ control, TRUE ~ treat)] %>%
    dplyr::select(treat)
  
  return(opt_ur)
}

# predictions <- analysis_res$predict_prof[[1]]
# control <- analysis_res$control[1]
find_opt_vr <- function(predictions, control){
  opt_vr <- predictions %>%
    .[ , .SD[which.max(profit_diff)], by = obs_id] %>%
    .[ , treat := case_when(profit_diff <= 0 ~ control, TRUE ~ treat)] %>%
    dplyr::rename("EOIR" = "treat") %>%
    .[ , profit_diff := case_when(EOIR == control ~ 0, TRUE ~ profit_diff)]
  
  return(opt_vr)
}

# predictions <- analysis_res$predict_prof[[1]]
# opt_ur <- analysis_res$opt_ur[[1]]
# sq_rate <- analysis_res$gc_rate[[1]]
# control <- analysis_res$control[[1]]
# treat_ls <- analysis_res$treat_ls[[1]]

get_prof_diff_ur_gc <- function(predictions, opt_ur, sq_rate, control, treat_ls){
  
  gc_rate <- c(control, treat_ls)[which.min(abs(c(control, treat_ls) - sq_rate))]
  
  if(gc_rate == control){
    mean_profit_gc = 0
  }else{
    mean_profit_gc <- predictions %>%
    data.table(.) %>%
    .[treat == gc_rate] %>%
    .[ , .(sum_prof = mean(profit_diff, na.rm = TRUE))] 
  }
  
  if(opt_ur == control){
    mean_profit_ur = 0
  }else{
    mean_profit_ur <- predictions %>%
      data.table(.) %>%
      .[treat %in% c(opt_ur)] %>%
      .[ , .(sum_prof = mean(profit_diff, na.rm = TRUE))] 
  }
  
  ur_gc_p_diff = mean_profit_ur - mean_profit_gc
  return(ur_gc_p_diff)
}

# predictions <- analysis_res$predict_prof[[1]]
# opt_vr <- analysis_res$opt_vr[[1]]
# opt_ur <- analysis_res$opt_ur[[1]]
# sq_rate <- analysis_res$gc_rate[[1]]
# control <- analysis_res$control[[1]]

get_prof_diff_vr_ur <- function(predictions, opt_vr, opt_ur, control){
  mean_profit_vr <- opt_vr %>%
    .[ , .(mean(profit_diff, na.rm = TRUE))]
  
  if(opt_ur == control){
    mean_profit_ur = 0
  }else{
    mean_profit_ur <- predictions %>%
      data.table(.) %>%
      .[treat %in% c(opt_ur)] %>%
      .[ , .(sum_prof = mean(profit_diff, na.rm = TRUE))] 
  }
  
  vr_ur_p_diff = mean_profit_vr - mean_profit_ur
  return(vr_ur_p_diff)
}

# data_sf <- analysis_res$
# opt_vr <- analysis_res$opt_vr[[1]]
# input_type <- analysis_res$input_type[[1]]
# treat_ls <- analysis_res$treat_ls[[1]]
vr_map <- function(data_sf, opt_vr, input_type, treat_ls, control){
  map_data <- data_sf %>%
    merge(., opt_vr[, c("obs_id", "EOIR")], by = "obs_id") %>%
    mutate(EOIR = factor(EOIR, levels = c(control, treat_ls)))
  
  if (input_type == "S" || input_type == "seed"){
    map_EOIR_cf <- tm_shape(map_data) +
      tm_polygons("EOIR", title = "Optimal Seed Rate", palette = "Greens") +
      tm_layout(legend.outside = TRUE, frame = FALSE, legend.title.size = 1.2, legend.text.size = 0.8)
  }else if (input_type %in% c("n","NH3", "urea", "uan32", "uan28", "1_2_1(36)", "LAN(26)", "MAP", "1_0_0", "1_0_1", "2_3_2(22)",
                              "15_10_6", "3_0_1", "2_3_4(32)", "4_3_4(33)", "5_1_5", "Sp", "N_equiv", "24-0-0-3 UAN","chicken_manure")){
    map_EOIR_cf <- tm_shape(map_data) +
      tm_polygons("EOIR", title = "Optimal Nitrogen Rate", palette = "Greys") +
      tm_layout(legend.outside = TRUE, frame = FALSE, legend.title.size = 1.2, legend.text.size = 0.8)
    }else if (input_type %in% c("urea82")){
      map_EOIR_cf <- tm_shape(map_data) +
        tm_polygons("EOIR", title = "Optimal Nitrogen Rate", palette = "Greys") +
        tm_layout(legend.outside = TRUE, frame = FALSE, legend.title.size = 1.2, legend.text.size = 0.8)
  }else{
    map_EOIR_cf <- tm_shape(map_data) +
      tm_polygons("EOIR", title = paste0("Optimal ", str_to_title(input_type)," Rate")) +
  tm_layout(legend.outside = TRUE, frame = FALSE, legend.title.size = 1.2, legend.text.size = 0.8)
  }
  return(map_EOIR_cf)
}

#/*=================================================*/
#' # Field variable name in the report  
#/*=================================================*/
field_var_data <- c(
    #=== topography ===#
    "twi", "tpi", "elev", "slope", 
    #=== ssurgo ===#
    "muname",
    #=== ec ===#
    "ecs","om", "cec",
    #=== soil_sample===#
    "k_soil","p_soil","pH","lime"
) %>% 
data.table(field_vars = .) %>% 
.[, var_txt_in_report := c(
  "topographic wetness index", "topographic position index", "topographical elevation", "topographical slope",
  "soil type",
  "Shallow Electrical Conductivity", "Organic matter", "CEC","Potassium","Phosporous","pH","lime"
)] 

# var_import <- analysis_res$var_importance[[1]]
# vars = "Soil"

get_top_vars <- function(var_import, vars, data_sf){
  top_vars <- var_import %>%
    t() %>%
    data.frame() %>%
    arrange(desc(.)) %>%
    rownames()
  
  soils <- unique(data_sf$soil_name)
  
  if(vars == "No soil"){
    top_vars <- top_vars[which(top_vars %notin% soils)]
    top_vars <- top_vars[1:2]
  }else if(vars == "All"){
    top_vars <- top_vars[1:3]
  }else{
    top_vars <- top_vars[which(top_vars %in% soils)]
    top_vars <- top_vars[1:2] %>%
      .[!is.na(.)]
  }
  
  return(top_vars)
}

# top_vars <- analysis_res$top_var[[1]]
# opt_vr <- analysis_res$opt_vr[[1]]
# data <- analysis_res$data[[1]]
# 
get_top_vars_corr <- function(top_vars, opt_vr, data){
  top_vars[-grep('muname', top_vars)]
  
  data <- merge(data, opt_vr, by = "obs_id") 

  corrtest1 <- cor.test(eval(parse(text = paste0("data$", top_vars[1]))), data$EOIR)
  corrtest2 <- cor.test(eval(parse(text = paste0("data$", top_vars[2]))), data$EOIR)
  
  return(list(corrtest1, corrtest2))
}

# data = plot_data
# treat = "treat"
# int_var = "terc_mukey"
# output_var = "profit"

get_perc_by_group_int <- function(data, treat, int_var, output_var){
  min <- data %>%
    group_by(treat, eval(parse(text = int_var))) %>%
    summarise(quant = quantile(eval(parse(text = output_var)), c(0.25))) %>%
    pull(quant) %>%
    min(.)
  
  
  max <- data %>%
    group_by(treat, eval(parse(text = int_var))) %>%
    summarise(quant = quantile(eval(parse(text = output_var)), c(0.75))) %>%
    pull(quant) %>%
    max(.)
  
  return(c(min - 5, max + 5))
  
}

# data = plot_data
# treat = "treat"
# int_var = paste0("terc_", top_vars[1])
# output_var = "profit" 
get_mean_by_group_int <- function(data, treat, int_var, output_var){
  median <- data %>%
    group_by(treat, eval(parse(text = int_var))) %>%
    summarise(mean = round(mean(eval(parse(text = output_var))))) %>%

  return(mean)
  
}

get_perc_by_group <- function(data, treat, output_var){
  min <- data %>%
    group_by(treat) %>%
    summarise(quant = quantile(eval(parse(text = output_var)), c(0.25))) %>%
    pull(quant) %>%
    min(.)
  
  
  max <- data %>%
    group_by(treat) %>%
    summarise(quant = quantile(eval(parse(text = output_var)), c(0.75))) %>%
    pull(quant) %>%
    max(.)
  
  return(c(min - 5, max + 5))
  
}


# top_vars <- analysis_res$top_var[[1]]
# opt_vr <- analysis_res$opt_vr[[1]]
# data <- analysis_res$data[[1]]
# input_type <- analysis_res$input_type[[1]]
# input_price <- analysis_res$price[[1]]


get_trt_seq = function(data_sf, input_type){
  trt_seq <- sort(unique(eval(parse(text = paste0("data_sf$", "trt_",tolower(input_type))))))
  
  return(trt_seq)
}
# 
# trt_seq <- comp_table$trt_seq[[1]]
# input_type <- comp_table$input_type[[1]]
# gc_rate <- comp_table$gc_rate[[1]]
# unit <- comp_table$unit[[1]]

find_gc_obs <- function(perc, comp_table){
  if (nrow(comp_table) == 1){
    seq_diff = diff(comp_table$trt_seq[[1]]) %>% mean()
    range_start = seq_diff/3

    input_rate = paste0(tolower(comp_table$input_type[[1]]), "_rate")
    
    test_seq <- seq((seq_diff/2)*0.6, (seq_diff/2)*4, length = 30)
    
    for (i in test_seq) {
      subset_gc <- comp_table$data_sf[[1]] %>%
        filter(eval(parse(text = input_rate)) > comp_table$gc_rate[[1]] - i & eval(parse(text = input_rate))  < comp_table$gc_rate[[1]] + i)
      
      if ((subset_gc %>% nrow(.))/(comp_table$data_sf[[1]] %>% nrow(.)) >= perc){
        obs = subset_gc$obs_id
        break
      }
      print(i)
    }
  }else{
    obs_no <- nrow(data_sf)*perc
    
    seq_diff_1 = diff(comp_table$trt_seq[[1]]) %>% mean()
    range_start_1 = seq_diff_1/3
    
    seq_diff_2 = diff(comp_table$trt_seq[[2]]) %>% mean()
    range_start_2 = seq_diff_2/3
    
    input_rate_1 = paste0(tolower(comp_table$input_type[[1]]), "_rate")
    input_rate_2 = paste0(tolower(comp_table$input_type[[2]]), "_rate")
    
    test_seq_1 <- seq((seq_diff_1/2)*0.6, (seq_diff_1/2)*4, length = 30)
    test_seq_2 <- seq((seq_diff_2/2)*0.6, (seq_diff_2/2)*4, length = 30)
    
    for (i in 1:length(test_seq_1)) {
      subset_gc <- data_sf %>%
        filter(eval(parse(text = input_rate_1)) > comp_table$gc_rate[[1]] - test_seq_1[i] & eval(parse(text = input_rate_1))  < comp_table$gc_rate[[1]] + test_seq_1[i] ) %>%
        filter(eval(parse(text = input_rate_2)) > comp_table$gc_rate[[2]] - test_seq_2[i] & eval(parse(text = input_rate_2))  < comp_table$gc_rate[[2]] + test_seq_2[i])

      if ((subset_gc %>% nrow(.))/(data_sf %>% nrow(.)) >= perc){
        obs = subset_gc$obs_id
        break
      }
      print(i)
    }
  }
  
  return(obs)
}

find_range <- function(perc, comp_table){
  if (nrow(comp_table) == 1){
    seq_diff = diff(comp_table$trt_seq[[1]]) %>% mean()
    range_start = seq_diff/3

    input_rate = paste0(tolower(comp_table$input_type[[1]]), "_rate")
    
    test_seq <- seq((seq_diff/2)*0.6, (seq_diff/2)*4, length = 30)
    
    for (i in test_seq) {
      subset_gc <- comp_table$data_sf[[1]] %>%
        filter(eval(parse(text = input_rate)) > comp_table$gc_rate[[1]] - i & eval(parse(text = input_rate))  < comp_table$gc_rate[[1]] + i)
      
      if ((subset_gc %>% nrow(.))/(comp_table$data_sf[[1]] %>% nrow(.)) >= perc){
        obs = subset_gc$obs_id
        range <- i
        break
      }
      print(i)
      
    }
  }else{
    obs_no <- nrow(comp_table$data_sf[[1]])*perc
    
    seq_diff_1 = diff(comp_table$trt_seq[[1]]) %>% mean()
    range_start_1 = seq_diff_1/3
    
    seq_diff_2 = diff(comp_table$trt_seq[[2]]) %>% mean()
    range_start_2 = seq_diff_2/3
    
    input_rate_1 = paste0(tolower(comp_table$input_type[[1]]), "_rate")
    input_rate_2 = paste0(tolower(comp_table$input_type[[2]]), "_rate")
    
    test_seq_1 <- seq((seq_diff_1/2)*0.6, (seq_diff_1/2)*4, length = 30)
    test_seq_2 <- seq((seq_diff_2/2)*0.6, (seq_diff_2/2)*4, length = 30)
    
    for (i in 1:length(test_seq_1)) {
      subset_gc <- data_sf %>%
        filter(eval(parse(text = input_rate_1)) > comp_table$gc_rate[[1]] - test_seq_1[i] & eval(parse(text = input_rate_1))  < comp_table$gc_rate[[1]] + test_seq_1[i] ) %>%
        filter(eval(parse(text = input_rate_2)) > comp_table$gc_rate[[2]] - test_seq_2[i] & eval(parse(text = input_rate_2))  < comp_table$gc_rate[[2]] + test_seq_2[i])
      
      if ((subset_gc %>% nrow(.))/(data_sf %>% nrow(.)) >= perc){
        obs = subset_gc$obs_id
        break
      }
      print(i)
      range <- c(test_seq_1[i], test_seq_2[i])
    }
  }
  
  return(range)
}

get_perc <- function(comp_table){
  if (nrow(comp_table) == 1){
    seq_diff = diff(comp_table$trt_seq[[1]]) %>% mean()
    perc = 1/length(comp_table$trt_seq[[1]])
  }else{
    perc = 1/(length(comp_table$trt_seq[[1]])*length(comp_table$trt_seq[[2]]))
  }
  
  return(perc)
}

make_hist_comp <- function(obs, comp_table){
  if(nrow(comp_table) == 1){
    # try to match price and rate and use paste0
    input_price_calc = paste0(tolower(comp_table$input_type), "_rate*", comp_table$price)
    
    profit_data <- comp_table$data_sf[[1]] %>%
      mutate(profit = yield*crop_price - eval(parse(text = input_price_calc))) 
    
  }else{
    # try to match price and rate and use paste0
    price1 <- paste0(tolower(comp_table$input_type[[1]]), "_rate*", comp_table$price[[1]])
    price2 <- paste0(tolower(comp_table$input_type[[2]]), "_rate*", comp_table$price[[2]])
    
    profit_data <- comp_table$data_sf[[1]] %>%
      mutate(profit = yield*crop_price - eval(parse(text = price1)) - eval(parse(text = price2)))
    
  }
  
  data_graph <- profit_data %>%
    rowwise() %>%
    mutate(Group = if(obs_id %in% obs){"Status Quo Data"}else{"Trial Data"})
  
  ggplot(data_graph, aes(x = profit, fill = Group)) + 
    geom_histogram()
}

get_profit_diff <- function(data_sf, gc_obs, comp_table){
  if(nrow(comp_table) == 1){
    # try to match price and rate and use paste0
    input_price_calc = paste0(tolower(comp_table$input_type), "_rate*", comp_table$price)
    
    profit_sq <- comp_table$data_sf[[1]] %>%
      filter(obs_id %in% gc_obs) %>%
      mutate(profit = yield*crop_price - eval(parse(text = input_price_calc))) %>%
      pull(profit) %>%
      mean()
    
    profit_trial <- comp_table$data_sf[[1]] %>%
      filter(obs_id %notin% gc_obs) %>%
      mutate(profit = yield*crop_price - eval(parse(text = input_price_calc))) %>%
      pull(profit) %>%
      mean()
    
    profit_diff = profit_sq - profit_trial
  }else{
    obs <- gc_obs
    # try to match price and rate and use paste0
    price1 <- paste0(tolower(comp_table$input_type[[1]]), "_rate*", comp_table$price[[1]])
    price2 <- paste0(tolower(comp_table$input_type[[2]]), "_rate*", comp_table$price[[2]])
    
    
    profit_sq <- data_sf %>%
      filter(obs_id %in% obs) %>%
      mutate(profit = yield*crop_price - eval(parse(text = price1)) - eval(parse(text = price2))) %>%
      pull(profit) %>%
      mean()
    
    profit_trial <- data_sf %>%
      filter(obs_id %notin% obs) %>%
      mutate(profit = yield*crop_price - eval(parse(text = price1)) - eval(parse(text = price2))) %>%
      pull(profit) %>%
      mean()
    
    profit_diff = profit_sq - profit_trial
  }
  
  
  return(profit_diff)
}

map_compensation <- function(obs, data_sf){
  comp_area_map <- tm_shape(data_sf %>% filter(obs_id %notin% obs)) +
    tm_fill(col = "red") +
    tm_shape(data_sf %>% filter(obs_id %in% obs)) +
    tm_fill(col = "blue") +
    tm_layout_to_add +
    tm_add_legend(type = "symbol",
                  labels = c("Status Quo Data", "Non-Status Quo Data"),
                  col = c("blue", "red"),
                  shape = 15)
  
  return(comp_area_map)
}

`%notin%` <- Negate(`%in%`)

table_compensation <- function(comp_table, profit_diff, trial_area, gc_obs, range){
  if (nrow(comp_table) == 1){
    data.table("Status Quo Range" = paste0("[", round(max(comp_table$gc_rate[[1]] - range[1], 0)), " - ", round(comp_table$gc_rate[[1]] + range[1]), "]"),
               "Number of Status Quo Observations" = length(gc_obs),
               "Profit Difference (SQ - Trial)" = paste0("$", round_dollars(profit_diff)),
               "Acres in Trial Rates" = round(trial_area*(1 - perc)),
               "Total Compensation" = paste0("$", max(round_dollars(trial_area*(1 - perc)*profit_diff),0))
               
    ) %>%
      as.data.frame() %>%
      flextable() %>%
      set_caption(., caption = "Compensation Results", 
                  style = "Table Caption")
  }else{
    table <- data.table("Status Quo Range of Input1" = paste0("[", round(max(comp_table$gc_rate[[1]] - range[1], 0)), " - ", round(comp_table$gc_rate[[1]] + range[1]), "]"),
               "Status Quo Range of Input 2" = paste0("[", round(max(comp_table$gc_rate[[2]] - range[2], 0)), " - ", round(comp_table$gc_rate[[2]] + range[2]), "]"),
               "Number of Status Quo Observations" = length(gc_obs),
               "Profit Difference (SQ - Trial)" = paste0("$", round_dollars(profit_diff)),
               "Acres in Trial Rates" = round(trial_area*(1 - perc)),
               "Total Compensation" = paste0("$", round_dollars(trial_area*(1 - perc)*profit_diff))
               
    ) %>%
      as.data.frame()
      
    
    colnames(table) <- c(paste0("Status Quo Range of ", comp_table$input_type[[1]]),
                    paste0("Status Quo Range of ", comp_table$input_type[[2]]),
                    "Number of Status Quo Observations",
                    "Profit Difference (SQ - Trial)",
                    "Acres in Trial Rates",
                    "Total Compensation")
    
    flextable(table) %>%
      set_caption(., caption = "Compensation Results", 
                  style = "Table Caption")
  }
  
}

round_dollars <- function(x){
  format(round(x,2), nsmall = 2)
}

# top_vars <- analysis_res$top_var[[1]]
# opt_vr <- analysis_res$opt_vr[[1]]
# data <- analysis_res$data[[1]]
# input_type <- analysis_res$input_type[[1]]
# crop_price
# input_price <- analysis_res$price[[1]]
# data_sf <- analysis_res$data_sf[[1]]
# top_var <- top_vars[1]

get_top_var_fig <- function(top_var, opt_vr, input_type, crop_price, input_price, data_sf, convert){
  field_bbox <- st_bbox(data_sf)
  
  sn_length <- field_bbox["ymax"] - field_bbox["ymin"]
  ew_length <- field_bbox["xmax"] - field_bbox["xmin"]
  
  sn_ew_ratio <- sn_length / ew_length
  
  st_set_geometry(data_sf, "geometry")
  
  input_type_name <- case_when(
    input_type %in% c("NH3", "urea", "uan32", "uan28", "1_2_1(36)", "LAN(26)", "MAP", "1_0_0", "1_0_1", "2_3_2(22)",
                      "15_10_6", "3_0_1", "2_3_4(32)", "4_3_4(33)", "5_1_5", "Sp", "N_equiv", "24-0-0-3 UAN","chicken_manure") & (convert == TRUE) ~ "nitrogen",
    TRUE ~ input_type
  )
  
  plot_data_sf <- copy(data_sf)
  
  plot_data_sf <- plot_data_sf %>%
    setnames(., paste0("trt_", tolower(input_type)), "treat")
  
  if(str_detect(top_var, "aspect") == TRUE){
    direction_var <- str_remove(top_var, "aspect_")
    
    plot_data_sf <- merge(plot_data_sf, opt_vr, by = "obs_id") %>%
      rowwise() %>%
      mutate(profit = (yield*crop_price) - (as.numeric(as.character(treat))*as.numeric(input_price))) %>%
      mutate(direction = case_when(
        eval(parse(text = top_var)) == "1" ~ direction_var,
        TRUE ~ "Other Directions"))
    
    plot_data <- st_drop_geometry(plot_data_sf)
    
    table_1 <- cbind(get_mean_by_group_int(plot_data, "treat", "direction", "yield"),
                     get_mean_by_group_int(plot_data, "treat", "direction", "profit") %>%
                       pull(mean))
    
    colnames(table_1) = c("treat", 'b', "mean_yield", "mean_profit")
    
    low1 = table_1 %>%
      filter(., b == direction_var) %>%
      mutate(treat = as.numeric(as.character(treat)))
    high1 = table_1 %>%
      filter(.,b == "Other Directions") %>%
      mutate(treat = as.numeric(as.character(treat)))
    
    scale_facs = high1$mean_profit/(high1$mean_yield) 
    
    fig1 = ggplot(high1) +
      geom_bar(stat = "identity",
               aes(x = treat, y = mean_yield),
               fill='#999999',
               width = .75*(max(high1$treat)-min(high1$treat))/length(high1$treat)) +
      geom_line( aes(x = treat, y = mean_profit/max(scale_facs)), color='black') +
      geom_point(aes(x = treat, y = mean_profit/max(scale_facs)), color='black') + 
      geom_text(aes(x = treat, 
                    y = mean_profit/max(scale_facs),
                    label=paste0('$',round(mean_profit,0))),
                vjust = 2,
                color = 'black') +
      scale_y_continuous(name = "Mean Yield",
                         sec.axis = sec_axis(~.*scale_facs[1], name = paste0("Mean Profit"))) + # [$/",w_field_data$land_unit,"]"))) +
      #geom_text(aes(x = treat, y = mean_profit/max(scale_facs),label=paste0('$',round(mean_profit,0))),vjust=2,color='yellow')+
      #geom_text(vjust = -1,aes(x = treat, y = mean_yield,label=round(mean_yield,0)),color='blue')+ 
      labs(title = paste0("Mean Yields and Profit facing ", direction),
           x = paste0(str_to_title(input_type_name), " Level")) +
      theme_bw() +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
      coord_cartesian(ylim=c(min(high1$mean_profit)/max(scale_facs)/1.2, 1.01*max(high1$mean_yield))) 
    fig2 = ggplot(low1) +
      geom_bar(stat = "identity", 
               aes(x = treat, y = mean_yield),
               fill='#E69F00',
               width = .75*(max(low1$treat)-min(low1$treat))/length(low1$treat)) +
      geom_line( aes(x = treat, y = mean_profit/max(scale_facs)), color='black') +
      geom_point(aes(x = treat, y = mean_profit/max(scale_facs)), color='black') +
      geom_text(aes(x = treat, 
                    y = mean_profit/max(scale_facs),
                    label=paste0('$',round(mean_profit,0))),
                vjust = 2,
                color = 'black') +
      scale_y_continuous(name = "Mean Yield",
                         sec.axis = sec_axis(~.*scale_facs[1], name = paste0("Mean Profit"))) +# [$/",w_field_data$land_unit,"]"))) +
      #geom_text(aes(x = treat, y = mean_profit/max(scale_facs),label=paste0('$',round(mean_profit,0))),vjust=2,color='yellow')+
      #geom_text(vjust = -1,aes(x = treat, y = mean_yield,label=round(mean_yield,0)),color='red')+ 
      labs(title = paste0("Mean Yields and Profit facing other directions"),
           x = paste0(str_to_title(input_type_name), " Level")) +
      theme_bw() +
      coord_cartesian(ylim=c(min(low1$mean_profit)/max(scale_facs)/1.2, 1.01*max(low1$mean_yield))) 
    #scale_x_continuous(breaks=round(low1$treat),name="Mean of Bin Ranges for _input_full_name_here_c_ Rate (_unit_here_)") 
      scale_x_continuous(breaks=round(low1$treat), name="Means of As-Applied Ranges") 
    
    map1 <- ggplot(data = plot_data_sf,
                   aes(fill = factor(direction))) +
      geom_sf() +
      labs(fill = "Direction") +
      scale_fill_manual(values = c(direction = "#999999",
                                   "Other Directions" = "#E69F00")) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 18),
            plot.subtitle = element_text(size = 13))
    
    if(sn_ew_ratio > .8){
      designz<-"AC
                BC"
    }else{
      designz<-"#AAA#
                #BBB#
                CCCCC
                CCCCC"
    }
    fig_var1 <- fig1 + fig2 + map1 + plot_layout(design = designz)
  }else{
    if(top_var == "curv"){
      var_levels = c("Upwardly Concave", "Linearly Flat", "Upwardly Convex")
    }else if (top_var == "tpi"){
      var_levels = c("Valley", "Constant Slope", "Hilltops")
    }else{
      var_levels = c(paste0("Low ", top_var), paste0("Medium ", top_var), paste0("High ", top_var))
    }
    colors = c("#56B4E9", '#E69F00', '#999999')
    
    if(top_var %in% c("curv", "tpi")){
      plot_data_sf <- merge(plot_data_sf, opt_vr, by = "obs_id") %>%
        mutate(!!paste0("terc_", top_var) := case_when(eval(parse(text = top_var)) < -0.05 ~ var_levels[1],
                                                       eval(parse(text = top_var)) > -0.05 & eval(parse(text = top_var)) < 0.05 ~ var_levels[2],
                                                       TRUE ~ var_levels[3]))
    }else{
      plot_data_sf <- merge(plot_data_sf, opt_vr, by = "obs_id") %>%
        mutate(!!paste0("terc_", top_var) := ntile(eval(parse(text = top_var)), 3)) %>%
        mutate(!!paste0("terc_", top_var) := case_when(eval(parse(text = paste0("terc_", top_var))) == 1 ~ var_levels[1],
                                                       eval(parse(text = paste0("terc_", top_var))) == 2 ~ var_levels[2],
                                                       TRUE ~ var_levels[3]))
    }
    
    plot_data_sf <- plot_data_sf %>%
      rowwise() %>%
      mutate(profit = (yield*crop_price) - (as.numeric(as.character(treat))*as.numeric(input_price))) %>%
      mutate(!!paste0("terc_", top_var) := as.factor(eval(parse(text = paste0("terc_", top_var))))) %>%
      mutate_if(~is.factor(.) & all(levels(.) %in% var_levels), factor, levels = var_levels)
    
    plot_data <- st_drop_geometry(plot_data_sf)
    
    table_1 <- cbind(get_mean_by_group_int(plot_data, "treat", paste0("terc_", top_var), "yield"),
                     get_mean_by_group_int(plot_data, "treat", paste0("terc_", top_var), "profit") %>%
                       pull(mean))
    
    colnames(table_1) = c("treat", 'b', "mean_yield", "mean_profit")
    levels_in_data <- sort(unique(table_1$b))
    
    #### beginning of new function ####
    make_var_fig_level <- function(level){
      high1 = table_1 %>%
        filter(., b == level) %>%
        mutate(treat=as.numeric(as.character(treat)))
      
      scale_facs = high1$mean_profit/(high1$mean_yield) 
      
      fig1 = ggplot(high1) +
        geom_bar(stat = "identity",
                 aes(x = treat, y = mean_yield),
                 fill = colors[which(var_levels == level)],
                 width = .75*(max(high1$treat)-min(high1$treat))/length(high1$treat)) +
        geom_line( aes(x = treat, y = mean_profit/max(scale_facs)), color='black') +
        geom_point(aes(x = treat, y = mean_profit/max(scale_facs)), color='black') + 
        geom_text(aes(x = treat, 
                      y = mean_profit/max(scale_facs),
                      label=paste0('$',round(mean_profit,0))),
                  vjust = 2,
                  color = 'black') +
        scale_y_continuous(name = "mean yield",
                           sec.axis = sec_axis(~.*scale_facs[1], name = paste0("mean profit"))) +
        scale_x_continuous(breaks = c(round(sort(unique(high1$treat))))) +
        labs(title = paste0("Mean Yields and Profit on ", level),
             x = paste0(str_to_title(input_type_name), " Level")) +
        theme_bw() +
        coord_cartesian(ylim=c(min(high1$mean_profit)/max(scale_facs)/1.2, 1.01*max(high1$mean_yield))) 
      
      if(which(levels_in_data == level) != length(levels_in_data)){
        fig1 <- eval(parse(text = paste0("fig1 +",
                                         "theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())")))
      }else{
        fig1 = fig1
      }
    }
    
    figs <- lapply(levels_in_data, make_var_fig_level)
    
    map1 <- ggplot(data = plot_data_sf,
                   aes(fill = factor(eval(parse(text = paste0("terc_", top_var)))))) +
      geom_sf() +
      labs(fill = paste0(top_var, " level")) +
      scale_fill_manual(values = colors) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 18),
            plot.subtitle = element_text(size = 13))
    
    if(sn_ew_ratio > .8){
      if(length(levels_in_data) == 3){
        designz <- "AD
                    BD
                    CD"
        fig_var1 <- figs[[1]] + figs[[2]] + figs[[3]] + map1 + plot_layout(design = designz)
        
      }else if(length(levels_in_data) == 2){
        designz<-"AC
                  BC"
        fig_var1 <- figs[[1]] + figs[[2]] + map1 + plot_layout(design = designz)
      }else{
        designz<-"AB"
        fig_var1 <- figs[[1]] + map1 + plot_layout(design = designz)
      }
    }else{
      if(length(levels_in_data) == 3){
        designz<-"#AAA#
                  #BBB#
                  #CCC#
                  DDDDD
                  DDDDD
                  DDDDD"  
        fig_var1 <- figs[[1]] + figs[[2]] + figs[[3]] + map1 + plot_layout(design = designz)
      }else if(length(levels_in_data) == 2){
        designz <- "#AAA#
                    #AAA#
                    #BBB#
                    #BBB#
                    CCCCC
                    CCCCC
                    CCCCC"  
        fig_var1 <- figs[[1]] + figs[[2]] + map1 + plot_layout(design = designz)
      }else{
        designz<-"#AAA#
                  BBBBB
                  BBBBB"
        
        fig_var1 <- figs[[1]] + map1 + plot_layout(design = designz)
        
      }
    }
    
  }
  
  return(fig_var1)
}

# top_soils <- analysis_res$top_soils[[1]]
# opt_vr <- analysis_res$opt_vr[[1]]
# data <- analysis_res$data[[1]]
# input_type <- analysis_res$input_type[[1]]
# crop_price
# input_price <- analysis_res$price[[1]]
# data_sf <- analysis_res$data_sf[[1]]

get_top_soil_fig <- function(top_soils, opt_vr, input_type, crop_price, input_price, data_sf, convert){
  field_bbox <- st_bbox(data_sf)
  
  #dropped soil is always the first soil type alphabetically
  dropped_soil <- sort(unique(data_sf$soil_name))[1]
  
  if(length(unique(data_sf$soil_name)) > 3){
    all_soils <- unique(data_sf$soil_name)
    
    others <- all_soils[which(all_soils %notin% c(top_soils, dropped_soil))] %>%
      c(., dropped_soil)
    
    others <- unique(data_sf$soil_name)[which(all_soils %in% others)]
    
    data_sf <- data_sf %>%
      mutate(soil_name_fig = case_when(
        soil_name %in% others ~ "Other Soils",
        TRUE ~ soil_name
      )) %>%
      mutate(soil_name_fig = gsub("_", " ", soil_name_fig))
  }else{
    data_sf <- data_sf %>%
      mutate(soil_name_fig = gsub("_", " ", soil_name))
    others <- dropped_soil
  }
  
  data_sf <- data_sf %>%
    mutate(soil_name_map = gsub("_", " ", soil_name))
  
  sn_length <- field_bbox["ymax"] - field_bbox["ymin"]
  ew_length <- field_bbox["xmax"] - field_bbox["xmin"]
  
  sn_ew_ratio <- sn_length / ew_length
  
  st_set_geometry(data_sf, "geometry")
  
  input_type_name <- case_when(
    input_type %in% c("NH3", "urea", "uan32", "uan28", "1_2_1(36)", "LAN(26)", "MAP", "1_0_0", "1_0_1", "2_3_2(22)",
                      "15_10_6", "3_0_1", "2_3_4(32)", "4_3_4(33)", "5_1_5", "Sp", "N_equiv", "24-0-0-3 UAN","chicken_manure") & (convert == TRUE) ~ "nitrogen",
    TRUE ~ input_type
  )
  
  plot_data_sf <- copy(data_sf)
  
  plot_data_sf <- plot_data_sf %>%
    setnames(., paste0("trt_", tolower(input_type)), "treat")
  
  plot_data_sf <- merge(plot_data_sf, opt_vr, by = "obs_id") %>%
    rowwise() %>%
    mutate(profit = (yield*crop_price) - (as.numeric(as.character(treat))*as.numeric(input_price)))
  
  soil_perc = plot_data_sf %>%
    group_by(soil_name_fig) %>%
    summarize(count = n()) %>%
    mutate(percent = paste0(round(count/sum(count)*100), "%"))
  
  plot_data <- st_drop_geometry(plot_data_sf)
  
  # make the map first to be able to identify the colors
  cbPalette <- viridis::viridis(length(unique(plot_data$soil_name)))
  
  map1 <- ggplot(data = plot_data_sf,
                 aes(fill = factor(soil_name_map))) +
    geom_sf() +
    labs(fill = "Soil Type") +
    scale_fill_manual(values = cbPalette,
                      guide = guide_legend(
                        direction = "vertical",
                        ncol = 2)) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          # plot.title = element_text(size = 18),
          # plot.subtitle = element_text(size = 13),
          legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size = 10), #change legend title font size
          legend.text = element_text(size = 8))
  
  ggplot_map_info <- ggplot_build(map1)
  
  soil_color_info <- data.frame(color = ggplot_map_info$plot$scales$scales[[1]]$palette.cache,
                                soil =   c(ggplot_map_info$plot$scales$scales[[1]]$range$range, rep(NA, length(ggplot_map_info$plot$scales$scales[[1]]$palette.cache) - length(ggplot_map_info$plot$scales$scales[[1]]$range$range))))
  
  # soil = list(others)
  make_soil_fig <- function(soil, plot_data_sf, soil_color_info, other){
    if(is.list(soil) == TRUE){
      soil = unlist(soil)
    }
    
    table_1 <- cbind(plot_data_sf %>%
                       filter(soil_name %in% soil) %>%
                       group_by(treat) %>%
                       summarise(mean_yield = round(mean(yield))) %>%
                       mutate(soil = plot_data %>%
                                filter(soil_name %in% soil) %>%
                                pull(soil_name_fig) %>%
                                unique()),
                     plot_data_sf %>%
                       filter(soil_name %in% soil) %>%
                       group_by(treat) %>%
                       summarise(mean_profit = round(mean(profit))) %>%
                       pull(mean_profit)) %>%
      data.frame() %>%
      dplyr::select(-c(geometry))
    
    colnames(table_1) = c("treat", "mean_yield", "soil", "mean_profit")
    
    high1 = table_1 %>%
      mutate(treat = as.numeric(as.character(treat)))
    
    scale_facs = high1$mean_profit/(high1$mean_yield) 
    
    soil_name = table_1$soil[1] %>%
      gsub("_", " ", .)
    
    # change for when other = TRUE
    if(other == FALSE){
      soil_color = soil_color_info %>%
        filter(soil == soil_name) %>%
        pull(color)
    }else if (length(soil) > 1){
      soil_color = '#999999'
    }else{
      soil_color = soil_color_info %>%
        filter(soil == soil_name) %>%
        pull(color)
    }
    perc = soil_perc %>%
      filter(soil_name_fig == soil_name) %>%
      pull(percent)
    
    fig1 = ggplot(high1) +
      geom_bar(stat = "identity",
               aes(x = treat, y = mean_yield),
               fill = soil_color,
               width = .75*(max(high1$treat)-min(high1$treat))/length(high1$treat)) +
      geom_line( aes(x = treat, y = mean_profit/max(scale_facs)), color=eval(neg_col(soil_color)))+
      geom_point(aes(x = treat, y = mean_profit/max(scale_facs)), color=eval(neg_col(soil_color))) + 
      geom_text(aes(x = treat, 
                    y = mean_profit/max(scale_facs),
                    label=paste0('$',round(mean_profit,0))),
                vjust = 2,
                color = eval(neg_col(soil_color))) +
      scale_y_continuous(name = "Mean Yield",
                         sec.axis = sec_axis(~.*scale_facs[1], name = paste0("Mean Profit"))) +
      scale_x_continuous(breaks = c(round(sort(unique(high1$treat))))) +

      labs(title = paste0("Mean Yields and Profit on ", soil_name),
           subtitle = paste0("(", perc, " of field)"),
           x = paste0(str_to_title(input_type_name), " Level")) +
      theme_bw() +
      coord_cartesian(ylim=c(min(high1$mean_profit)/max(scale_facs)/1.2, 1.01*max(high1$mean_yield))) 
    
    if(other == FALSE){
      fig1 <- eval(parse(text = paste0("fig1 +",
                                       "theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())")))
    }
    
    return(fig1)
  }
  
  soil_figs <- lapply(top_soils, make_soil_fig, plot_data_sf = plot_data_sf, soil_color_info = soil_color_info, other = FALSE)
  soil_figs_other <- lapply(list(others), make_soil_fig, plot_data_sf = plot_data_sf, soil_color_info = soil_color_info, other = TRUE)
  soil_figs <- c(soil_figs, soil_figs_other)
  
  if(sn_ew_ratio > .8){
    if(length(soil_figs) > 2){
      designz<-"AD
                BD
                CD"
      # note that A,B, C, D represent the 1st, 2nd, 3rd,and 4th elements of the following plot layout
      # the above design visually displays the space each element will take up in relation to others
      fig_var1 <- soil_figs[[1]] + soil_figs[[2]] + soil_figs[[3]] + map1 + plot_layout(design = designz)
      
    }else if( length(soil_figs)== 2){
      designz<-"AC
                BC"
      fig_var1<-soil_figs[[1]] + soil_figs[[2]] + map1 + plot_layout(design = designz)
    }else if( length(soil_figs) == 1){   
      
      designz<-"AB"
      fig_var1 <- soil_figs[[1]] + map1 + plot_layout(design = designz)
    }else{
      fig_var1=map1
    }
  }else{
    if(length(soil_figs) > 2){
      designz<-"#AAA#
                #BBB#
                #CCC#
                DDDDD
                DDDDD
                DDDDD"  
      fig_var1<-soil_figs[[1]] + soil_figs[[2]] + soil_figs[[3]] + map1 + plot_layout(design = designz)
    }else if( length(soil_figs)== 2){
    designz<-"#AAA#
              #AAA#
              #BBB#
              #BBB#
              CCCCC
              CCCCC
              CCCCC"  
      fig_var1 <- soil_figs[[1]] + soil_figs[[2]] + map1 + plot_layout(design = designz)
    }else if( length(soil_figs)==1){   
    designz<-"#AAA#
              BBBBB
              BBBBB"
      
      fig_var1 <- soil_figs[[1]] + map1 + plot_layout(design = designz)
      
    }else{
      fig_var1<=map1
    }
  }
  
  return(fig_var1)
}
neg_col=function(color){
  if(!exists('color')|| color=='#999999'){
    neg='black'}
  else{
    neg=rgb(255-t(col2rgb(color)),maxColorValue = 255)
  }
  return(neg)
}

