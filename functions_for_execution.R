# /*=================================================*/
#' # Non experimental data processing and reporting
# /*=================================================*/

non_exp_process_make_report <- function(ffy, rerun = FALSE, locally_run = FALSE) {
  
  library(knitr)
  options(knitr.duplicate.label = "allow")
  
  print(paste0("Proessing non-experiment data for ", ffy))
  
  source(here("Codes/DIFM/functions/unpack_field_parameters.R"))
  
  boundary_file <- here("Data", "Growers", ffy) %>%
    file.path(., "Raw/boundary.shp")
  
  # if (!file.exists(boundary_file)) {
  #   return(print("No boundary file exists."))
  # }
  
  #--- read in the template ---#
  nep_rmd <- read_rmd("DataProcessing/data_processing_template.Rmd", locally_run = locally_run)
  
  if (rerun) {
    #--- remove cached files ---#
    list.files(
      file.path(here(), "Data", "Growers", ffy, "DataProcessingReport"),
      full.names = TRUE
    ) %>%
      .[str_detect(., "report_non_exp")] %>%
      .[str_detect(., c("cache|files"))] %>%
      unlink(recursive = TRUE)
  }
  
  #--- topography data ---#
  topo <- NULL
  topo_file <- file.path(here("Data", "Growers", ffy), "Intermediate/topography.rds")
  
  if (!file.exists(topo_file)) {
    ne01 <- read_rmd("DataProcessing/ne01_topography.Rmd", locally_run = locally_run)
  } else {
    ne01 <- read_rmd("DataProcessing/ne01_topography_show.Rmd", locally_run = locally_run)
  }
  
  nep_rmd_t <- c(nep_rmd, ne01)
  
  #--- soil data ---#
  soils <- NULL
  
  if ((soil_source == "SSURGO")) {
    ne02 <- read_rmd("DataProcessing/ne02_ssurgo.Rmd", locally_run = locally_run)
  } else {
    ne02 <- read_rmd("DataProcessing/ne02_quebec.Rmd", locally_run = locally_run)
  }
  
  nep_rmd_ts <- c(nep_rmd_t, ne02)
  
  #--- Weather data ---#
  weather_file <- file.path(here("Data", "Growers", ffy), "Intermediate/weather_daymet.rds")
  
  if (!file.exists(weather_file)) {
    ne03 <- read_rmd("DataProcessing/ne03_weather.Rmd", locally_run = locally_run)
  } else {
    # ne03 <- read_rmd("DataProcessing/ne03_weather_show.Rmd", locally_run = locally_run)
    ne03 <- NULL
  }
  
  nep_rmd_tsw <- c(nep_rmd_ts, ne03)
  
  #--- soil data ---#
  #soil_exists <- field_data[field_year == ffy, ec]
  soil_sample<-NULL
  soil_raw_file <- file.path(here::here("Data", "Growers", ffy), "Raw/soil_sampling.shp")
  soil_file <- file.path(here::here("Data", "Growers", ffy), "Intermediate/ec.rds")
  
  if (file.exists(soil_file)) {
    ne05 <- read_rmd("DataProcessing/ne05_soil_sampling.Rmd", locally_run = locally_run)
  } else {
    
    farm_field <- paste0(str_split(ffy,"_")[[1]][1],"_",str_split(ffy,"_")[[1]][2])
    
    for (i in seq(2023, 2016, by = -1)) {
      ffi <- paste0(farm_field, "_", i)
      soil_year <- file.path(here("Data/Growers", ffi, "Raw/soil_sampling.shp"))
      
      if (file.exists(soil_year)) {
        
        ne05 <- read_rmd("DataProcessing/ne05_soil_sampling.Rmd", locally_run = locally_run)
        break
      } else {
        # if soil.shp does not exist
        print("This field either does not have soil data or soil data has not been uploaded in the right place")
        ne05 <- NULL
      }
    }
  }
  nep_rmd_tsw <- c(nep_rmd_tsw, ne05)
  
  #--- EC data ---#
  #ec_exists <- field_data[field_year == ffy, ec]
  ec<-NULL
  ec_raw_file <- file.path(here::here("Data", "Growers", ffy), "Raw/ec.shp")
  ec_file <- file.path(here::here("Data", "Growers", ffy), "Intermediate/ec.rds")
  
  if (file.exists(ec_file)) {
    ne04 <- read_rmd("DataProcessing/ne04_ec_show.Rmd", locally_run = locally_run)
  } else {
    
    farm_field <- paste0(str_split(ffy,"_")[[1]][1],"_",str_split(ffy,"_")[[1]][2])
    
    for (i in seq(2023, 2016, by = -1)) {
      ffi <- paste0(farm_field, "_", i)
      ec_year <- file.path(here("Data/Growers", ffi, "Raw/ec.shp"))
      
      if (file.exists(ec_year)) {
        
        ne04 <- read_rmd("DataProcessing/ne04_ec.Rmd", locally_run = locally_run)
        break
      } else {
        # if ec.shp does not exist
        print("This field either does not have EC data or EC data has not been uploaded in the right place")
        ne04 <- NULL
      }
    }
  }
  
  nep_rmd_tswe <- c(nep_rmd_tsw, ne04) %>% 
    gsub("field-year-here", ffy, .) %>% 
    gsub("title-here", "Non-experiment Data Processing Report", .)
  
  nep_rmd_tswe <- c(nep_rmd_tsw, ne04) %>% 
    gsub("field-year-here", ffy, .) %>% 
    gsub("title-here", "Non-experiment Data Processing Report", .)
  
  # /*----------------------------------*/
  #' ## Write out the rmd and render
  # /*----------------------------------*/
  nep_report_rmd_file_name <- file.path(here(), "Data/Growers", ffy, "DataProcessingReport/dp_report_non_exp.Rmd")
  
  writeLines(nep_rmd_tswe, con = nep_report_rmd_file_name)
  
  #--- render ---#
  render(nep_report_rmd_file_name)
  
}

# /*=================================================*/
#' # Experiment data processing and reporting
# /*=================================================*/

exp_process_make_report <- function(ffy, rerun = FALSE, locally_run = TRUE) {
  
  library(knitr)
  options(knitr.duplicate.label = "allow")
  
  cat(paste0("============================================\n= Processing experiment data for ", ffy, 
             "\n============================================")
  )
  #--- define field parameters ---#
  source(here("Codes/DIFM/Functions/unpack_field_parameters.R"))
  
  exp_temp_rmd <- read_rmd(
    "DataProcessing/data_processing_template.Rmd", 
    locally_run = locally_run
  )
  
  e01 <- read_rmd(
    "DataProcessing/e01_gen_yield_polygons.Rmd", 
    locally_run = locally_run
  )
  
  exp_rmd_y <- c(exp_temp_rmd, e01)
  
  #/*----------------------------------*/
  #' ## Rmd(s) for input processing
  #/*----------------------------------*/
  e02 <- trial_info %>% 
    rowwise() %>% 
    mutate(
      e02_rmd = list(
        prepare_e02_rmd(
          input_type, 
          process, 
          use_td
        )
      )
    ) %>% 
    data.table() %>% 
    .[, e02_rmd] %>% 
    reduce(c)
  
  exp_rmd_yi <- c(exp_rmd_y, e02)
  
  #/*----------------------------------*/
  #' ## Merge yield and input data
  #/*----------------------------------*/
  e03 <- read_rmd(
    "DataProcessing/e03_yield_input_integration.Rmd", 
    locally_run = locally_run
  )
  
  #/*----------------------------------*/
  #' ## Personalize the report 
  #/*----------------------------------*/
  exp_rmd_yiy <- c(exp_rmd_yi, e03) %>% 
    gsub("field-year-here", ffy, .) %>% 
    gsub("title-here", "Experiment Data Processing Report", .) %>% 
    gsub("trial-type-here", trial_type, .)
  
  #/*=================================================*/
  #' # Remove cached files if rerun == TRUE
  #/*=================================================*/  
  if (rerun) {
    #--- remove cached files ---#
    list.files(
      file.path(here(), "Data", "Growers", ffy, "DataProcessingReport"),
      full.names = TRUE
    ) %>%
      .[str_detect(., "report_exp")] %>%
      .[str_detect(., c("cache|files"))] %>%
      unlink(recursive = TRUE)
  }
  
  #/*=================================================*/
  #' # Write out the rmd and render
  #/*=================================================*/
  exp_report_rmd_file_name <- here(
    "Data/Growers", 
    ffy, 
    "DataProcessingReport/dp_report_exp.Rmd"
  )
  
  exp_report_r_file_name <- here(
    "Data/Growers", 
    ffy, 
    "DataProcessingReport/for_debug.R"
  )
  
  writeLines(exp_rmd_yiy, con = exp_report_rmd_file_name)
  
  purl(exp_report_rmd_file_name, output = exp_report_r_file_name)
  
  render(exp_report_rmd_file_name)
  
}

# /*=================================================*/
#' # Final data processing and reporting
# /*=================================================*/
f_process_make_report <- function(ffy, rerun = FALSE, locally_run = TRUE) {
  
  library(knitr)
  options(knitr.duplicate.label = "allow")
  
  # /*----------------------------------*/
  #' ## Experiment data processing
  # /*----------------------------------*/
  # fp_temp_rmd <- here() %>%
  #   file.path(., "Codes/DataProcessing/data_processing_template.Rmd") %>%
  #   readLines() %>%
  #   gsub("field-year-here", ffy, .)
  source(here("Codes/DIFM/Functions/unpack_field_parameters.R"))
  
  fp_temp_rmd <- read_rmd("DataProcessing/data_processing_template.Rmd", locally_run = locally_run) %>%
    gsub("field-year-here", ffy, .)
  
  # f01_rmd <- readLines(file.path(here(), "Codes/DataProcessing/f01_combine_all_datasets.Rmd"))
  
  f01_rmd <- read_rmd("DataProcessing/f01_combine_all_datasets.Rmd", locally_run = locally_run)
  
  fp_rmd <- c(fp_temp_rmd, f01_rmd)
  
  if (rerun) {
    #--- remove cached files ---#
    list.files(
      file.path(here(), "Data", "Growers", ffy, "DataProcessingReport"),
      full.names = TRUE
    ) %>%
      .[str_detect(., "final")] %>%
      .[str_detect(., "cache|files")] %>%
      unlink(recursive = TRUE)
  }
  
  # /*----------------------------------*/
  #' ## Data availability check (EC, soil sampling)
  # /*----------------------------------*/
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### topography data
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  topo_exist <- file.path(here("Data", "Growers", ffy), "Intermediate/topography.rds") %>%
    file.exists()
  
  if (topo_exist) {
    fp_rmd <- gsub("topo_eval_here", TRUE, fp_rmd)
  } else {
    fp_rmd <- gsub("topo_eval_here", FALSE, fp_rmd)
  }
  
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Soil data
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  soils_exist <- file.path(here("Data", "Growers", ffy), "Intermediate/soils.rds") %>%
    file.exists()
  
  if (soils_exist) {
    fp_rmd <- gsub("ssurgo_eval_here", TRUE, fp_rmd)
  } else {
    fp_rmd <- gsub("ssurgo_eval_here", FALSE, fp_rmd)
  }
  
  
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### OM
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  
  om_file <- file.path(here("Data", "Growers", ffy), "Intermediate/om.rds")
  
  if (file.exists(om_file)) {
    fp_rmd <- gsub("om_eval_here", TRUE, fp_rmd)
  } else {
    fp_rmd <- gsub("om_eval_here", FALSE, fp_rmd)
  }
  
  
  
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### EC
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  ec_exists <- w_field_data[field_year == ffy, ec == "received"]
  ec_file <- file.path(here("Data", "Growers", ffy), "Intermediate/ec.rds")
  
  if (ec_exists & file.exists(ec_file)) {
    fp_rmd <- gsub("ec_eval_here", TRUE, fp_rmd)
  } else {
    fp_rmd <- gsub("ec_eval_here", FALSE, fp_rmd)
  }
  
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Soil sampling
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  gss_exists <- w_field_data[field_year == ffy, soil_sampling == "received"]
  gss_file <- file.path(here("Data", "Growers", ffy), "Intermediate/gss.rds")
  
  if (gss_exists & file.exists(gss_file)) {
    fp_rmd <- gsub("gss_eval_here", TRUE, fp_rmd)
  } else {
    fp_rmd <- gsub("gss_eval_here", FALSE, fp_rmd)
  }
  
  # /*----------------------------------*/
  #' ## Write out the rmd and render
  # /*----------------------------------*/
  exp_report_rmd_file_name <- "DataProcessingReport/final_processing_report.Rmd" %>%
    file.path(here(), "Data", "Growers", ffy, .) %>%
    paste0(.)
  
  writeLines(fp_rmd, con = exp_report_rmd_file_name)
  
  render(exp_report_rmd_file_name)
}


#/*=================================================*/
#' # Run analysis
#/*=================================================*/
run_analysis <- function(ffy, rerun = FALSE, locally_run = TRUE){
  
  library(knitr)
  options(knitr.duplicate.label = "allow")
  
  data_for_analysis_exists <- here("Data", "Growers", ffy, "Analysis-Ready", "analysis_data.rds") %>% 
    file.exists()
  
  if (!data_for_analysis_exists) {
    return(print("No data that is ready for analysis exist. Process the datasets first."))
  }
  
  if (rerun){
    #--- remove cached files ---#
    list.files(
      file.path(here(), "Reports/Growers", ffy),
      full.names = TRUE
    ) %>% 
      .[str_detect(., "analysis")] %>% 
      .[str_detect(., "cache|files")] %>% 
      unlink(recursive = TRUE)
  }
  
  analysis_rmd <- read_rmd(
    "Analysis/a01_analysis.Rmd", 
    locally_run = locally_run
  ) %>% 
    gsub("field-year-here", ffy, .)
  
  #/*----------------------------------*/
  #' ## Save and run
  #/*----------------------------------*/
  analysis_rmd_file_name <- here() %>% 
    paste0(., "/Reports/Growers/", ffy, "/analysis.Rmd")
  
  writeLines(analysis_rmd, con = analysis_rmd_file_name)
  
  render(analysis_rmd_file_name)
  
}

#/*=================================================*/
#' # Make report (run after run_analysis)
#/*=================================================*/
make_grower_report <- function(ffy, rerun = TRUE, locally_run = TRUE){
  
  library(knitr)
  options(knitr.duplicate.label = "allow")
  
  source(here("Codes/DIFM/Functions/unpack_field_parameters.R"))
  
  #/*----------------------------------*/
  #' ## If rerun = TRUE
  #/*----------------------------------*/
  if (rerun){
    #--- remove cached files ---#
    list.files(
      file.path(here(), "Reports/Growers", ffy),
      full.names = TRUE
    ) %>% 
      .[str_detect(., "grower-report")] %>% 
      .[str_detect(., c("cache|files"))] %>% 
      unlink(recursive = TRUE)
  }
  
  results <- readRDS(here("Reports", "Growers", ffy, "analysis_results.rds"))
  
  treat_ls <- results$treat_ls[[1]]
  opt_vr <- results$opt_vr[[1]]
  control <- results$control[[1]]
  opt_ur <- results$opt_ur[[1]]
  gc_rate <- results$gc_rate[[1]]
  
  base_rmd <- readLines(here("Codes/DIFM/Report", "r00_report_header.Rmd"))
  
  results_gen_rmd <- readLines(here("Codes/DIFM/Report", "r01_gen_results.Rmd"))  
  
  report_rmd_ls <- results %>% 
    mutate(
      report_rmd = list(c(base_rmd, results_gen_rmd))  
    ) %>% 
    rowwise() %>% 
    mutate(
      unit_txt = case_when(
        input_type == "seed" ~ "K seeds",
        input_type %in% c("NH3", "urea", "uan32", "uan28", "1_2_1(36)", "LAN(26)", "MAP", "1_0_0", "1_0_1", "2_3_2(22)",
                          "15_10_6", "3_0_1", "2_3_4(32)", "4_3_4(33)", "5_1_5", "Sp", "N_equiv", "24-0-0-3 UAN","chicken_manure") & (convert == TRUE) ~ "lbs",
        input_type == "K" ~ "lbs",
        TRUE ~ unit
      )
    ) %>% 
    mutate(
      input_full_name = case_when(
        input_type == "seed" ~ "Seed",
        input_type %in% c("NH3", "urea", "uan32", "uan28", "1_2_1(36)", "LAN(26)", "MAP", "1_0_0", "1_0_1", "2_3_2(22)",
                          "15_10_6", "3_0_1", "2_3_4(32)", "4_3_4(33)", "5_1_5", "Sp", "N_equiv", "24-0-0-3 UAN","chicken_manure") & (convert == TRUE) ~ "Nitrogen",
        input_type == "K" ~ "Potassium",
        TRUE ~ input_type
      )
    ) %>% 
    mutate(
      report_body = list(
        readLines(here("Codes/DIFM/Report", "r01_make_report.Rmd"))
      )
    ) %>% 
    mutate(
      report_rmd = list(
        c(report_rmd, report_body)
      )
    )  %>% 
    mutate(
      write_file_name = list(
        here(
          "Reports/Growers", ffy, 
          paste0("grower-report-", tolower(input_type), ".Rmd")
        )
      )
    ) %>% 
    mutate(
      report_rmd = list(
        report_rmd %>% 
          gsub(
            "_status_quo_rate_statement_here_", 
            case_when(
              gc_type == "uniform" ~ "`r round(_gc_rate_here_, digits = 0)` _unit_here_/ac",
              gc_type == "Rx" ~ "commercial Rx (see the map below)"
            ), 
            .
          ) %>% 
          str_replace_all("_unit_here_", unit_txt) %>% 
          str_replace_all("_crop_unit_", crop_unit) %>% 
          str_replace_all("_input_full_name_here_c_", input_full_name) %>% 
          str_replace_all("_input_type_here_", input_type) %>% 
          str_replace_all("_field-year-here_", ffy) %>%  
          str_replace_all("_gc_rate_here_", as.character(gc_rate)) %>% 
          str_replace_all("_var1-here_", as.character(results$top_var[[1]][1])) %>% 
          str_replace_all("_var2-here_", as.character(results$top_var[[1]][2])) %>% 
          str_replace_all(
            "_land_unit_",
            case_when(
              land_unit == "ac" ~ "ac",
              land_unit == "ha" ~ "ha"
            )
          ) %>%
          str_replace_all(
            "_land_unitlong_",
            case_when(
              land_unit == "ac" ~ "acre",
              land_unit == "ha" ~ "hectare"
            )
          ) %>%
          str_replace_all(
            "_length_unit_",
            case_when(
              reporting_unit == "imperial" ~ "ft",
              reporting_unit != "metric" ~ "m"
            )
          ) %>%
          str_replace_all(
            "_planter-applicator-here_",
            case_when(
              input_type == "seed" ~ "planter",
              input_type != "seed" ~ "applicator"
            )
          ) %>% 
          str_replace_all(
            "_planter-applicator-here-c_",
            case_when(
              input_type == "seed" ~ "Planter",
              input_type != "seed" ~ "Applicator"
            )
          ) %>%           str_replace_all(
            "_asa-or-asp_",
            case_when(
              input_type == "seed" ~ "as-planted",
              input_type != "seed" ~ "as-applied"
            )
          ) %>% 
          str_replace_all(
            "_applying-or-planting_",
            case_when(
              input_type == "seed" ~ "planting",
              input_type != "seed" ~ "applying"
            )
          ) %>% 
          str_replace_all(
            "_apply-or-plant_",
            case_when(
              input_type == "seed" ~ "plant",
              input_type != "seed" ~ "apply"
            )
          ) %>%
          str_replace_all(
            "_seeding-or-application_",
            case_when(
              input_type == "seed" ~ "seeding",
              input_type != "seed" ~ "_input_full_name_l_ application"
            )
          )%>%
          str_replace_all(
            "_secion_num_", 
            as.character(section_num)
          )%>%
          str_replace_all(
            "_input_plot_width_", 
            as.character(trial_width)
          )%>% 
          str_replace_all(
            "_input_full_name_l_", 
            tolower(input_full_name)
          ) %>% 
          str_replace_all(
            "_crop_type_here_", 
            case_when(
              crop == "soy" ~ "soybean",
              crop != "soy" ~ crop
            )
          ) %>% 
          str_replace_all(
            "_crop_type_c_here_", 
            str_to_title(crop)
          ) %>% 
          str_replace_all(
            "_area_of_optimal_", 
            case_when(
              future_recs(treat_ls, control, opt_vr) == "decreased" ~ "at or near the bottom of",
              future_recs(treat_ls, control, opt_vr) == "increased" ~ "at or near the top of",
              future_recs(treat_ls, control, opt_vr) == "similar" ~ "within",
              future_recs(treat_ls, control, opt_vr) == "expanded" ~ "across all of"
              
                )
            ) %>% 
          str_replace_all(
            "_lower_higher_future_", 
            case_when(
              future_recs(treat_ls, control, opt_vr) == "decreased" ~ "lower",
              future_recs(treat_ls, control, opt_vr) == "increased" ~ "higher",
              future_recs(treat_ls, control, opt_vr) == "similar" ~ "similar",
              future_recs(treat_ls, control, opt_vr) == "expanded" ~ "expanded"
              
            )
          ) %>% 
          str_replace_all(
            "_fut_trial_text_", 
            case_when(
              future_recs(treat_ls, control, opt_vr) == "decreased" ~ " to examine whether rates even lower than the lowest rate of the 2023 trial might be economically optimal in some part of the field.",
              future_recs(treat_ls, control, opt_vr) == "increased" ~ " to examine whether rates even higher than the highest rate of the 2023 trial might be economically optimal in some part of the field.",
              future_recs(treat_ls, control, opt_vr) == "similar" ~ ".",
              future_recs(treat_ls, control, opt_vr) == "expanded" ~ " to examine whether rates even higher or lower than the 2023 rates might be economically optimal in some part of the field."
            )
          ) %>% 
          str_replace_all(
            "_uniform_Rx_1_", 
            case_when(
              gc_type == "Rx" ~ "the commercial variable-rate prescription",
              gc_type != "Rx" ~ "the uniform rate"
            )
          ) %>%
          str_replace_all(
            "_uniform_Rx_2_", 
            case_when(
              gc_type == "Rx" ~ "commercial Rx",
              gc_type != "Rx" ~ "grower’s status quo management plan"
            )
          ) %>%
          str_replace_all(
            "_uniform_Rx_3_", 
            case_when(
              gc_type == "Rx" ~ "commercial Rx",
              gc_type != "Rx" ~ "grower’s status quo"
            )
          )%>%
          str_replace_all(
            "_RX_notes_here_", 
            case_when(
              gc_type == "Rx" ~ "(the average of the Rx rates within the zone)",
              gc_type != "Rx" ~ " "
            )
          )%>%
          str_replace_all(
            "_grower_choice_", 
            case_when(
              gc_type == "Rx" ~ "the average of the Rx rates within the zone",
              gc_type != "Rx" ~ "the grower’s choice of a uniform  application"
            )
          )
      )
    )
  
  
  #### field-interaction figures and descriptions excluded ####
  
  # mutate(field_plots_rmd = list(
  #   if (!is.null(field_plots)) {
  #     field_plots %>% 
  #       ungroup() %>% 
  #       mutate(index = as.character(seq_len(nrow(.)))) %>% 
  #       rowwise() %>% 
  #       mutate(rmd = list(
  #         read_rmd(
  #           "Report/ri04_interaction_figures.Rmd", 
  #           locally_run = locally_run
  #         ) %>% 
  #           str_replace_all("_i-here_", index) %>% 
  #           str_replace_all(
  #             "_ch_var_here_no_underbar_", 
  #             gsub("_", "-", ch_var)
  #           ) %>%   
  #           str_replace_all("_ch_var_here_", ch_var)  
  #       )) %>% 
  #       pluck("rmd") %>% 
  #       reduce(c)
  #   } else {
  #     NULL
  #   }
  # )) %>% 
  #   mutate(report_rmd = list(
  #     insert_rmd(
  #       target_rmd = report_rmd, 
  #       inserting_rmd = field_plots_rmd,
  #       target_text = "_field-interactions-here_"
  #     )
  #   ))
  # 
  
  #/*----------------------------------*/
  #' ## Write to Rmd file(s)
  #/*----------------------------------*/
  report_rmd_ls %>% 
    summarise(
      list(
        writeLines(report_rmd, write_file_name)
      )
    )
  
  #/*----------------------------------*/
  #' ## Knit
  #/*----------------------------------*/
  report_rmd_ls %>% 
    pluck("write_file_name") %>% 
    lapply(., render)
  
}

#/*=================================================*/
#' # Make trial design and create a report
#/*=================================================*/
# Create plots and ab-lines

make_trial_design <- function(
  ffy, 
  json_file,
  head_dist = NA, 
  side_dist = NA,
  boundary = NA,
  plot_heading = NA, # the name of the ab-line file
  perpendicular = FALSE,
  min_plot_length = 200,
  max_plot_length = 300,
  ab_line_type = "free", # one of "free", "lock", "non"
  design_type = NA,
  rates = NA,
  num_levels = 6, 
  max_jumps = NA,
  file_name_append = NA,
  locally_run = FALSE
) 
{
  
  print(paste0("Generating a trial-design for ", ffy))
  
  # plot_heading = "ab-line"
  
  #/*----------------------------------*/
  #' ## Create trial data
  #/*----------------------------------*/
  trial_data <- 
    get_td_parameters(ffy, json_file)  %>%
    mutate(
      input_plot_width = conv_unit(input_plot_width, "ft", "m"),
      machine_width = conv_unit(machine_width, "ft", "m"),
      section_width = machine_width / section_num,
      harvester_width = conv_unit(harvester_width, "ft", "m"),
      plot_heading = plot_heading,
      perpendicular = perpendicular,
      min_plot_length = min_plot_length,
      max_plot_length = max_plot_length,
      ab_line_type = ab_line_type,
      rates = rates,
      num_levels = num_levels,
      max_jump = max_jumps,
      file_name_append = file_name_append
    )
  
  
  #=== trial design type ===#
  if (nrow(trial_data) == 1) {
    if (TRUE %in% is.na(design_type)) {
      trial_data$design_type <- "ejca"
    } else {
      trial_data$design_type <- design_type[1]
    }
  } else if (TRUE %in% is.na(design_type)) {
    #=== if two inputs and no design_type is specified ===#
    trial_data <- trial_data %>% 
      mutate(design_type = ifelse(form == "seed", "jcl", "ejca")) 
  } else if (length(design_type) == 1) {
    return(print(
      "Error: you did not provide two design type values even though this is a two
      input-case."
    ))
  } else {
    trial_data <- trial_data %>% 
      mutate(design_type = design_type)
  }
  
  if (nrow(trial_data) > 1 & all(trial_data$design_type == "ejca")) {
    return(print(
      "Error: you cannot use ejca for both inputs as it will create
      significant positive or negative correlations between the two inputs"
    ))
  }
  
  #=== head distance ===#
  if (is.na(head_dist)) {
    #=== machine width converted above ===#
    head_dist <- 2 * max(trial_data$machine_width)
  } else {
    #=== conversion necessary ===#
    head_dist <- conv_unit(head_dist, "ft", "m")
  }
  
  #=== side distance ===#
  if (is.na(side_dist)) {
    #=== section width converted above ===#
    side_dist <- max(max(trial_data$section_width), 30)
  } else {
    #=== conversion necessary ===#
    side_dist <- conv_unit(side_dist, "ft", "m")
  }
  
  trial_data$headland_length <- head_dist
  trial_data$side_length <- side_dist
  
  saveRDS(
    trial_data,
    here("Data", "Growers", ffy, "TrialDesign/trial_data.rds")
  )
  
  #/*=================================================*/
  #' # Rmd
  #/*=================================================*/
  td_rmd <- 
    read_rmd(
      "TrialDesignGeneration/make-trial-design-template.Rmd", 
      locally_run = locally_run
    ) %>% 
    gsub("_field-year-here_", ffy, .) %>% 
    gsub("_json-file-here_", json_file, .) %>% 
    gsub(
      "_boundary-file-here_", 
      ifelse (is.na(boundary), "boundary", boundary),
      .
    )  
  
  
  #/*=================================================*/
  #' # Wrapping up
  #/*=================================================*/
  td_file_name <- file.path(
    here(), "Data/Growers", ffy, 
    paste0(
      "TrialDesign/make_trial_design", 
      ifelse(is.na(file_name_append) | file_name_append == "", "", paste0("_", file_name_append)),
      ".Rmd"
    )
  )
  
  writeLines(td_rmd, con = td_file_name)
  
  td_r_file_name <- here(
    "Data/Growers", 
    ffy, 
    "TrialDesign/for_debug.R"
  )
  
  purl(td_file_name, output = td_r_file_name)
  
  #--- render ---#
  render(td_file_name)
  
}

#/*----------------------------------*/
#' ## Read rmd file from github repository
#/*----------------------------------*/

# file_name <- "DataProcessing/data_processing_template.Rmd"
# rmd_file[1:10]

read_rmd <- function(file_name, locally_run = FALSE) {
  
  if (locally_run == FALSE) {
    rmd_file <- readLines(here("Codes/DIFM", file_name))
  } else if (here() == "/Users/tmieno2/Box/DIFM_DevTeam"){
    #=== if in TM's DIFM_DevTeam folder ===#
    rmd_file <- readLines(here("Codes", file_name))
  } else {
    #=== if in anybody's DIFM_HQ  ===#
    rmd_file <- readLines(here("Codes/DIFM", file_name))
  }
  
  return(rmd_file)
  
}

get_r_file_name <- function(file_name, locally_run = FALSE) {
  
  if (locally_run == FALSE) {
    file_name <- paste0("https://github.com/tmieno2/DIFM/blob/master/", file_name, "?raw=TRUE")  
  } else if (here() == "/Users/tmieno2/Box/DIFM_DevTeam"){
    #=== if in TM's DIFM_DevTeam folder ===#
    file_name <- here("Codes", file_name)
  } else {
    #=== if in anybody's DIFM_HQ  ===#
    file_name <- here("Codes_team", file_name)
  }
  
  return(file_name)
  
}

insert_rmd <- function(target_rmd, inserting_rmd, target_text) {
  
  inserting_index <- which(str_detect(target_rmd, target_text))
  
  return_md <- c(
    target_rmd[1:(inserting_index-1)],
    inserting_rmd,
    target_rmd[(inserting_index+1):length(target_rmd)]
  )
  
  return(return_md)
  
}   


get_IMP_texts <- function(input_type, gc_rate, whole_profits_test, pi_dif_test_zone, opt_gc_data, gc_type, locally_run = FALSE){
  
  #=== for debugging ===#
  # input_type <- report_rmd_ls$input_type[[1]]
  # gc_rate <- report_rmd_ls$gc_rate[[1]]
  # whole_profits_test <- report_rmd_ls$whole_profits_test[[1]]
  # pi_dif_test_zone <- report_rmd_ls$pi_dif_test_zone[[1]]
  # opt_gc_data <- report_rmd_ls$opt_gc_data[[1]]
  # gc_type <- report_rmd_ls$gc_type[[1]]
  
  res_table_rmd <- read_rmd("Report/ri00_implementation_table.Rmd", locally_run = locally_run )
  
  #/*----------------------------------*/
  #' ## the profit-maximizing seed rate description right above figure1
  #' ## By Jae, Requested by DAVID  , Jan31,22
  #/*----------------------------------*/
  
  
  gc_opt_desc_txt <- opt_gc_data[type=="opt_v",] %>% 
    #=== y for gc and x for opt_v ===#
    mutate(dsc = input_rate) %>% 
    dplyr::select(zone_txt, dsc) %>% 
    arrange(zone_txt) %>% 
    mutate(
      gc_opt_desc_txt = 
        paste0(
          abs(dsc) %>% round(digits = 0),
          " _unit_here_/ac",
          " in ",
          str_to_title(zone_txt)
        )
    ) %>% 
    pull(gc_opt_desc_txt) %>% 
    paste(collapse = ", ")
  
  res_table_rmd <- gsub(
    "_gc-opt-desc-txt-comes-here_",
    gc_opt_desc_txt,
    res_table_rmd
  )
  
  
  #' #/*----------------------------------*/
  #' #' ## Profit differential narrative
  #' #/*----------------------------------*/
  #' # Statements about the difference between 
  #' # optimal vs grower-chosen rates
  #' 
  #' if (nrow(pi_dif_test_zone) > 1) {
  #'   pi_dif_zone_rmd <- tibble(
  #'     w_zone = 2:nrow(pi_dif_test_zone)
  #'   ) %>% 
  #'     rowwise() %>% 
  #'     mutate(t_value = list(
  #'       pi_dif_test_zone[zone_txt == paste0("Zone ", w_zone), t]
  #'     )) %>% 
  #'     mutate(pi_dif_rmd_indiv = list(
  #'       read_rmd(
  #'         "Report/ri02_profit_dif_statement.Rmd",
  #'         locally_run = locally_run
  #'       ) %>%
  #'         gsub("_insert-zone-here_", w_zone, .) %>% 
  #'         gsub(
  #'           "_t-confidence-statement_", 
  #'           get_t_confidence_statement(t_value), 
  #'           .
  #'         )
  #'     )) %>% 
  #'     pluck("pi_dif_rmd_indiv") %>% 
  #'     reduce(c)
  #' } else {
  #'   #=== if there is only one zone ===#
  #'   pi_dif_zone_rmd <- NULL
  #' }
  #' 
  #' res_disc_rmd <- insert_rmd(
  #'   target_rmd = res_disc_rmd, 
  #'   inserting_rmd = pi_dif_zone_rmd,
  #'   target_text = "_rest-of-the-zones-here_"
  #' ) %>% 
  #'   gsub("_gc_rate_here_", gc_rate, .) %>% 
  #'   #=== t-test statement for zone 1 (exception) ===#
  #'   gsub(
  #'     "_t-confidence-statement_1_", 
  #'     get_t_confidence_statement(
  #'       pi_dif_test_zone[zone_txt == paste0("Zone ", 1), t]
  #'     ), 
  #'     .
  #'   )
  #' 
  #' #/*----------------------------------*/
  #' #' ## Difference between optimal vs grower-chosen rates
  #' #/*----------------------------------*/
  #' # Statements about the difference between 
  #' # optimal vs grower-chosen rates
  #' 
  #' gc_opt_comp_txt <- left_join(
  #'   opt_gc_data[type == "opt_v", ],
  #'   opt_gc_data[type == "gc", ],
  #'   by = "zone_txt"
  #' ) %>% 
  #'   #=== y for gc and x for opt_v ===#
  #'   mutate(dif = input_rate.y - input_rate.x) %>% 
  #'   dplyr::select(zone_txt, dif) %>% 
  #'   arrange(zone_txt) %>% 
  #'   mutate(
  #'     gc_opt_comp_txt = 
  #'       paste0(
  #'         abs(dif) %>% round(digits = 0),
  #'         " _unit_here_ per acre", 
  #'         ifelse(dif > 0, " too high", " too low"),
  #'         " in ",
  #'         str_to_title(zone_txt)
  #'       )
  #'   ) %>% 
  #'   pull(gc_opt_comp_txt) %>% 
  #'   paste(collapse = ", ")
  #' 
  #' res_disc_rmd <- gsub(
  #'   "_gc-opt-comp-txt-comes-here_",
  #'   gc_opt_comp_txt,
  #'   res_disc_rmd
  #' )
  #' 
  #' 
    return(res_table_rmd) 
  
}


get_ERI_texts <- function(input_type, gc_rate, whole_profits_test, pi_dif_test_zone, opt_gc_data, gc_type, locally_run = FALSE){
  
  #=== for debugging ===#
  # input_type <- report_rmd_ls$input_type[[1]]
  # gc_rate <- report_rmd_ls$gc_rate[[1]]
  # whole_profits_test <- report_rmd_ls$whole_profits_test[[1]]
  # pi_dif_test_zone <- report_rmd_ls$pi_dif_test_zone[[1]]
  # opt_gc_data <- report_rmd_ls$opt_gc_data[[1]]
  # gc_type <- report_rmd_ls$gc_type[[1]]
  
  res_disc_rmd <- read_rmd("Report/ri01_results_by_zone.Rmd", locally_run = locally_run )
  
  #/*----------------------------------*/
  #' ## the profit-maximizing seed rate description right above figure1
  #' ## By Jae, Requested by DAVID  , Jan31,22
  #/*----------------------------------*/

  
  gc_opt_desc_txt <- opt_gc_data[type=="opt_v",] %>% 
    #=== y for gc and x for opt_v ===#
    mutate(dsc = input_rate) %>% 
    dplyr::select(zone_txt, dsc) %>% 
    arrange(zone_txt) %>% 
    mutate(
      gc_opt_desc_txt = 
        paste0(
          abs(dsc) %>% round(digits = 0),
          " _unit_here_/ac",
          " in ",
          str_to_title(zone_txt)
        )
    ) %>% 
    pull(gc_opt_desc_txt) %>% 
    paste(collapse = ", ")
  
  res_disc_rmd <- gsub(
    "_gc-opt-desc-txt-comes-here_",
    gc_opt_desc_txt,
    res_disc_rmd
  )
  
  
  #/*----------------------------------*/
  #' ## Profit differential narrative
  #/*----------------------------------*/
  # Statements about the difference between 
  # optimal vs grower-chosen rates
  
  if (nrow(pi_dif_test_zone) > 1) {
    pi_dif_zone_rmd <- tibble(
      w_zone = 2:nrow(pi_dif_test_zone)
    ) %>% 
      rowwise() %>% 
      mutate(t_value = list(
        pi_dif_test_zone[zone_txt == paste0("Zone ", w_zone), t]
      )) %>% 
      mutate(pi_dif_rmd_indiv = list(
        read_rmd(
          "Report/ri02_profit_dif_statement.Rmd",
          locally_run = locally_run
        ) %>%
          gsub("_insert-zone-here_", w_zone, .) %>% 
          gsub(
            "_t-confidence-statement_", 
            get_t_confidence_statement(t_value), 
            .
          )
      )) %>% 
      pluck("pi_dif_rmd_indiv") %>% 
      reduce(c)
  } else {
    #=== if there is only one zone ===#
    pi_dif_zone_rmd <- NULL
  }
  
  res_disc_rmd <- insert_rmd(
    target_rmd = res_disc_rmd, 
    inserting_rmd = pi_dif_zone_rmd,
    target_text = "_rest-of-the-zones-here_"
  ) %>% 
    gsub("_gc_rate_here_", gc_rate, .) %>% 
    #=== t-test statement for zone 1 (exception) ===#
    gsub(
      "_t-confidence-statement_1_", 
      get_t_confidence_statement(
        pi_dif_test_zone[zone_txt == paste0("Zone ", 1), t]
      ), 
      .
    )
  
  #/*----------------------------------*/
  #' ## Difference between optimal vs grower-chosen rates
  #/*----------------------------------*/
  # Statements about the difference between 
  # optimal vs grower-chosen rates
  
  gc_opt_comp_txt <- left_join(
    opt_gc_data[type == "opt_v", ],
    opt_gc_data[type == "gc", ],
    by = "zone_txt"
  ) %>% 
    #=== y for gc and x for opt_v ===#
    mutate(dif = input_rate.y - input_rate.x) %>% 
    dplyr::select(zone_txt, dif) %>% 
    arrange(zone_txt) %>% 
    mutate(
      gc_opt_comp_txt = 
        paste0(
          abs(dif) %>% round(digits = 0),
          " _unit_here_ per acre", 
          ifelse(dif > 0, " too high", " too low"),
          " in ",
          str_to_title(zone_txt)
        )
    ) %>% 
    pull(gc_opt_comp_txt) %>% 
    paste(collapse = ", ")
  
  res_disc_rmd <- gsub(
    "_gc-opt-comp-txt-comes-here_",
    gc_opt_comp_txt,
    res_disc_rmd
  )
  
  
  return(res_disc_rmd) 
  
}


get_t_confidence_statement <- function(t_value) {
  case_when(
    t_value < qt(0.75, df = 1000) ~ "could establish only with a negligible",
    qt(0.75, df = 1000) <= t_value & t_value < qt(0.85, df = 1000) ~ "could establish only with a limited",
    qt(0.85, df = 1000) <= t_value & t_value < qt(0.95, df = 1000) ~ "established with a moderate", 
    t_value >= qt(0.95, df = 1000)  ~ "established with a strong"  
  )
}

get_t_confidence_statement_prof_diff <- function(t_value) {
  case_when(
    t_value < qt(0.75, df = 1000) ~ "could provide only a negligible",
    qt(0.75, df = 1000) <= t_value & t_value < qt(0.85, df = 1000) ~ "could provide only a limited",
    qt(0.85, df = 1000) <= t_value & t_value < qt(0.95, df = 1000) ~ "provide a moderate", 
    t_value >= qt(0.95, df = 1000)  ~ "provide strong"  
  )
}

get_whole_pi_txt <- function(results) {
  
  whole_pi_t <- results$whole_profits_test[[1]][type_short == "ovg", t]
  
  if (whole_pi_t > qt(0.95, df = 1000)) {
    
    text_summary <- "The data and model provide a high degree of statistical confidence in this result"
    
  } else if (whole_pi_t > qt(0.85, df = 1000)) {
    
    text_summary <- "The data and model provide a moderate degree of statistical confidence in this result"
    
  } else if (whole_pi_t > qt(0.75, df = 1000)) {
    
    text_summary <- "The data and model provide an only limited  degree of statistical confidence in this result"
    
  } else {
    
    text_summary <- "But, the data and model provide a low degree of statistical confidence in this result"
    
  }
  
  return(text_summary)
  
}


get_td_text <- function(input_type, gc_type, locally_run = FALSE) {
  
  td_rmd_file <- "Report/ri03_trial_design.Rmd"
  
  td_rmd <- read_rmd(td_rmd_file, locally_run = locally_run)
  
  if (gc_type == "Rx") {
    grower_plan_text <- "follow the commercial prescription depicted 
      in the first panel of figure \\\\@ref(fig:mix-figures)"
  } else if (gc_type == "uniform") {
    grower_plan_text <- "apply `r round(_gc_rate_here_, digits = 0)` _unit_here_ per acre 
      uniformly across the field. " 
  }
  
  if (input_type == "S") {
    input_implement_text <- "The trial design's targeted seed rates were  `r paste0(round(tgti_ls,0))`
    _unit_here_/ac"
  } else if (input_type == "N") {
    input_implement_text <- "The producer applied a `r n_base_rate` lbs/ac base N rate 
    before implementing the experiment. Then the N trial was implementing by applying `r input_data_n$form` 
    at rates of `r paste0(round(tgt_gal_ls,0))` `r input_data_n$unit` /ac (not converted into N equivalent)
    according the the trial design shown in figure 1.  The base rate and `r input_data_n$form` rates 
    resulted in total N rates of  `r paste0(round(tgti_ls,0))` lbs/ac.(converted into N equivalent)  " 
  }

  
  td_rmd <- td_rmd   %>%
               gsub("_grower-plan-here_", grower_plan_text, .) %>%
               gsub("_input-implement-here_",input_implement_text, .)
  
  return(td_rmd)    
  
}

prepare_e02_rmd <- function(input_type, process, use_td, locally_run = TRUE){
  
  if (process & !use_td) {
    
    return_rmd <- read_rmd(
      "DataProcessing/e02_process_as_applied_base.Rmd", 
      locally_run = locally_run
    ) %>% 
      gsub("input_type_here", input_type, .) %>% 
      gsub(
        "as-applied-file-name-here", 
        paste0("as-applied-", tolower(input_type)), 
        .
      )
    
  } else if (process & use_td){
    
    return_rmd <- read_rmd(
      "DataProcessing/e02_use_td.Rmd", 
      locally_run = locally_run
    ) %>% 
      gsub("input_type_here", input_type, .)
    
    
  } else {
    
    return_rmd <- NULL
    
  }
  
  return(return_rmd)
}

get_input <- function(opt_gc_data, c_type, w_zone){
  opt_gc_data[type == c_type & zone_txt == paste0("Zone ", w_zone), input_rate] %>% round(digits = 0)
}

get_plot_size <- function(ffy, input_type, trial_type, reporting_unit){
    plot_width <- trial_info %>%
      filter(input_type == input_type) %>%
      pull(trial_width)

  plot_lengths <- here("Data/Growers", ffy, "TrialDesign", paste0("trial-design-", input_type, ".shp")) %>%
    st_read() %>%
    st_transform_utm() %>%
    st_area(.) %>%
    as.numeric(.) %>%
    data.frame(.) %>%
    filter(. < 2*median(.)) %>%
    conv_unit(., "m2", "ft2")/plot_width 
  
  if(reporting_unit == "metric"){
    plot_lengths <- conv_unit(plot_lengths, "ft", "m")
  }
  
 return(paste0(round(min(plot_lengths$.)), " - ", round(max(plot_lengths$.))))
}
# 
# treat_ls <- results$treat_ls[[1]]
# control <- results$control[[1]]
# opt_vr <- results$opt_vr[[1]]
#--- function for determining if future rates should increase, decrease, or widen ---#
future_recs <- function(treat_ls, control, opt_vr){
  if(length(treat_ls) == 1){
    perc_high <- opt_vr %>%
      filter(EOIR == max(treat_ls, control)) %>%
      nrow()/nrow(opt_vr)
    
    perc_low <- opt_vr %>%
      filter(EOIR == min(treat_ls, control)) %>%
      nrow()/nrow(opt_vr)
    
    if(perc_low > .20 & perc_high > .20){
      recommendation <- "expanded"
    }else if(perc_low < .15 & perc_high > .50){
      recommendation <- "increased"
    }else if(perc_low > .50 & perc_high < .15){
      recommendation <- "decreased"
    }else{
      recommendation <- "similar"
    }
    
  }else{
    t_num <- length(treat_ls)
    
    perc_high <- opt_vr %>%
      filter(EOIR == treat_ls[t_num] |
               EOIR == treat_ls[t_num - 1]) %>%
      nrow()/nrow(opt_vr)
    
    perc_low <- opt_vr %>%
      filter(EOIR == min(treat_ls) |
               EOIR == control) %>%
      nrow()/nrow(opt_vr)
    
    if(perc_low > .15 & perc_high > .20){
      recommendation <- "expanded"
    }else if(perc_low < .15 & perc_high > .50){
      recommendation <- "increased"
    }else if(perc_low > .50 & perc_high < .15){
      recommendation <- "decreased"
    }else{
      recommendation <- "similar"
    }
  }

  return(recommendation)
}

get_field_size <- function(ffy, input_type){
  if(file.exists(here("Data/Growers", ffy, "TrialDesign/boundary.shp"))){
    area <- here("Data/Growers", ffy, "TrialDesign/boundary.shp") %>%
      st_read(.) %>%
      st_make_valid(.) %>%
      st_set_4326(.) %>%
      st_transform_utm() %>%
      st_area(.) %>%
      sum(.) %>%
      as.numeric(.) %>%
      conv_unit(., "m2", "acre")
  }else{
    area <- here("Data/Growers", ffy, "TrialDesign", paste0("trial-design-", tolower(input_type), ".shp")) %>%
      st_read(.) %>%
      st_make_valid(.) %>%
      st_set_4326(.) %>%
      st_transform_utm() %>%
      st_area(.) %>%
      sum(.) %>%
      as.numeric(.) %>%
      conv_unit(., "m2", "acre")
  }
  return(area)
}

make_compensation_report <- function(ffy, rerun = TRUE){
  
  
  #/*----------------------------------*/
  #' ## If rerun = TRUE
  #/*----------------------------------*/
  if (rerun){
    #--- remove cached files ---#
    list.files(
      file.path(here(), "Reports/Growers", ffy),
      full.names = TRUE
    ) %>% 
      .[str_detect(., "compensation-report")] %>% 
      .[str_detect(., c("cache|files"))] %>% 
      unlink(recursive = TRUE)
  }
  
  results <- readRDS(here("Reports", "Growers", ffy, "analysis_results.rds"))
  
  
  base_rmd <- readLines(here("Codes/DIFM/Report", "c_r00_report_header.Rmd"))
  
  results_gen_rmd <- readLines(here("Codes/DIFM/Report", "c_r01_gen_results.Rmd"))  


  report_rmd_ls <- results %>% 
    mutate(
      report_rmd = list(c(base_rmd, results_gen_rmd))  
    ) %>% 
    rowwise() %>% 
    mutate(
      report_body = list(
        readLines(here("Codes/DIFM/Report", "c_r01_make_report.Rmd"))
      )
    ) %>% 
    mutate(
      report_rmd = list(
        c(report_rmd, report_body)
      )
    )  %>%
    mutate(
       write_file_name =  list(
        here(
          "Reports/Growers", ffy, 
          paste0(ffy,"_compensation-report", ".Rmd")
        )
      )
    )
      

  
  #%>% str_replace_all("_field-year-here_", ffy) 
  
  #/*----------------------------------*/
  report_rmd_ls %>% 
    summarise(
      list(
        writeLines(report_rmd, write_file_name)
      )
    )
  
  #/*----------------------------------*/
  #' ## Knit
  #/*----------------------------------*/
  report_rmd_ls %>% 
    pluck("write_file_name") %>% 
    lapply(., render)
  
}  

# Functions for grower reprot creation

get_misalignment_type = function(figure_table){
  if((figure_table %>%
      filter(area == "worst") %>%
      pull(perc) %>%
      unlist() %>%
      mean() < .80) == TRUE){
    #### incompatible machinery widths ####
    inputs <- input_data %>%
      filter(strategy != "base")
    harvester_width <- w_field_data$h_width[[1]]
    
    applicator_widths <- input_data %>%
      filter(strategy != "base") %>%
      pull(machine_width)
    
    section_num <- input_data %>%
      filter(strategy != "base") %>%
      pull(section_num)
    
    use_tgt <- input_data %>%
      filter(strategy != "base") %>%
      pull(use_td)
    
    plot_widths <- input_data %>%
      filter(strategy != "base") %>%
      pull(trial_width)
    
    if (nrow(inputs) == 1){
      input_1_fits <- (max(plot_widths) %% applicator_widths[1] == 0 | 
                         max(plot_widths) %% (applicator_widths[1]/section_num[1]) == 0 | 
                         use_tgt[1] == TRUE)
      
      if (input_1_fits == FALSE){
        incomp_mach = TRUE
      }else{
        incomp_mach = FALSE
      }
    }else{
      input_plots_fit <- ((plot_widths[1] == plot_widths[2]) | (plot_widths[1] %% plot_widths[2] == 0))
      
      input_1_fits <- (max(plot_widths) %% applicator_widths[1] == 0 | 
                         max(plot_widths) %% (applicator_widths[1]/section_num[1]) == 0 | 
                         use_tgt[1] == TRUE)
      input_2_fits <- (max(plot_widths) %% applicator_widths[2] == 0 |
                         max(plot_widths) %% (applicator_widths[2]/section_num[2]) == 0 |
                         use_tgt[2] == TRUE)
      
      if (input_1_fits == FALSE | input_2_fits == FALSE){
        incomp_mach = TRUE
      }else{
        incomp_mach = FALSE
      }
      
      if (input_plots_fit == FALSE){
        incomp_plots = TRUE # this is a user error at the point of trial design
      }else{
        incomp_plots = FALSE
      }
    }
    
    #### angle difference ####
    # use the data from the worst area
    data <- figure_table %>%
      filter(area == "worst")
    
    yield_angle <- data %>%
      filter(type == "yield") %>%
      dplyr::select(int) %>%
      .[[1]] %>%
      .[[1]] %>%
      pull(angle) %>%
      median(., na.rm = TRUE)
    
    if(nrow(data) == 2){
      aa_angle_1 <- data %>%
        filter(type != "yield") %>%
        dplyr::select(int) %>%
        .[[1]] %>%
        .[[1]] %>%
        pull(angle) %>%
        median(. , na.rm = TRUE)
    }else{
      for (i in 1:2){
        aa_angle <- data %>%
          filter(type != "yield") %>%
          dplyr::select(int) %>%
          .[[i]] %>%
          .[[1]] %>%
          pull(angle) %>%
          median(., na.rm = TRUE)
        
        assign(paste0("aa_angle_", i), aa_angle)
      }
    }
    
    if(nrow(data) == 2){
      y_aa1_diff <- yield_angle - aa_angle_1
      
      if ((y_aa1_diff < 0.99 | y_aa1_diff > 179)){
        heading_diff = FALSE
      }else{
        heading_diff = TRUE
      }
      
    }else{
      y_aa1_diff <- yield_angle - aa_angle_1
      y_aa2_diff <- yield_angle - aa_angle_2
      aa_1_2_diff <- aa_angle_1 - aa_angle_2
      
      if ((y_aa1_diff < 0.99 | y_aa1_diff > 179)
          (y_aa2_diff < 0.99 | y_aa2_diff > 179)
          (aa_1_2_diff < 0.99 | aa_1_2_diff > 179)){
        heading_diff = FALSE
      }else{
        heading_diff = TRUE
      }
      
    }
    
    #### find parallel-shift by logic if it isn't angle diff or incompatible machinery ####
    if(heading_diff == TRUE){
      misalignment_type = "The primary type of misalignment present on this field was caused by the machines being driven at different angles, causing the harvester to cross through different application rates in one pass. This can be avoided through improved trial planning and communication."
    }else if(incomp_mach == TRUE){
      misalignment_type = "The primary type of misalignment present on this field was caused by machinery sizes that were not compatible with each other or with the plot width, creating many parts of the field where the harvester passed through different application rates. This type of misalignment is unavoidable; however, if the harvester is smaller than one-half the width of the plot widths, each trial plot can have at least one clean harvest pass."
    }else{
      misalignment_type = "The primary type of misalignment present on this field was caused by a parallel-shift between the application and harvest passes. While the machines were driven in the same directions, they did not align correctly to harvest from one application rate in each pass. This type of misalignment can be improved in the future by supplying another harvest ab-line after the application to ensure accurate harvesting."
    }
    
  }else{
    misalignment_type = ""
  }
  
  return(misalignment_type)
}

get_input_price <- function(price, crop, input_type){
  if(input_type == "seeds"){
    if(is.na(price) == TRUE) {
      s_price <- input_prices %>%
        filter(Crop == crop & Input == "S" & Type == "current") %>%
        pull(Price)
    } else { # if price is numeric
      if (price < 0.01) {
        price <- price * 1000
      }
    }
  }
  else if (input_type %in% c("NH3", "urea", "uan32", "uan28", "1_2_1(36)", "LAN(26)", "MAP", "1_0_0", "1_0_1", "2_3_2(22)",
                             "15_10_6", "3_0_1", "2_3_4(32)", "4_3_4(33)", "5_1_5", "Sp", "N_equiv", "24-0-0-3 UAN","chicken_manure")){
    if(is.na(price) == TRUE) {
      price <- input_prices %>%
        filter(Crop == crop & Input == "N" & Type == "current" & Form == input_type) %>%
        mutate(Price = if(Price.Unit == "Ton"){Price/conv_unit(1, "short_ton", "lbs")}) %>%
        pull(Price)
      
      price <- price/convert_N_unit(form = input_type, unit = "lbs", rate = 1, reporting_unit = "imperial")
      # add line to convert to pounds N and then divide again to get price for one pound of N
    }
  }
  else if (input_type == "sulfur"){
    if(is.na(price) == TRUE) {
      price <- input_prices %>%
        filter(Crop == crop & Input == "Sulfur" & Type == "current") %>%
        pull(Price)
    } else { # if price is numeric
      if (price < 0.01) {
        price <- price * 1000
      }
    }
  }else if(input_type == "K"){
    if(is.na(price) == TRUE) {
      price <- input_prices %>%
        filter(Crop == crop & Input == "K" & Type == "current") %>%
        pull(Price)
    }
  }else{
    price == price # when the input is not in the system, we only have the price they supply
  }
  
  return(price)
}

get_gc_rate <- function(gc_rate, input_type, form, unit, convert, base_rate){
  if((input_type %in% c("NH3", "urea", "uan32", "uan28", "1_2_1(36)", "LAN(26)", "MAP", "1_0_0", "1_0_1", "2_3_2(22)",
                       "15_10_6", "3_0_1", "2_3_4(32)", "4_3_4(33)", "5_1_5", "Sp", "N_equiv", "24-0-0-3 UAN","chicken_manure")) 
     & (convert == TRUE)){
    if (!is.numeric(gc_rate)) {
      Rx_file <- file.path(
        here("Data/Growers", ffy, "Raw"), 
        paste0(gc_rate_n, ".shp")
      )
      
      if (file.exists(Rx_file)){
        #--- if the Rx file exists ---#
        gc_type <- "Rx"
        gc_rate <- Rx_file_n
        
      }
    } else {
      gc_rate <- data.table(gc_rate = gc_rate, 
                            form = form,
                            unit = unit) %>%
        rowwise() %>%
        mutate(gc_rate :=  convert_N_unit(form, unit, gc_rate, reporting_unit) + base_rate) %>%
        ungroup() %>%
        as.data.frame() %>%
        pull("gc_rate")
      
      gc_type <- "uniform"
    }
  }else{
    gc_rate = gc_rate
  }
  
  return(gc_rate)
}

get_base_rate <- function(input_data, input_type){
  if(input_type %in% c("NH3", "urea", "uan32", "uan28", "1_2_1(36)", "LAN(26)", "MAP", "1_0_0", "1_0_1", "2_3_2(22)",
                       "15_10_6", "3_0_1", "2_3_4(32)", "4_3_4(33)", "5_1_5", "Sp", "N_equiv", "24-0-0-3 UAN","chicken_manure")){
    is_base <- "base" %in% input_data[, strategy]
    
    if (is_base) {
      base_rate <- input_data[strategy == "base", ] %>% 
        rowwise() %>% 
        mutate(
          n_equiv_rate = convert_N_unit(
            form = form, 
            unit = unit, 
            rate = rate, 
            reporting_unit = w_field_data$reporting_unit
          ) 
        ) %>% 
        data.table() %>% 
        .[, sum(n_equiv_rate)]
    } else {
      base_rate <- 0
    }
  }else{
    base_rate = 0
  }
  
  return(base_rate)
}

# area_type = "average"
make_figure <- function(figure_table, area_type, trial_design, trial_info){
  subset <- figure_table %>%
    filter(area == area_type) %>%
    arrange(desc(use_td))
  
  tmap_data <- list()
  tmap_mach <- list()
  for (i in 1:(nrow(subset))){
    tmap_data[[i]] <- paste0("subset$tmap_data[[", i, "]]")
    tmap_mach[[i]] <- paste0("subset$tmap_mach[[", i, "]]")
  }
  
  if(TRUE %in% trial_info$use_td){
    fig <- 
      eval(parse(text = paste0(tmap_data, collapse = " + "))) +
      eval(parse(text = paste0(tmap_mach, collapse = " + "))) +
      tm_shape(st_bbox(subset$int[[1]]) %>%
                 st_as_sfc() %>% 
                 st_as_sf()) +
      tm_borders(col = if(area_type == "best"){
        "skyblue"
      }else if(area_type == "worst"){
        "#e34234"
      }else{
        "black"},
      lwd = 4) +
      tm_layout_to_add +
      tm_layout(
        legend.outside.position = "left",
        legend.position = c(0.25, 0.25)) 
    
  }else{
    fig <- tm_shape(trial_design, bbox = st_bbox(subset$int[[1]])) +
      tm_borders(
        col = "black") +
      tm_shape(st_bbox(subset$int[[1]]) %>%
                 st_as_sfc() %>% 
                 st_as_sf()) +
      tm_borders(col = if(area_type == "best"){
        "skyblue"
      }else if(area_type == "worst"){
        "#e34234"
      }else{
        "black"},
      lwd = 4) +
      tm_layout_to_add +
      tm_layout(
        legend.outside.position = "left",
        legend.position = c(0.25, 0.25)) +
      eval(parse(text = paste0(tmap_data, collapse = " + "))) +
      eval(parse(text = paste0(tmap_mach, collapse = " + "))) 
    
  }
  
  return(fig)
}

make_grid <- function(trial_design){
  area <- trial_design %>% 
    st_union() %>%
    st_convex_hull() %>%
    st_buffer(., max(conv_unit(trial_info$machine_width, "ft", "m")*3))
  bbox <- st_bbox(area)
  
  x_num = round((bbox[3] - bbox[1])/100)
  y_num = round((bbox[4] - bbox[2])/100)
  
  grid <- st_make_grid(
    area,
    n = c(x_num, y_num),
    what = "polygons") %>%
    st_sf(id = 1:(x_num*y_num), geometry = .)
  
  return(grid)
}

# grid <- figure_table$grid[[1]]
# input_dev <- figure_table$input_dev[[1]]

cell_info <- function(i, grid, input_dev){
  subset <- grid[i,] %>%
    input_dev[., ] 
  
  
  perc_good_field <- mean(subset$good_dev)
  points_in_cell <- nrow(subset)
  return(c(i, perc_good_field, points_in_cell))
}

make_grid_table <- function(grid, input_dev){
  cell_table <- sapply(1:nrow(grid), cell_info, grid = grid, input_dev = input_dev) %>%
    t(.) %>%
    data.frame(.)
  
  names(cell_table) <- c("grid", "perc_good_field", "points_in_cell")
  
  return(cell_table)
}

# cell_table <- figure_table$cell_table[[1]]
# grid <- figure_table$grid[[1]]
# type <- figure_table$area[[1]]
# data <- figure_table$data[[1]]
# input_dev <- figure_table$input_dev[[1]]
# use_td <- figure_table$use_td[[1]]

grid_data_int <- function(cell_table, grid, type, data, input_dev, use_td){
  if(type == "average"){
    grid_id <- cell_table %>%
      filter(points_in_cell >= 0) %>% # has enough points
      filter(points_in_cell >= quantile(points_in_cell, probs = 0.8)) %>% # has enough points
      mutate(diff_perc = abs(mean(perc_good_field, na.rm = TRUE) - perc_good_field)) %>% # closest to average perc
      filter(diff_perc == min(diff_perc, na.rm = TRUE)) %>%
      filter(points_in_cell == max(points_in_cell, na.rm = TRUE)) %>%
      .[1,] %>%
      pull(grid)
  }else if(type == "best"){
    grid_id <- cell_table %>%
      filter(points_in_cell >= 0) %>% # has enough points
      filter(points_in_cell >= quantile(points_in_cell, probs = 0.8)) %>% # has enough points
      filter(perc_good_field == max(perc_good_field, na.rm = TRUE)) %>%
      filter(points_in_cell == max(points_in_cell, na.rm = TRUE)) %>%
      .[1,] %>%
      pull(grid)
  }else{
    grid_id <- cell_table %>%
      filter(points_in_cell >= 0) %>% # has enough points
      filter(points_in_cell >= quantile(points_in_cell, probs = 0.8)) %>% # has enough points
      filter(perc_good_field == min(perc_good_field, na.rm = TRUE)) %>%
      filter(points_in_cell == max(points_in_cell, na.rm = TRUE)) %>%
      .[1,] %>%
      pull(grid)
  }
  
  center <- grid %>%
    filter(id == grid_id) %>%
    st_centroid() %>%
    st_coordinates()
  
  plot_width <- trial_info %>%
    pull(trial_width) %>%
    conv_unit(., "ft", "m") %>%
    max()
  
  m_width <- harvester_width %>%
    conv_unit(., "ft", "m")
  
  # find number of trial plots to have 6 passes of the machines
  plot_num <- (m_width*6)/plot_width
  
  #--- get the new x and y limits for the box around center ---#
  ylims <- c(center[2] - plot_width*plot_num/2, center[2] + plot_width*plot_num/2)
  xlims <- c(center[1] - plot_width*plot_num/2, center[1] + plot_width*plot_num/2)
  
  # Create sf for this bounding box
  bounding.box <- st_as_sf(data.frame(x = xlims, y = ylims),
                           coords = c("x", "y"),
                           crs = st_crs(input_dev)) %>%
    st_bbox(box_coords) %>% 
    st_as_sfc(.)
  
  if(use_td == TRUE){
    subset = bounding.box
  }else{
    data <- data %>%
      st_centroid() %>%
      group_points_sc(., by_var = NA, 30) %>%
      dplyr::select(group, X, Y) %>%
      group_by(group) %>%
      mutate(
        seq_id = row_number(),
        dx = X - dplyr::lag(X, n = 1),
        dy = Y - dplyr::lag(Y, n = 1)
      )
    
    subset <- st_intersection(data, bounding.box)
    
  }
  
  return(subset)
}

get_perc_good <- function(cell_table, grid, type, input_dev){
  if(type == "average"){
    perc <- cell_table %>%
      filter(points_in_cell >= quantile(points_in_cell, probs = 0.8)) %>% # has enough points
      mutate(diff_perc = abs(mean(perc_good_field) - perc_good_field)) %>% # closest to average perc
      filter(diff_perc == min(diff_perc)) %>%
      .[1,] %>%
      pull(perc_good_field) %>%
      round(., 2)
  }else if(type == "best"){
    perc <- cell_table %>%
      filter(points_in_cell >= quantile(points_in_cell, probs = 0.8)) %>% # has enough points
      filter(perc_good_field == max(perc_good_field)) %>%
      .[1,] %>%
      pull(perc_good_field) %>%
      round(., 2)
  }else{
    perc <- cell_table %>%
      filter(points_in_cell >= quantile(points_in_cell, probs = 0.8)) %>% # has enough points
      filter(perc_good_field == min(perc_good_field)) %>%
      .[1,] %>%
      pull(perc_good_field) %>%
      round(., 2)
  }
  
  return(perc)
}

get_perc_good_mean <- function(cell_table, grid, input_dev){
  perc <- cell_table %>%
    pull(perc_good_field) %>% # closest to average perc
    mean(., na.rm = TRUE) %>%
    round(., 2)
  
  return(perc)
}

# data <- figure_table$data[[4]]
# width <- figure_table$width[[4]]
# get_new_width(data, width)

get_new_width <- function(data, width){
  width_data <- median(data$width, na.rm = TRUE)
  
  if (is.null(width_data) == FALSE){
    if(width != width_data){
      width = width_data
    }else{
      width = width
    }
  }else{
    width = width
  }
  
  return(width)
}

# int = figure_table$int[[1]]
# type <- figure_table$type[[1]]
# machine_number <- figure_table$machine_number[[1]]
# width = figure_table$width[[1]]
# use_td = figure_table$use_td[[1]]

make_machines <- function(int, type, machine_number, width, use_td){
  
  if(use_td == TRUE){
    machine_polys = NULL
  }else{
    # width = int$width
    grp_main <- int
    
    if(length(unique(grp_main$group)) >= 2){
      groups <- grp_main %>%
        group_by(group) %>%
        tally() %>%
        filter(n >= (mean(n) - 2*sd(n))) %>%
        pull(group)
    }else{
      groups <- grp_main %>%
        group_by(group) %>%
        tally() %>%
        pull(group)
    }
    
    grp_diff <- grp_main %>%
      filter(group %in% groups) %>%
      group_by(group) %>%
      summarize(med_dx = median(dx, na.rm = TRUE),
                med_dy = median(dy, na.rm = TRUE)) %>%
      na.omit(.)
    
    # find the two directions of the machine and create a variable with the direction
    # this is important because the direction changes where we want the machine figure to be
    if (nrow(grp_diff) > 2){
      km <- kmeans(grp_diff %>% as.data.frame %>% .[, c("med_dx", "med_dy")], centers = 2, nstart = 25)
      grp_diff$cluster = km$cluster
      group_list = unique(grp_diff$group)
    }else if(nrow(grp_diff) == 2){
      grp_diff$cluster = 1:2
      group_list = unique(grp_diff$group)
    }else{
      grp_diff$cluster = 1
      group_list = 1
    }
    
    if (nrow(grp_diff) > 2){
      # create line across the area of the figure and take intersection
      area_cent <- st_centroid(int) %>% st_union() %>% st_convex_hull() %>% st_centroid() %>% st_coordinates() %>% as.matrix()
      move_vec_mach_dir <- scalar(km$centers[1,]) # just choosing the first direction
      move_vec_perp <- rotate_vec(move_vec_mach_dir, 90) # just choosing the first direction
    }else if(nrow(grp_diff) == 2){
      # create line across the area of the figure and take intersection
      area_cent <- st_centroid(int) %>% st_union() %>% st_convex_hull() %>% st_centroid() %>% st_coordinates() %>% as.matrix()
      move_vec_mach_dir <- scalar(c(grp_diff$med_dx[1], grp_diff$med_dy[1])) # just choosing the first direction
      move_vec_perp <- rotate_vec(move_vec_mach_dir, 90) # just choosing the first direction
    }else{
      area_cent <- st_centroid(int) %>% st_union() %>% st_convex_hull() %>% st_centroid() %>% st_coordinates() %>% as.matrix()
      move_vec_mach_dir <- scalar(c(grp_diff$med_dx[1], grp_diff$med_dy[1])) # just choosing the first direction
      move_vec_perp <- rotate_vec(move_vec_mach_dir, 90) # just choosing the first direction
    }
    
    
    # make line across the field in the direction of the machine and then take intersection with the data area
    line_in_mach_dir <-
      rbind(
        area_cent - move_vec_mach_dir * 200,
        area_cent + move_vec_mach_dir * 200
      ) %>%
      st_linestring() %>%
      st_sfc(crs = st_crs(grp_main)) %>%
      st_sf() %>%
      st_intersection(., st_centroid(int) %>% st_union() %>% st_convex_hull())
    
    # find centers for new lines that depend on the machine and the length of the line
    # if we have three machines, then we need room for 6 rows of machine polygons
    line_shift <- st_length(line_in_mach_dir)/(((nrow(trial_info) + 1)*2) + 2)
    units(line_shift) <- NULL
    
    if (move_vec_mach_dir[1] < 0){
      center_to_add <- st_coordinates(line_in_mach_dir)[, 1:2] %>%
        as.data.frame() %>%
        filter(X == max(X)) %>%
        as.matrix()
      
      center_to_sub <- st_coordinates(line_in_mach_dir)[, 1:2] %>%
        as.data.frame() %>%
        filter(X == min(X)) %>%
        as.matrix()
    }else{
      center_to_add <- st_coordinates(line_in_mach_dir)[, 1:2] %>%
        as.data.frame() %>%
        filter(X == min(X)) %>%
        as.matrix()
      
      center_to_sub <- st_coordinates(line_in_mach_dir)[, 1:2] %>%
        as.data.frame() %>%
        filter(X == max(X)) %>%
        as.matrix()
    }
    
    line_1 <-
      rbind(
        (center_to_add + move_vec_mach_dir*(line_shift*machine_number)) - move_vec_perp * 200,
        (center_to_add + move_vec_mach_dir*(line_shift*machine_number)) + move_vec_perp * 200
      ) %>%
      st_linestring() %>%
      st_sfc(crs = st_crs(grp_main)) %>%
      st_sf()
    
    line_9 <-
      rbind(
        (center_to_sub - move_vec_mach_dir*(line_shift*machine_number)) - move_vec_perp * 200,
        (center_to_sub - move_vec_mach_dir*(line_shift*machine_number)) + move_vec_perp * 200
      ) %>%
      st_linestring() %>%
      st_sfc(crs = st_crs(grp_main)) %>%
      st_sf()
    
    # find the placement for each cluster
    # making a line in the direction of the cluster
    # we want the machine figure to be closer to the line that it doesn't intersect with
    
    line_for_cluster <- vector()
    if(nrow(grp_diff) >= 2){
      for (i in 1:2){
        if(nrow(grp_diff) > 2){
          dir_cluster <- km$centers[i,]
        }else{
          dir_cluster <- grp_diff[i, ] %>%
            as.data.frame() %>%
            dplyr::select(med_dx, med_dy) %>%
            as.numeric(.)
          names(dir_cluster) <- c("X", "Y")
        }
        
        line_new <-
          rbind(
            area_cent,
            area_cent + scalar(dir_cluster) * 100
          ) %>%
          st_linestring() %>%
          st_sfc(crs = st_crs(grp_main)) %>%
          st_sf()
        
        if(nrow(st_intersection(line_new, line_1)) == 0){
          line_for_cluster[i] = "line_1"
        }else{
          line_for_cluster[i] = "line_9"
        }
      }
    }else{
      dir_cluster <- grp_diff[1, ] %>%
        as.data.frame() %>%
        dplyr::select(med_dx, med_dy) %>%
        as.numeric(.)
      names(dir_cluster) <- c("X", "Y")
      
      line_new <-
        rbind(
          area_cent,
          area_cent + scalar(dir_cluster) * 100
        ) %>%
        st_linestring() %>%
        st_sfc(crs = st_crs(grp_main)) %>%
        st_sf()
      
      if(nrow(st_intersection(line_new, line_1)) == 0){
        line_for_cluster[1] = "line_1"
      }else{
        line_for_cluster[1] = "line_9"
      }
    }
    
    
    #choose a direction and make a line that 
    
    machines <- list()
    for (i in 1:nrow(grp_diff)) {
      current_group <- grp_diff$group[i]
      current_cluster <- grp_diff$cluster[i]
      
      # first find the direction for the current group
      move_vec_dir <- scalar(c(grp_diff$med_dx[i], grp_diff$med_dy[i]))
      
      # then adjust the perpendicular line for the current group
      if (current_cluster == 1){
        line <- eval(parse(text = line_for_cluster[1]))
      }else{
        line <- eval(parse(text = line_for_cluster[2]))
      }
      
      # find the point in the current group that is closest to the line
      center_pt <- grp_main %>% filter(group == current_group) %>%
        .[st_nearest_feature(line, grp_main %>% filter(group == current_group)), ] %>%
        st_centroid() %>%
        st_coordinates()
      
      machine_poly <- make_machine_poly(width = width,
                                        height = conv_unit(10, "ft", "m"),
                                        center = center_pt,
                                        move_vec = move_vec_dir,
                                        crs = st_crs(int))
      
      machines[[i]] <- machine_poly
    }
    machine_polys <- do.call(rbind, machines)
    
  }
  
  return(machine_polys)
}

make_machine_poly <- function(width, height, center, move_vec, crs) {
  normalized_move_vec <- scalar(move_vec) # normalized direction aka normal vector
  perp_move_vec <- rotate_vec(normalized_move_vec, 90) # perpendicular vector to the normal vector
  
  width_in <- width * 0.4
  height_up <- height * 0.4
  
  # Create vertices
  middle_bottom <- center - normalized_move_vec * height / 2
  left_bottom <- middle_bottom - perp_move_vec * width / 2
  left_up <- left_bottom + normalized_move_vec * height
  left_in_bottom <- left_up + perp_move_vec * width_in
  left_in_up <- left_in_bottom + normalized_move_vec * height_up
  left_in_left <- left_in_up - perp_move_vec * height_up
  top_point <- center + normalized_move_vec * (height + height_up / 2)
  right_bottom <- middle_bottom + perp_move_vec * width / 2
  right_up <- right_bottom + normalized_move_vec * height
  right_in_bottom <- right_up - perp_move_vec * width_in
  right_in_up <- right_in_bottom + normalized_move_vec * height_up
  right_in_right <- right_in_up + perp_move_vec * height_up
  
  # Create machine sf
  polygon_sf <-
    list(
      rbind(
        middle_bottom,
        left_bottom,
        left_up,
        left_in_bottom,
        left_in_up, 
        left_in_left,
        top_point,
        right_in_right,
        right_in_up,
        right_in_bottom,
        right_up,
        right_bottom,
        middle_bottom
      )
    ) %>%
    st_polygon() %>%
    st_sfc(crs = crs) %>%
    st_sf()
  
  # polygon <- polygon_sf %>% st_coordinates(.) %>% # get coordinates of the polygon
  #   .[, -(3:4)] # remove the last two columns
  # 
  # # now use st_distance to measure the distance of the first two sides of the polygon
  # st_distance(st_point(polygon[1,]), st_point(polygon[2,]))
  # st_distance(st_point(polygon[2,]), st_point(polygon[3,]))
  # st_distance(st_point(polygon[3,]), st_point(polygon[4,]))
  # st_distance(st_point(polygon[4,]), st_point(polygon[5,]))
  # st_distance(st_point(polygon[5,]), st_point(polygon[6,]))
  # st_distance(st_point(polygon[5,]), st_point(polygon[6,]))
  
  return(polygon_sf)
}

scalar <- function(x) {x / sqrt(sum(x^2))}

rotate_vec <- function(vec, angle) {
  rotate_mat <- matrix(
    c(
      cos(pi * angle / 180),
      sin(pi * angle / 180),
      -sin(pi * angle / 180),
      cos(pi * angle / 180)
    ),
    nrow = 2
  )
  return(vec %*% rotate_mat)
}

tmap_machine <- function(machine_poly, type, int, use_td) {
  if(use_td == TRUE){
    NULL
  }else{
    tm_shape(machine_poly, bbox = st_bbox(int)) +
      tm_borders(col = "darkgray", lwd = 1) +
      tm_fill(col = if (type == "seed") {
        "lawngreen"
      } else if(type == "yield"){
        "#E69F00"
      }else{
        "#0072B2"
      },
      alpha = .5)
  }
}

# int <- figure_table$int[[4]]
# type <- figure_table$type[[4]]
# use_td <- figure_table$use_td[[4]]

tmap_data <- function(int, type, use_td, trial_info){
  if(use_td == TRUE){
    td <- here("Data/Growers", ffy, "TrialDesign", paste0("trial-design-", tolower(type), ".shp")) %>%
      st_read() %>% 
      st_transform_utm() %>%
      setnames(names(.), tolower(names(.))) %>%
      mutate(target = as.factor(eval(parse(text = paste0("tgt_", type)))))
    
    td <- st_intersection(td, int)
    
    tm_shape(td) +
      tm_polygons(
        col = "target",
        palette = if(type == "seed"){
          "Greens"
          }else{
            "Blues"
          },
        size = 0.1
      ) +
      tm_layout(legend.show = FALSE, bg.color = "#00000000")
  }else{
    tm_shape(int) +
      tm_dots(
        col = if(type == "seed"){
          "lawngreen"
        }else if(type == "yield"){
          "#E69F00"
        }else{
          "#0072B2"
        },
        size = 0.1
      )
  }
  
}

legend_machine <- function(type, use_td){
  if(use_td == TRUE){
    NULL
  }else{
    tm_add_legend(type = "symbol",
                  labels = if(type == "seed"){
                    "Planter"
                  }else if(type == "yield"){
                    "Harvester"
                  }else{
                    paste0(str_to_title(type), " Applicator")
                  },
                  col = if(type == "seed"){
                    "lawngreen"
                  }else if(type == "yield"){
                    "#E69F00"
                  }else{
                    "#0072B2"
                  },
                  shape = 15,
                  # size = 0.7,
                  alpha = 0.5,
                  border.col = "darkgray",
                  border.lwd = 1)
      # tm_layout(legend.text.size = 0.2)
  }
}

legend_data <- function(type, use_td, trial_info, int){
  if(use_td == TRUE){
    td <- here("Data/Growers", ffy, "TrialDesign", paste0("trial-design-", tolower(type), ".shp")) %>%
      st_read() %>% 
      st_transform_utm() %>%
      setnames(names(.), tolower(names(.))) %>%
      mutate(target = as.factor(eval(parse(text = paste0("tgt_", type)))))
    
    td <- st_intersection(td, int)
    
    tm_shape(td) +
      tm_polygons(
        col = "target",
        title = paste0('Target ', str_to_title(type), " Rate"),
        palette = if(type == "seed"){
          "Greens"
        }else{
          "Blues"
        }) +
      tm_layout(legend.only= T)
  }else{
    tm_add_legend(type = "symbol",
                  labels = if(type == "seed"){
                    "Planting Observation"
                  }else if(type == "yield"){
                    "Yield Observation"
                  }else{
                    paste0(str_to_title(type), " Observation")
                  },
                  col = if(type == "seed"){
                    "lawngreen"
                  }else if(type == "yield"){
                    "#E69F00"
                  }else{
                    "#0072B2"
                  },
                  shape = 16)
      # tm_layout(legend.text.size = 0.2)
  }
}



