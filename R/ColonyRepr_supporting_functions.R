#' Extracts data from the SEATRACK DB and prepares it for the ColonyRepr functions
#'
#' This function will extract gls positional data for the specified species-colony. An active connection with the data base is required for this.
#' It will then prepare the extracted data to be used in the ColonyRepr functions
#'
#' @param species specify the species you'd like to extract the data from
#' @param colony specify the colony you'd like to select the data from
#'
#' @return position data from the specified species-colony, ready to be inserted into ColonyRepr functions
#' @concept ColonyRepr
#'
#' @export

Prep_SEATRACK_data_for_ColonyRepr <- function(species,
                                              colony){

  spec_col_positions <- seatrackR::getPositions(species = species,
                                                colony = colony)

  ind <- seatrackR::getIndividInfo(colony = colony)

  # all sessions where birds were tagged as pullus
  pul <- ind[ind$status_age %in% c("pullus", "chick") & ind$eventType %in% "Deployment",]

  ind_sub <- ind[!is.na(ind$eventType),]
  ind_sub$status_age <- "adult"
  ind_sub$status_age[ind_sub$session_id %in% pul$session_id] <- "juvenile"

  adult_sessions <- ind_sub %>%
    dplyr::filter(status_age == "adult") %>%
    pull(session_id)

  #Match retrieval years to sessions
  SEATRACK_logger_info <- seatrackR::getLoggerInfo() %>%
    mutate(retrieval_year = year(retrieval_date)) %>%
    relocate(retrieval_year)

  Year_retrieved <- SEATRACK_logger_info %>%
    dplyr::select(session_id, retrieval_year)


  spec_col_positions2 <- spec_col_positions %>%
    filter(session_id %in% adult_sessions) %>%  # Only include adult tracks
    mutate(Year = year(date_time),
           month = month(date_time),
           W_Year = case_when(month <= 6 ~ paste(Year-1,Year, sep = "/"), # determine which winter each position relates to
                              month > 6 ~ paste(Year,Year+1, sep = "/")),
           individ_Year = paste(individ_id, W_Year, sep = "#") # create a unique identifier per migration track, linked to and individual and winter
    ) %>%
    dplyr::select(session_id,
                  individ_id,
                  species,
                  colony,
                  lon,
                  lat,
                  sex,
                  col_lon,
                  col_lat,
                  Year,
                  month,
                  date_time,
                  W_Year,
                  individ_Year
    ) %>%
    rename(longitude = lon,
           latitude = lat,
           lat_colony = col_lat,
           lon_colony = col_lon
    )%>%
    left_join(Year_retrieved, by = "session_id") # include information on when birds were retrieved

  return(spec_col_positions2)
}






#' Function to extract desired data from the SEATRACK database and fully run the colony representativeness analysis
#'
#' This is a wrapper function around `Prep_SEATRACK_data_for_ColonyRepr()` and the analysis pipeline.
#' It will thus extract the specified gls positional data and plug it into the pipeline that will produce
#' a listed output with various elements that can be used to indicate captured colony representativeness.
#' An active connection with the SEATRACK data base is required.
#'
#' @param species Specify species of interest (numeric)
#' @param colony Specify colony of interest (numeric)
#' @param Month_of_interest Specify month of interest (numeric)
#' @param KDE_contours_of_interest Specify KDE contours of interest (can be a vector)
#' @param smoothing_parameter Specify a smoothing parameter to be used in the UD calculation
#' @param N_iterations Specify the number of iterations you'd like to perform when combining surface areas (the mean will be used in fitting the MM-curve)
#'
#' @return A list with all components of the colony representativeness analysis
#' @concept ColonyRepr
#' @export
#'
run_ColonyRepr <- function(species,
                           colony,
                           Month_of_interest = 12,
                           KDE_contours_of_interest = seq(from = 25, to = 95),
                           smoothing_parameter = NULL,
                           N_iterations = 20){

  result_of_iteration <- tryCatch({

    # 1. Extract the positional data from the SEATRACK DB
    dat <- Prep_SEATRACK_data_for_ColonyRepr(species = species,
                                             colony = colony)
    Month_abb <- month.abb[Month_of_interest]


    # This whole function can't run for the kittiwakes from Kara Gate due to some issues with the grid used in the UD calculation
    if(species == "Black-legged kittiwake" & colony == "Kara Gate"){

      final_result <- "Can't do Kara Gate kittiwakes..."
      named_output <- list(final_result)

      # Assign the colony name to this outer list element.
      names(named_output) <- paste(species,colony,Month_abb, sep = "#")

      # Return the *named* outer list.
      return(named_output)

    }

    # 2. Filter Tracks
    Tracks <- ColonyRepr::KDE_filter_tracks_fun(
      dat,
      month. = Month_of_interest
    )

    # If minimum requirements are not met, return a message
    if (inherits(Tracks, "character")) {

      final_result <- "Not enough tracks available"
      named_output <- list(final_result)

      # Assign species-colony information to the name of this list
      names(named_output) <- paste(species,colony,Month_abb, sep = "#")

      # Return the named list.
      return(named_output)

    }

    # 3. Determine Utilization Density (UD) per track
    UD_list <- list()
    if(is.null(smoothing_parameter)){
      h_par <- mean_smooths %>%
        filter(species == species) %>%
        pull(mean_h)}
    else{h_par <- smoothing_parameter}

    for (i in Tracks$Tracks) {
      UD_list[[i]] <- ColonyRepr::KDE_UD_fun(
        Tracks$Dataset %>% filter(individ_Year == i),
        h_par
      )
    }

    # Run the rest of the pipeline per contour of interest (foreach loop):
    named_output <- foreach(
      KDE_contour = KDE_contours_of_interest,
      # Iterate over KDE_contours_of_interest
      .combine = 'c',
      # Combine results into a list
      .packages = c("tidyverse", "ColonyRepr")
      # Specify packages needed inside the loop
    ) %do% {
      #KDE_contour = 25

      # 4. Determine individual contours
      contours_list <- lapply(UD_list, FUN = function(x) {
        ColonyRepr::KDE_contours_fun(
          UD = x$UD,
          projection = x$projection,
          KDE_contour = KDE_contour,
          species_colony_data = Tracks$Dataset,
          h_par
        )
      })

      # 5. Combine individual-level kernels
      Combine <- ColonyRepr::KDE_combine_areas_fun(
        contours_sf = bind_rows(contours_list),
        tot_loc_data = Tracks$Dataset,
        n_iterations = N_iterations
      )

      # 6. Fit Michaelis-Menten (Saturation Curve)
      result <- ColonyRepr::KDE_saturation_curve(Combine$Kernel_areas)
      final_result <- list()

      final_result$Kernel_areas_tot <- result$Kernel_areas
      final_result$Kernel_areas_summarised <- result$Kernel_areas_summarised

      # 7. Restructure and add metadata to the final result list
      final_result$Tracks <- Tracks$Dataset
      final_result$KDE_contours_sf <- bind_rows(contours_list)
      final_result$colony_projection <- Combine$colony_projection

      spec_col_month_kde <- list()
      spec_col_month_kde[[paste(species,colony,Month_abb,KDE_contour, sep = "#")]] <- final_result

      return(spec_col_month_kde)
    }
    # 4. Return the *named* outer list.
    return(named_output)

  }, error = function(e) {
    # --- Error Handling (if the task fails) ---

    error_list <- list(error = e$message)

    names(error_list) <- paste(species,colony,Month_abb, sep = "#")
    # Return a specific placeholder object instead of crashing
    return(error_list)
  })

  # The loop continues to the next iteration because 'tryCatch' returned a value.
  return(result_of_iteration)

}
