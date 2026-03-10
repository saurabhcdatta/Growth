# After the full_join, force ALL _cu030 suffix columns to match their counterparts
mergedCU_data <- main_data %>%
  full_join(cu030_data, by = "join_number", suffix = c("", "_cu030"))

# Cast each _cu030 column to match the type of its primary column
for (col in columns) {
  cu030_col <- paste0(col, "_cu030")
  if (cu030_col %in% names(mergedCU_data)) {
    target_type <- class(mergedCU_data[[col]])
    mergedCU_data[[cu030_col]] <- switch(target_type,
                                         "character" = as.character(mergedCU_data[[cu030_col]]),
                                         "numeric"   = as.numeric(mergedCU_data[[cu030_col]]),
                                         "integer"   = as.integer(mergedCU_data[[cu030_col]]),
                                         mergedCU_data[[cu030_col]]  # fallback: leave as-is
    )
  }
}

# Now coalesce safely
columns <- c("cu_number", "cu_name", "state_fips", "actual_state")

mergedCU_data <- mergedCU_data %>%
  mutate(across(
    all_of(columns),
    ~ coalesce(., get(paste0(cur_column(), "_cu030")))
  )) %>%
  select(-ends_with("_cu030")) %>%
  filter(!(mainflag != 1 & cu030flag == 1 & status == "I")) %>%
  select(-mainflag, -cu030flag)