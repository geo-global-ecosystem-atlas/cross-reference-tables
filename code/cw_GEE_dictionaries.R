# Script to wrangle out crosswalk fields into GEE script

library(tidyverse)
library(googlesheets4)
rm(list=ls())


crosswalk <- read_sheet("https://docs.google.com/spreadsheets/d/1GciZm_1l7q_P_2V7rjKcQXCTBzYogo5MpqruoKwim_o/edit#gid=32989082", sheet = "Crosswalk_MASTER") |> 
  mutate(in_class_value = map(in_class_value, ~ifelse(is.null(.x), NA, .x)) |>  unlist()) |>  # In-class vals mixed var types - need unlisting
  mutate(efg_code = map(efg_code, ~ifelse(is.null(.x), NA, .x)) |>  unlist()) |>  # so does efg_code
  filter(status == "final" |
           status == "Final")



crosswalk <- crosswalk |> 
  mutate(in_class_value = case_when(
    is.na(as.numeric(in_class_value)) ~ paste0("'", in_class_value, "'"),  # Wrap strings in quotes
    TRUE ~ in_class_value  # Keep numeric values as is
  ))

# Split
crosswalk  |>  
  split(crosswalk$data_id_code)  |> 
  list2env(envir = globalenv())


rm(crosswalk)


cw_names <- ls()  # Adjust the pattern to match the actual naming

# Function to process each dataframe and generate the JavaScript dictionary
process_dataframe <- function(df_name) {
  
  # Access the dataframe using get()
  df <- get(df_name)
  
  # The dataframe name becomes the data_id_code
  data_id_code <- df_name

  # Generate the ee_asset_id using the band_layer_name field
  ee_asset_id <- paste0("projects/UQ_intertidal/gee-geo-atlas/open-datasets/jcu/", df$data_id_code[1])
  
  # Extract the fields row-wise, keeping NA and repeated values
  in_class_field_name <- df$in_class_field_name
  in_class_value <- df$in_class_value
  out_class_value <- df$out_class_value
  efg_name <- df$efg_name
  efg_code <- df$efg_code
  
  # Generate the JavaScript dictionary content with aligned values row by row
  js_content <- paste0(
    "//", data_id_code, "\n",
    "var ", data_id_code, " = {\n",
    "  data_id_code: '", data_id_code, "',\n",
    "  ee_asset_id: '", ee_asset_id, "',\n",
    "  in_class_field_name: [", paste0("'", in_class_field_name, "'", collapse = ", "), "],\n",
    "  in_class_value: [", paste0(in_class_value, collapse = ", "), "],\n",
    "  out_class_value: [", paste0(out_class_value, collapse = ", "), "],\n",
    "  efg_name: [", paste0("'", efg_name, "'", collapse = ", "), "],\n",
    "  efg_code: [", paste0("'", efg_code, "'", collapse = ", "), "]\n",
    "};\n"
  )
  
  return(js_content)
}

# Process all dataframes and combine the JavaScript content
combined_js_content <- cw_names %>%
  map_chr(process_dataframe) %>%
  paste(collapse = "\n\n")

# Write the combined result to a markdown (.md) file
write(combined_js_content, file = "combined_output.md")

# View the combined JavaScript content (optional)
cat(combined_js_content)
