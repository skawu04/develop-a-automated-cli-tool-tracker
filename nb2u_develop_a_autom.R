# nb2u_develop_a_autom.R

# Automated CLI Tool Tracker

# Packages
library( readr )
library( stringr )

# Data Model
tracker_data <- tibble(
  id = integer(),
  tool_name = character(),
  description = character(),
  version = character(),
  last_updated = date(),
  frequency = character(),
  command = character()
)

# CLI Tracker Function
cli_tracker <- function(tool_name, description, version, frequency, command) {
  new_tool <- tibble(
    id = max(tracker_data$id, na.rm = TRUE) + 1,
    tool_name = tool_name,
    description = description,
    version = version,
    last_updated = Sys.Date(),
    frequency = frequency,
    command = command
  )
  
  tracker_data <<- tracker_data %>% add_row(new_tool)
  
  print("Tool added successfully!")
}

# CLI Tracker Updater Function
cli_updater <- function(id, tool_name, description, version, frequency, command) {
  tracker_data <<- tracker_data %>%
    mutate(
      tool_name = ifelse(id == !!id, tool_name, tool_name),
      description = ifelse(id == !!id, description, description),
      version = ifelse(id == !!id, version, version),
      last_updated = ifelse(id == !!id, Sys.Date(), last_updated),
      frequency = ifelse(id == !!id, frequency, frequency),
      command = ifelse(id == !!id, command, command)
    )
  
  print("Tool updated successfully!")
}

# CLI Tracker Remover Function
cli_remover <- function(id) {
  tracker_data <<- tracker_data %>% filter(id != !!id)
  
  print("Tool removed successfully!")
}

# CLI Tracker Viewer Function
cli_viewer <- function() {
  print(tracker_data)
}