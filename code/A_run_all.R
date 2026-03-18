# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Get all R scripts in the current directory
r_scripts <- list.files(pattern = "\\.[rR]$")

# Exclude this script to avoid infinite recursion
r_scripts <- setdiff(r_scripts, "A_run_all.R")

# Source each script
for (script in r_scripts) {
  message("Running ", script, "...")
  source(script)
}

message("All scripts executed! Check the 'plots' folder for your PDFs.")