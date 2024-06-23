# Load the necessary package
library(fredr)

# Set your FRED API key
fredr_set_key("dfdf7dff8e1f85865d60e113b4f67426")

# The series ID for the household debt service ratio (DSR) is "TDSP"
dsr_data <- fredr(series_id = "TDSP", 
                  observation_start = as.Date("1995-01-01"), 
                  observation_end = as.Date("2023-12-31"))

# Display the first few rows of the data
head(dsr_data)
