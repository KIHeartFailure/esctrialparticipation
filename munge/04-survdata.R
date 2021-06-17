
# Create survdata used in long-term outcome analysis ----------------------

survdata <- edata %>%
  filter(survpop)
