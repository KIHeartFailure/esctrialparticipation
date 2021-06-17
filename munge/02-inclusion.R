

# Primary criteria --------------------------------------------------------

flow <- c(paste0("Number of patients in ESC "), nrow(esc))

edata <- esc %>%
  filter(!is.na(num_dmRCT))
flow <- rbind(flow, c("Non-missing information on clinical trial participation", nrow(edata)))

edata <- edata %>%
  mutate(
    enddtm = coalesce(num_f1DeathDt, num_f1contDt),
    startdtm = coalesce(num_dcDischdt, num_dmVisitdt),
    outtime_death = as.numeric(enddtm - startdtm),
    survpop = num_f1lost == "No" & outtime_death >= 0 & !is.na(outtime_death)
  )

flow <- rbind(flow, c(
  ". Discharged alive and not lost to follow-up and not negative follow-up times (used in long-term outcome analysis)",
  nrow(edata %>% filter(survpop))
))

colnames(flow) <- c("Criteria", "N")
