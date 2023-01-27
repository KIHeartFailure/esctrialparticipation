edata <- edata %>%
  mutate(
    year = year(num_dmVisitdt),
    num_age_cat = factor(case_when(
      num_age < 60 ~ 1,
      num_age < 70 ~ 2,
      num_age < 80 ~ 3,
      num_age >= 80 ~ 4
    ), levels = 1:4, 
    labels = c("<60", "60-69", "70-79", ">=80")),
    num_dmBmi_cat = case_when(
      is.na(num_dmBmi) ~ NA_character_,
      num_dmBmi < 25 ~ "<25",
      num_dmBmi >= 25 ~ ">=25"
    ),
    num_dmBp1_cat = case_when(
      is.na(num_dmBp1) ~ NA_character_,
      num_dmBp1 < 100 ~ "<100",
      num_dmBp1 >= 100 ~ ">=100"
    ),
    num_dmBpm_cat = case_when(
      is.na(num_dmBpm) ~ NA_character_,
      num_dmBpm < 70 ~ "<70",
      num_dmBpm >= 70 ~ ">=70"
    ),
    num_dmhome_cat = recode_factor(num_dmhome, "Nursing home" = "Other"),
    num_dmSmoking_cat = factor(case_when(
      is.na(num_dmSmoking) ~ NA_real_,
      num_dmSmoking == "Current" ~ 2,
      TRUE ~ 1
    ), levels = 1:2, labels = c("Never/Former", "Current")),
    d_HFdiagnosis = case_when(
      num_dmHF == "No" ~ "<12mo",
      num_dmMonth %in% c("< 6 months", "6 - 12 months") ~ "<12mo",
      num_dmMonth %in% c("> 12 months") ~ ">12mo"
    ),
    num_dmEtio_c1 = relevel(num_dmEtio_c1, ref = "Non-ischemic heart disease"),
    num_dmEtio = factor(case_when(
      num_dmEtio == "Ischemic heart disease documented by coronary angiography" ~ "IHD doc by ca",
      num_dmEtio == "Ischemic heart disease not documented by coronary angiography" ~ "IHD not documented by ca",
      TRUE ~ as.character(num_dmEtio)
    )),
    num_dmDev_cat = factor(case_when(
      is.na(num_dmDev) ~ NA_real_,
      num_dmDev %in% c("No") ~ 1,
      num_dmDev %in% c("PM") ~ 2,
      num_dmDev %in% c("CRT-P", "CRT-D", "ICD") ~ 3
    ), levels = 1:3, labels = c("No device", "PM", "CRT/ICD")),
    num_Cre = coalesce(num_dcCre, num_opCre),
    # eGFR according to CKD-EPI 2021 https://www.nejm.org/doi/full/10.1056/NEJMoa2102953
    tmp_k = if_else(num_dmgender == "Female", 0.7, 0.9),
    tmp_a = if_else(num_dmgender == "Female", -0.241, -0.302),
    tmp_add = if_else(num_dmgender == "Female", 1.012, 1),
    num_CKDEPI = 142 * pmin(num_Cre / tmp_k, 1)^tmp_a * pmax(num_Cre / tmp_k, 1)^-1.200 * 0.9938^num_age * tmp_add,
    num_CKDEPI = if_else(num_CKDEPI == Inf, NA_real_, num_CKDEPI),
    num_CKDEPI_cat = factor(case_when(
      num_CKDEPI < 60 ~ 2,
      num_CKDEPI >= 60 ~ 1
    ),
    levels = 1:2,
    labels = c(">=60", "<60")
    ),
    num_Nyha = coalesce(num_dcNyha, num_opNyha),
    num_Nyha_cat = case_when(
      is.na(num_Nyha) ~ NA_character_,
      num_Nyha %in% c("NYHA I", "NYHA II") ~ "I-II",
      num_Nyha %in% c("NYHA III", "NYHA IV") ~ "III-IV"
    ),
    num_Nt = coalesce(num_dcNt, num_opNt),
    num_Nt_cat = case_when(
      is.na(num_Nt) ~ NA_character_,
      num_Nt < 1000 ~ "<1000",
      num_Nt >= 1000 ~ ">=1000"
    ),
    num_Hypop = coalesce(num_dcHypop, num_opHypop),
    num_Jvp = coalesce(num_dcJvp, num_opJvp),
    num_Hep = coalesce(num_dcHep, num_opHep),
    num_Oed = coalesce(num_dcOed, num_opOed),
    per_congestion = case_when(
      is.na(num_Jvp) | is.na(num_Hep) | is.na(num_Oed) ~ NA_character_,
      num_Jvp == "Yes" | num_Hep == "Yes" | num_Oed == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    ## x-ray
    num_Xrn = coalesce(num_dcXrn, num_opXrn),
    num_Xpu = coalesce(num_dcXpu, num_opXpu),
    pulm_congestion = if_else(num_Xrn == "Yes", "No", as.character(num_Xpu)),

    # ech-doppler
    num_Ef = coalesce(num_dcEf, num_opEf),
    num_Ef_cat = factor(case_when(
      is.na(num_Ef) ~ NA_real_,
      num_Ef < 41 ~ 1,
      num_Ef <= 49 ~ 2,
      num_Ef >= 50 ~ 3
    ),
    levels = 1:3, labels = c("<=40%", "41-49%", ">=50%")
    ),
    num_dmEflp_cat = factor(case_when(
      is.na(num_dmEflp) ~ NA_real_,
      num_dmEflp < 41 ~ 1,
      num_dmEflp <= 49 ~ 2,
      num_dmEflp >= 50 ~ 3
    ),
    levels = 1:3, labels = c("<=40%", "41-49%", ">=50%")
    ),
    num_Echo = coalesce(num_dcEcho, num_opEcho),
    num_MitReg = coalesce(num_dcMitReg, num_opMitReg),

    num_Crt = coalesce(num_dcCrt, num_opCrt),
    num_Icd = coalesce(num_dcIcd, num_opIcd),
    
    ## meds
    num_mdACE_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdACEd,
      num_dmPtype == "Outpatient" ~ num_mdACEh
    ),
    num_mdACE_c2_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdACEd_c2,
      num_dmPtype == "Outpatient" ~ num_mdACEh_c2
    ),
    num_mdACEdo_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdACEddo,
      num_dmPtype == "Outpatient" ~ num_mdACEhdo
    ),
    num_mdAT_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdATd,
      num_dmPtype == "Outpatient" ~ num_mdATh
    ),
    num_mdAT_c2_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdATd_c2,
      num_dmPtype == "Outpatient" ~ num_mdATh_c2
    ),
    num_mdATdo_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdATddo,
      num_dmPtype == "Outpatient" ~ num_mdAThdo
    ),
    num_mdARNI_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdARNId,
      num_dmPtype == "Outpatient" ~ num_mdARNIh
    ),
    num_mdARNIdo_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdARNIddo,
      num_dmPtype == "Outpatient" ~ num_mdARNIhdo
    ),
    d_rasiarni_after = case_when(
      is.na(num_mdACE_after) | is.na(num_mdAT_after) ~ NA_character_,
      num_mdACE_after == "Yes" | num_mdAT_after == "Yes" | num_mdARNI_after == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    d_ACEdose_eqCaptopril_after = case_when(
      num_mdACE_c2_after == "Captopril" ~ num_mdACEdo_after / 150 * 150,
      num_mdACE_c2_after == "Ramipril" ~ num_mdACEdo_after / 10 * 150,
      num_mdACE_c2_after == "Enalapril" ~ num_mdACEdo_after / 40 * 150,
      num_mdACE_c2_after == "Perindopril" ~ num_mdACEdo_after / 16 * 150,
      num_mdACE_c2_after == "Lisinopril" ~ num_mdACEdo_after / 40 * 150,
      num_mdACE_c2_after == "Fosinopril" ~ num_mdACEdo_after / 40 * 150
    ),
    d_ATdose_eqCaptopril_after = case_when(
      num_mdAT_c2_after == "Candesartan" ~ num_mdATdo_after / 32 * 150,
      num_mdAT_c2_after == "Losartan" ~ num_mdATdo_after / 150 * 150,
      num_mdAT_c2_after == "Valsartan" ~ num_mdATdo_after / 320 * 150
    ),
    d_ARNIdose_eqCaptopril_after = case_when(
      num_mdARNI_after == "Yes" ~ num_mdARNIdo_after / 400 * 150
    ),
    d_rasiarnimaxdose_after = pmax(d_ACEdose_eqCaptopril_after, d_ATdose_eqCaptopril_after, d_ARNIdose_eqCaptopril_after, na.rm = T),
    d_rasiarnidosetarget_after = factor(case_when(
      d_rasiarnimaxdose_after < 150 / 2 ~ 1,
      d_rasiarnimaxdose_after < 150 ~ 2,
      d_rasiarnimaxdose_after >= 150 ~ 3
    ), levels = c(1:3), labels = c("<50%", "50-<100%", "100%")),

    # aldo
    num_mdAL_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdALd,
      num_dmPtype == "Outpatient" ~ num_mdALh
    ),
    num_mdALdo_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdALddo,
      num_dmPtype == "Outpatient" ~ num_mdALhdo
    ),
    d_ALdosetarget_after = factor(case_when(
      num_mdALdo_after < 50 / 2 ~ 1,
      num_mdALdo_after < 50 ~ 2,
      num_mdALdo_after >= 50 ~ 3
    ), levels = c(1:3), labels = c("<50%", "50-<100%", "100%")),

    ## bbl
    num_mdBB_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdBBd,
      num_dmPtype == "Outpatient" ~ num_mdBBh
    ),
    num_mdBB_c2_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdBBd_c2,
      num_dmPtype == "Outpatient" ~ num_mdBBh_c2
    ),
    num_mdBBdo_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdBBddo,
      num_dmPtype == "Outpatient" ~ num_mdBBhdo
    ),
    d_BBdose_eqCarvedilol_after = case_when(
      num_mdBB_c2_after == "Bisoprolol" ~ num_mdBBdo_after / 10 * 50,
      num_mdBB_c2_after == "Metoprolol" ~ num_mdBBdo_after / 200 * 50,
      num_mdBB_c2_after == "Nebivolol" ~ num_mdBBdo_after / 10 * 50,
      num_mdBB_c2_after == "Carvedilol" ~ num_mdBBdo_after / 50 * 50
    ),
    d_BBdosetarget_after = factor(case_when(
      d_BBdose_eqCarvedilol_after < 10 / 2 ~ 1,
      d_BBdose_eqCarvedilol_after < 10 ~ 2,
      d_BBdose_eqCarvedilol_after >= 10 ~ 3
    ), levels = c(1:3), labels = c("<50%", "50-<100%", "100%")),

    # Meds at follow-up

    f1_ACE = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1ACEh)
    ),
    f1_AT = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1ATh)
    ),
    f1_ARNI = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1ARNI)
    ),
    f1_rasiarni = case_when(
      is.na(f1_ACE) | is.na(f1_AT) ~ NA_character_,
      f1_ACE == "Yes" | f1_AT == "Yes" | f1_ARNI == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    f1_ACEdose_eqCaptopril = case_when(
      num_f1ACEh_c2 == "Captopril" ~ num_f1ACEhdo / 150 * 150,
      num_f1ACEh_c2 == "Ramipril" ~ num_f1ACEhdo / 10 * 150,
      num_f1ACEh_c2 == "Enalapril" ~ num_f1ACEhdo / 40 * 150,
      num_f1ACEh_c2 == "Perindopril" ~ num_f1ACEhdo / 16 * 150,
      num_f1ACEh_c2 == "Lisinopril" ~ num_f1ACEhdo / 40 * 150,
      num_f1ACEh_c2 == "Fosinopril" ~ num_f1ACEhdo / 40 * 150
    ),
    f1_ATdose_eqCaptopril = case_when(
      num_f1ATh_c2 == "Candesartan" ~ num_f1AThdo / 32 * 150,
      num_f1ATh_c2 == "Losartan" ~ num_f1AThdo / 150 * 150,
      num_f1ATh_c2 == "Valsartan" ~ num_f1AThdo / 320 * 150
    ),
    f1_ARNIdose_eqCaptopril = case_when(
      num_f1ARNI == "Yes" ~ num_f1ARNIdo / 400 * 150
    ),
    f1_rasiarnimaxdose = pmax(f1_ACEdose_eqCaptopril, f1_ATdose_eqCaptopril, f1_ARNIdose_eqCaptopril, na.rm = T),
    f1_rasiarnidosetarget = factor(case_when(
      f1_rasiarnimaxdose < 150 / 2 ~ 1,
      f1_rasiarnimaxdose < 150 ~ 2,
      f1_rasiarnimaxdose >= 150 ~ 3
    ), levels = c(1:3), labels = c("<50%", "50-<100%", "100%")),
    f1_AL = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1ALh)
    ),
    f1_ALdosetarget = factor(case_when(
      num_f1ALhdo < 50 / 2 ~ 1,
      num_f1ALhdo < 50 ~ 2,
      num_f1ALhdo >= 50 ~ 3
    ), levels = c(1:3), labels = c("<50%", "50-<100%", "100%")),
    f1_BB = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1BBh)
    ),
    f1_BBdose_eqCarvedilol = case_when(
      num_f1BBh_c2 == "Bisoprolol" ~ num_f1BBhdo / 10 * 50,
      num_f1BBh_c2 == "Metoprolol" ~ num_f1BBhdo / 200 * 50,
      num_f1BBh_c2 == "Nebivolol" ~ num_f1BBhdo / 10 * 50,
      num_f1BBh_c2 == "Carvedilol" ~ num_f1BBhdo / 50 * 50
    ),
    f1_BBdosetarget = factor(case_when(
      f1_BBdose_eqCarvedilol < 50 / 2 ~ 1,
      f1_BBdose_eqCarvedilol < 50 ~ 2,
      f1_BBdose_eqCarvedilol >= 50 ~ 3
    ), levels = c(1:3), labels = c("<50%", "50-<100%", "100%")),
    num_dmPtype = recode_factor(num_dmPtype,
      Hospital = "Inpatient",
      Outpatient = "Outpatient"
    ),
    num_dmPtype = relevel(num_dmPtype, ref = "Outpatient"),

    # Outcomes
    enddtm = coalesce(num_f1DeathDt, num_f1contDt),
    startdtm = coalesce(num_dcDischdt, num_dmVisitdt),
    outtime_death = as.numeric(enddtm - startdtm),

    # remove if neg follow-up times
    outtime_death = if_else(outtime_death < 0, NA_real_, outtime_death),
    out_death = case_when(
      num_f1vital == "Alive" ~ 0,
      num_f1vital == "Dead" ~ 1
    ),

    # HF hosp
    out_hosphf = case_when(
      num_f1lost != "No" ~ NA_real_,
      num_f1hosp1cs == "HF" |
        num_f1hosp2cs == "HF" |
        num_f1hosp3cs == "HF" |
        num_f1hosp4cs == "HF" |
        num_f1hosp5cs == "HF" ~ 1,
      TRUE ~ 0
    ),
    out_hosphfdtm = case_when(
      num_f1hosp1cs == "HF" ~ num_f1hosp1dt,
      num_f1hosp2cs == "HF" ~ num_f1hosp2dt,
      num_f1hosp3cs == "HF" ~ num_f1hosp3dt,
      num_f1hosp4cs == "HF" ~ num_f1hosp4dt,
      num_f1hosp5cs == "HF" ~ num_f1hosp5dt
    ),
    outtime_hosphf = as.numeric(out_hosphfdtm - startdtm),
    # impute hf hosp date
    outtime_hosphf = ifelse(out_hosphf == 1 & (is.na(outtime_hosphf) | outtime_hosphf < 0), outtime_death / 2, outtime_hosphf),
    outtime_hosphf = pmin(outtime_hosphf, outtime_death, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.character), as.factor)) %>%
  select(-starts_with("tmp_"))
