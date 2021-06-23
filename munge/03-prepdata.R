edata <- edata %>%
  mutate(
    num_age_cat = case_when(
      num_age < 65 ~ "<65",
      num_age >= 65 ~ ">=65"
    ),
    num_dmBmi_cat = case_when(
      is.na(num_dmBmi) ~ NA_character_,
      num_dmBmi < 25 ~ "<25",
      num_dmBmi >= 25 ~ ">=25"
    ),
    num_dmhome = if_else(num_dmhome == "Other", "Other situation", as.character(num_dmhome)),
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
    ), levels = 1:3, labels = c("No device", "PM", "CT/ICD")),

    # eGFR according to CKD-EPI
    sex = recode(num_dmgender, "Male" = 1, "Female" = 0),
    ethnicity = case_when(
      is.na(num_dmEthnic) ~ NA_real_,
      num_dmEthnic == "Black" ~ 1,
      TRUE ~ 0
    ),
    num_Cre = coalesce(num_dcCre, num_opCre),
    num_CKDEPI = nephro::CKDEpi.creat(num_Cre, sex, num_age, ethnicity),
    num_CKDEPI = if_else(num_CKDEPI == Inf, NA_real_, num_CKDEPI),
    num_CKDEPI_cat = factor(case_when(
      num_CKDEPI < 60 ~ 2,
      num_CKDEPI >= 60 ~ 1
    ),
    levels = 1:2,
    labels = c(">=60", "<60")
    ),
    # num_Hb = coalesce(num_dcHb, num_opHb),
    num_Nyha = coalesce(num_dcNyha, num_opNyha),
    num_Nt = coalesce(num_dcNt, num_opNt),
    # num_Mr = coalesce(num_dcMr, num_opMr),
    num_Hypop = coalesce(num_dcHypop, num_opHypop),
    num_Jvp = coalesce(num_dcJvp, num_opJvp),
    num_Hep = coalesce(num_dcHep, num_opHep),
    num_Oed = coalesce(num_dcOed, num_opOed),
    per_congestion = case_when(
      is.na(num_Jvp) | is.na(num_Hep) | is.na(num_Oed) ~ NA_character_,
      num_Jvp == "Yes" | num_Hep == "Yes" | num_Oed == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    # tab 2

    ## ecg
    num_Ecg = coalesce(num_dcEcg, num_opEcg),
    num_Ryth = coalesce(num_dcRyth, num_opRyth),
    num_QrsD = coalesce(num_dcQrsD, num_opQrsD),
    num_Qt = coalesce(num_dcQt, num_opQt),
    num_Lbbb = coalesce(num_dcLbbb, num_opLbbb),
    num_Lvh2 = coalesce(num_dcLvh2, num_opLvh2),

    ## x-ray
    num_Xr = coalesce(num_dcXr, num_opXr),
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
    levels = 1:3, labels = c("<41%", "41-49%", ">=50%")
    ),
    num_dmEflp_cat = factor(case_when(
      is.na(num_dmEflp) ~ NA_real_,
      num_dmEflp < 41 ~ 1,
      num_dmEflp <= 49 ~ 2,
      num_dmEflp >= 50 ~ 3
    ),
    levels = 1:3, labels = c("<41%", "41-49%", ">=50%")
    ),
    num_Echo = coalesce(num_dcEcho, num_opEcho),
    num_Ef = coalesce(num_dcEf, num_opEf),
    num_Lvdd = coalesce(num_dcLvdd, num_opLvdd),
    num_Lvh = coalesce(num_dcLvh, num_opLvh),
    # num_Ladim = coalesce(num_dcLadim, num_opLadim),
    num_Rsp = coalesce(num_dcRsp, num_opRsp),
    num_MitReg = coalesce(num_dcMitReg, num_opMitReg),
    num_AorSte = coalesce(num_dcAorSte, num_opAorSte),
    num_AorReg = coalesce(num_dcAorReg, num_opAorReg),
    num_TriCus = coalesce(num_dcTriCus, num_opTriCus),

    ## Excercice test
    num_ExInab = coalesce(num_dcExInab, num_opExInab),
    num_Cycle = coalesce(num_dccycle, num_opcycle),
    num_Tread = coalesce(num_dctread, num_optread),
    num_VO2 = coalesce(num_dcVO2, num_opVO2),
    num_Walk = coalesce(num_dcWalk, num_opWalk),

    ## holter monitoring
    num_Hol = coalesce(num_dcHol, num_opHol),
    # num_Hr = coalesce(num_dcHr, num_opHr),
    # num_Pvc = coalesce(num_dcPvc, num_opPvc),
    num_Uvt = coalesce(num_dcUvt, num_opUvt),
    num_Svt = coalesce(num_dcSvt, num_opSvt),
    # num_Afib = coalesce(num_dcAfib, num_opAfib),

    ## other
    num_Cora = coalesce(num_dcCora, num_opCora),
    num_Cct = coalesce(num_dcCct, num_opCct),
    num_Pci = coalesce(num_dcPci, num_opPci),
    num_Eps = coalesce(num_dcEps, num_opEps),
    num_Transab = coalesce(num_dcTransab, num_opTransab),
    num_Cardiov = coalesce(num_dcCardiov, num_opCardiov),
    num_RhCath = coalesce(num_dcRhCath, num_opRhCath),
    num_Mysc = coalesce(num_dcMysc, num_opMysc),
    num_EndBi = coalesce(num_dcEndBi, num_opEndBi),
    num_Iapb = coalesce(num_dcIapb, num_opIapb),
    num_Crt = coalesce(num_dcCrt, num_opCrt),
    num_Icd = coalesce(num_dcIcd, num_opIcd),
    num_Score = coalesce(num_dcScore, num_opScore),

    ## meds

    d_rasi_prior = case_when(
      is.na(num_mdACEp) | is.na(num_mdATp) ~ NA_character_,
      num_mdACEp == "Yes" | num_mdATp == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    d_rasiarni_prior = case_when(
      is.na(num_mdACEp) | is.na(num_mdATp) ~ NA_character_,
      num_mdACEp == "Yes" | num_mdATp == "Yes" | num_mdARNIp == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    d_ACEdose_eqCaptopril_prior = case_when(
      num_mdACEp_c2 == "Captopril" ~ num_mdACEpdo / 150 * 150,
      num_mdACEp_c2 == "Ramipril" ~ num_mdACEpdo / 10 * 150,
      num_mdACEp_c2 == "Enalapril" ~ num_mdACEpdo / 40 * 150,
      num_mdACEp_c2 == "Perindopril" ~ num_mdACEpdo / 16 * 150,
      num_mdACEp_c2 == "Lisinopril" ~ num_mdACEpdo / 40 * 150,
      num_mdACEp_c2 == "Fosinopril" ~ num_mdACEpdo / 40 * 150
    ),
    d_ATdose_eqCaptopril_prior = case_when(
      num_mdATp_c2 == "Candesartan" ~ num_mdATpdo / 32 * 150,
      num_mdATp_c2 == "Losartan" ~ num_mdATpdo / 150 * 150,
      num_mdATp_c2 == "Valsartan" ~ num_mdATpdo / 320 * 150
    ),
    d_ARNIdose_eqCaptopril_prior = case_when(
      num_mdARNIp == "Yes" ~ num_mdARNIpdo / 400 * 150
    ),
    d_rasiarnimaxdose_prior = pmax(d_ACEdose_eqCaptopril_prior, d_ATdose_eqCaptopril_prior, d_ARNIdose_eqCaptopril_prior, na.rm = T),
    d_rasiarnidosetarget_prior = factor(case_when(
      d_rasiarnimaxdose_prior < 150 / 2 ~ 1,
      d_rasiarnimaxdose_prior < 150 ~ 2,
      d_rasiarnimaxdose_prior >= 150 ~ 3
    ), levels = c(1:3), labels = c("<50%", "50-<100%", "100%")),
    num_mdACE_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdACEd,
      num_dmPtype == "Outpatient" ~ num_mdACEh
    ),
    num_mdACE_c2_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdACEd_c2,
      num_dmPtype == "Outpatient" ~ num_mdACEh_c2
    ),
    num_mdACE_aftern = case_when(
      num_dmPtype == "Hospital" ~ num_mdACEdn,
      num_dmPtype == "Outpatient" ~ num_mdACEhn
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
    num_mdAT_aftern = case_when(
      num_dmPtype == "Hospital" ~ num_mdATdn,
      num_dmPtype == "Outpatient" ~ num_mdAThn
    ),
    num_mdATdo_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdATddo,
      num_dmPtype == "Outpatient" ~ num_mdAThdo
    ),
    num_mdARNI_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdARNId,
      num_dmPtype == "Outpatient" ~ num_mdARNIh
    ),
    num_mdARNI_aftern = case_when(
      num_dmPtype == "Hospital" ~ num_mdARNIdn,
      num_dmPtype == "Outpatient" ~ num_mdARNIhn
    ),
    num_mdARNIdo_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdARNIddo,
      num_dmPtype == "Outpatient" ~ num_mdARNIhdo
    ),
    d_rasi_after = case_when(
      is.na(num_mdACE_after) | is.na(num_mdAT_after) ~ NA_character_,
      num_mdACE_after == "Yes" | num_mdAT_after == "Yes" ~ "Yes",
      TRUE ~ "No"
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
    num_mdAL_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdALd,
      num_dmPtype == "Outpatient" ~ num_mdALh
    ),
    num_mdAL_aftern = case_when(
      num_dmPtype == "Hospital" ~ num_mdALdn,
      num_dmPtype == "Outpatient" ~ num_mdALhn
    ),

    ## bbl
    d_BBdose_eqCarvedilol_prior = case_when(
      num_mdBBp_c2 == "Bisoprolol" ~ num_mdBBddo / 10 * 50,
      num_mdBBp_c2 == "Metoprolol" ~ num_mdBBddo / 200 * 50,
      num_mdBBp_c2 == "Nebivolol" ~ num_mdBBddo / 10 * 50,
      num_mdBBp_c2 == "Carvedilol" ~ num_mdBBddo / 50 * 50
    ),
    d_BBdosetarget_prior = factor(case_when(
      d_BBdose_eqCarvedilol_prior < 50 / 2 ~ 1,
      d_BBdose_eqCarvedilol_prior < 50 ~ 2,
      d_BBdose_eqCarvedilol_prior >= 50 ~ 3
    ), levels = c(1:3), labels = c("<50%", "50-<100%", "100%")),
    num_mdBB_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdBBd,
      num_dmPtype == "Outpatient" ~ num_mdBBh
    ),
    num_mdBB_aftern = case_when(
      num_dmPtype == "Hospital" ~ num_mdBBdn,
      num_dmPtype == "Outpatient" ~ num_mdBBhn
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
    num_mdBBdo_aftercn = case_when(
      num_mdBB_after == "Yes" | num_mdBB_aftern %in% c("Contraindicated", "Not tolerated") ~ "Yes/Not tol/Contraindicated",
      num_mdBB_after == "No" ~ "Reason No Other/Missing"
    ),

    # Meds at follow-up

    f1_AL = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1ALh)
    ),
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
    f1_rasi = case_when(
      is.na(f1_ACE) | is.na(f1_AT) ~ NA_character_,
      f1_ACE == "Yes" | f1_AT == "Yes" ~ "Yes",
      TRUE ~ "No"
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
    out_deathcv = case_when(
      is.na(out_death) ~ NA_real_,
      num_f1DeathCs %in% c("Cardiac", "Vascular") ~ 1,
      TRUE ~ 0
    ), # pats with missing info are NOT included in CV

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
    outtime_hosphf = ifelse(out_hosphf == 1 & is.na(outtime_hosphf), outtime_death / 2, outtime_hosphf),
    outtime_hosphf = pmin(outtime_hosphf, outtime_death, na.rm = TRUE),

    # death or hf hosp
    out_deathhosphf = ifelse(out_hosphf == 1, 1, out_death)
  ) %>%
  mutate_if(is.character, as.factor) %>%
  select(-starts_with("tmp_"))
