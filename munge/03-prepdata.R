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
    num_CKDEPI_cat = factor(case_when(
      num_CKDEPI < 60 ~ 2,
      num_CKDEPI >= 60 ~ 1
    ),
    levels = 1:2,
    labels = c(">=60", "<60")
    ),
    num_Hb = coalesce(num_dcHb, num_opHb),
    num_Nyha = coalesce(num_dcNyha, num_opNyha),
    num_Nt = coalesce(num_dcNt, num_opNt),
    num_Mr = coalesce(num_dcMr, num_opMr),
    num_Hypop = coalesce(num_dcHypop, num_opHypop),
    num_Xpu = coalesce(num_dcXpu, num_opXpu),

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
    num_Xpu = if_else(num_Xrn == "Yes", "No", as.character(num_Xpu)),
    num_Xen = coalesce(num_dcXen, num_opXen),
    num_Xen = if_else(num_Xen == "Yes", "No", as.character(num_Xen)),
    num_Xal = coalesce(num_dcXal, num_opXal),
    num_Xal = if_else(num_Xal == "Yes", "No", as.character(num_Xal)),

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

    num_mdACE_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdACEd,
      num_dmPtype == "Outpatient" ~ num_mdACEh
    ),
    num_mdAT_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdATd,
      num_dmPtype == "Outpatient" ~ num_mdATh
    ),
    num_mdARNI_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdARNId,
      num_dmPtype == "Outpatient" ~ num_mdARNIh
    ),
    num_mdAL_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdALd,
      num_dmPtype == "Outpatient" ~ num_mdALh
    ),
    num_mdBB_after = case_when(
      num_dmPtype == "Hospital" ~ num_mdBBd,
      num_dmPtype == "Outpatient" ~ num_mdBBh
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
    f1_BB = case_when(
      num_f1MedAny == "No" ~ "No",
      TRUE ~ as.character(num_f1BBh)
    ),

    # Outcomes
    enddtm = coalesce(num_f1DeathDt, num_f1contDt),
    startdtm = coalesce(num_dcDischdt, num_dmVisitdt),
    outtime_death = as.numeric(enddtm - startdtm),
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
    outtime_hosphf = ifelse(out_hosphf == 1 & is.na(outtime_hosphf), outtime_death / 2, outtime_hosphf),
    outtime_hosphf = pmin(outtime_hosphf, outtime_death, na.rm = TRUE),

    # cv death or hf hosp
    out_deathcvhosphf = ifelse(out_hosphf == 1, 1, out_deathcv)
  ) %>%
  mutate_if(is.character, as.factor) %>%
  select(-starts_with("tmp_"))
