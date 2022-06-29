
ProjectTemplate::reload.project()

dataass <- mice::complete(imp, 3)
dataass <- mice::complete(imp, 6)

# dataass <- dataass[edataforimp$num_nation != "LATVIA", ]

# check assumptions for cox models ----------------------------------------

mod <- coxph(formula(paste0("Surv(outtime_death, out_death == 1) ~ num_dmRCT + ", paste0(modvars, collapse = "+"))),
  data = dataass
)

testpat <- cox.zph(mod)
print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

x11()
plot(testpat[1], resid = F, ylim = c(-4, 4))

mod <- coxph(formula(paste0("Surv(outtime_hosphf, out_hosphf == 1) ~ num_dmRCT + ", paste0(modvars, collapse = "+"))),
  data = dataass
)

testpat <- cox.zph(mod)
print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

x11()
plot(testpat[1], resid = F, ylim = c(-4, 4))
