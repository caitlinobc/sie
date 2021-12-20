

# number of participants by sex and age category


dt_sa = dt[ ,.(value = length(unique(pid))), by =.(sex, age_cat)]
dt_sa = dcast(dt_sa, age_cat~sex)
write.csv(dt_sa, paste0(outDir, 'age_sex_endline.csv'))

dt_elig = dt[, .(value = length(unique(pid))) , by = .(ahd_elig)]
write.csv(dt_elig, paste0(outDir, 'elig_baseline.csv'))