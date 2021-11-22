

# -------------------------------------
# SUMMARIZE DATA 

# -------------------------------------
# summarize the eligibility data 

# summarize age
dt[, mean(age, na.rm=TRUE)]
dt[, median(age, na.rm=TRUE)]
dt[, range(age, na.rm=TRUE)]

dt[under5==TRUE, mean(age, na.rm=TRUE)]
dt[under5==TRUE, median(age, na.rm=TRUE)]
dt[under5==TRUE, range(age, na.rm=TRUE)]
dt[under5==TRUE & age < 1]

# summarize hiv status
dt[knwstat==F]
dt[is.na(knwstat)]
dt[dtpos < as.Date("2020-10-06 UTC")]
dt[hivresult==TRUE]

# study eligibility
dt[ ,table(ahd_elig)]
dt[ ,table(ahd_elig)/2464]


dt[ ,table(whostage1_done)]
dt[ ,table(whostage1st)]
dt[ ,table(whostage1st)/2457]

dt[ , table(tbsympscrn)]
