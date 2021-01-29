
#-------------------------------------------------
# DATA QUALITY

# sourced from ahd_data_prep to illustrate data quality issues
# --------------------
# examine the data quality challenges

# export a list of sites to compare to study criterion
sites = dt[ ,.(siteid = unique(siteid)), by = .(dhisid, dhisname, region, district)]
sites = sites[!is.na(dhisid)]
sites = dt[ ,.(siteid, dhisid, dhisname, region, district)] # put siteid first
write.csv(sites, paste0(dir, '/data/prepped/sites_included.csv'))

# patient ids: these counts should be the same (length of sheet and length of data)
dt[ ,length(unique(pid))]
nrow(dt)

# export a list of repeat patient identifiers
dt[ , pid_count:=.N, by = pid]
pids = dt[1 < pid_count]
pids = pids[order(pid)]
write.csv(pids, paste0(dir, '/data/prepped/repeat_pt_ids.csv'))

# export the rows with substantial missing data 
missing_rows = dt[is.na(ahd_dt)]
write.csv(missing_rows, paste0(dir, '/data/prepped/rows_with_missing_data.csv'))

# --------------------
