
# --------------------
# patch in the drc health zone level shape file

if (drc_patch==TRUE) {
  
  #--------------------
  # import the health zone level shape file
  drc = shapefile(paste0(dir, 'drc_zones/RDC_Zones de santé.shp'))
  drc_names = data.table(cbind(district = drc@data$Nom,
                               id = drc@data$Pcode))
  drc_names[ , country:='COD']
  
  # create health zone labels
  drc_labels = data.table(coordinates(drc))
  setnames(drc_labels, c('long', 'lat'))
  drc_labels = cbind(drc_labels, drc_names)
  
  # correct the topology intersection in the shape file
  drc2 = gBuffer(drc, byid=TRUE, width = 0)
  drc2$id = drc$Pcode
  
  # list of health zones located within Kinshasa
  kinshasa_ids = data.table(cbind(district = drc@data$Nom,
                                  id = drc@data$Pcode, 
                                  province = drc$Anc_provin))
  kinshasa_ids = kinshasa_ids[province=='Kinshasa']
  
  # fortify the shape file
  drc_coord = data.table(fortify(drc2, region='Pcode'))
  drc_coord[ , country:='COD'] 
  
  # subset the shape file to only kinshasa for display
  drc_coord = drc_coord[id %in% kinshasa_ids$id]
  
  
  #--------------------
  
  #--------------------
  # remove DRC from the main file and replace with health zone level
  full_shape = full_shape[country!='COD']
  full_shape = rbind(full_shape, drc_coord)
  
  full_names = full_names[country!='COD']
  full_names = rbind(full_names, drc_names)
  
  full_labels = full_labels[country!='COD']
  full_labels = rbind(full_labels, drc_labels)
}

# -----------------------------------------