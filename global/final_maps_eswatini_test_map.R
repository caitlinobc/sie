
g4_pres = ggplot(dist_coord, aes(x=long, y=lat, group=group, fill=vlc)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues'), na.value='#d9d9d9',
                       limits = c(min(dist_coord$vlc), 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral load coverage", 
       fill="Received a VL test (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=14))+
  geom_label_repel(data = labels2, aes(label = vls_label, 
     x=long, y=lat, group=district), inherit.aes=FALSE, size=3)

#---------------
g4_pres_alt = ggplot(dist_coord, aes(x=long, y=lat, group=group, fill=vlc)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Greens'), na.value='#d9d9d9',
                       limits = c(min(dist_coord$vlc), 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral load coverage", 
       fill="Received a VL test (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=14))+
  geom_label_repel(data = labels2, aes(label = vls_label, 
                                       x=long, y=lat, group=district), inherit.aes=FALSE, size=3)

#---------------
# annual suppression ratios

g5_pres = ggplot(dist_coord, aes(x=long, y=lat, group=group, fill=vls)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(6, 'Greens'), na.value='#d9d9d9',
                       limits = c(min(dist_coord$vls), 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral suppression ratio", 
       fill="Virally suppressed (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=14))+
  geom_label_repel(data = labels2, aes(label = vls_label, 
       x=long, y=lat, group=district), inherit.aes=FALSE, size=3)

#---------------
g5_pres_alt = ggplot(dist_coord, aes(x=long, y=lat, group=group, fill=vls)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(6, 'Greens'), na.value='#d9d9d9',
                       limits = c(70, 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral suppression ratio", 
       fill="Virally suppressed (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=14))+
  geom_label_repel(data = labels2, aes(label = vls_label, 
                                       x=long, y=lat, group=district), inherit.aes=FALSE, size=3)

#--------------------------------
# CYAN

lightsky = c('lightskyblue', 'lightskyblue1', 'lightskyblue2', 'lightskyblue3', 'lightskyblue4' )
dodger = c('dodgerblue', 'dodgerblue1', 'dodgerblue2', 'dodgerblue3', 'dodgerblue4' )
pale_turq = c('paleturquoise', 'paleturquoise1', 'paleturquoise2', 'paleturquoise3', 'paleturquoise4' )

g5_pres_cy = ggplot(dist_coord, aes(x=long, y=lat, group=group, fill=vls)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = pale_turq, na.value='#d9d9d9',
                       limits = c(min(dist_coord$vls), 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral suppression ratio", 
       fill="Virally suppressed (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=14))+
  geom_label_repel(data = labels2, aes(label = vls_label, 
                                       x=long, y=lat, group=district), inherit.aes=FALSE, size=3)

#---------------
g5_pres_cy = ggplot(dist_coord, aes(x=long, y=lat, group=group, fill=vls)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = cyans, na.value='#d9d9d9',
                       limits = c(70, 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral suppression ratio", 
       fill="Virally suppressed (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=14))+
  geom_label_repel(data = labels2, aes(label = vls_label, 
            x=long, y=lat, group=district), inherit.aes=FALSE, size=3)


#-------------------------
# facet wrap with labels

# shape labels long in case of future facet wrapping
labels2_long = melt(labels2, id.vars= c('district', 'lat', 'long'))
labels2_long[variable=='vlc_label', variable:='Viral Load Coverage']
labels2_long[variable=='vls_label', variable:='Viral Suppression']

g_final_dist = ggplot(dist_long, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(8, 'Blues'),
                       na.value='#d9d9d9',
                       limits = c(min(dist_long$value), 100), 
                       breaks = c(70, 80, 90)) + 
  theme_void() +
  facet_wrap(~variable)+
  labs(fill = '%',
    caption='Source: EGPAF monitoring data') +
  theme(plot.caption=element_text(vjust=6), 
        text=element_text(size=16), 
        strip.text = element_text(size = 18, vjust=1), 
        plot.margin = unit(c(1,1,1,1), "cm"))


geom_label_repel(data = labels2_long, aes(label = value, 
                                          x=long, y=lat, group=district), inherit.aes=FALSE, size=3)+


#---------------
pdf(paste0(OutDir, 'eswatini_vlc_vls_dist_maps_final.pdf'), 
    width=14, height=8)

grid.arrange(g4_pres_alt, g5_pres_alt, ncol = 2, padding = 5)
grid.arrange(g4_pres, g5_pres, ncol = 2)

dev.off()
#---------------

#---------------
pdf(paste0(OutDir, 'eswatini_same_scale_final.pdf'), 
    width=14, height=10)

g_final_dist

dev.off()
#---------------