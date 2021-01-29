# diagnostic plots

# histogram of age
hist(dt$age)

# jittered scatter of age by enrollment classification
ggplot(dt, aes(x = ahd_elig, y = age, color = ahd_elig))+
         geom_jitter()+
         theme_bw()

# box plot of age by enrollment classification
ggplot(dt, aes(x = ahd_elig, y = age))+
  geom_boxplot()+
  theme_bw()
