# Rough Work
plot_RSL_animation<-function(SL_df_raw,SL_df_new){
  #---------------Plotting the Sea Level over the years----------> Animation
  world <- map_data("world")
  canada <- map_data("worldHires", "Canada")
  usa<- map_data("usa")
  NAmap_animated <- ggplot() + geom_polygon(data = usa,
                                            aes(x=long, y = lat, group = group),
                                            fill = "white",
                                            color="grey") +
    geom_polygon(data = canada, aes(x=long, y = lat, group = group),
                 fill = "white", color="grey") +
    geom_point(data=SL_df_raw,aes(x=Longitude,y=Latitude,colour=RSL),size=5)+
    geom_point(data=SL_df_new,aes(x=LongData,y=LatData,colour=RSLData),size=5)+
    geom_point(data=SL_df_new,aes(x=LongData,y=LatData,colour=RSLMod),size = 5)+
    #geom_tile(data=SL_df,aes(x=LongData,y=LatData,fill=RSLMod))+
    #geom_contour_fill(data=SL_df,aes(x=LongData,y=LatData,z=RSLMod),na.fill = TRUE)+
    coord_fixed(xlim = c(-85, -45),  ylim = c(25, 55), ratio = 1.2)+
    transition_time(SL_df_new$AgeMod) +
    #shadow_wake(wake_length = .1)+
    labs(title="Year {frame_time} CE", x="Longitude", y="Latitude",shape = "Model and Original data")+
    theme_bw()
  NAmap_animated
  # NAmap_animation_new <- animate(NAmap_animated, nframes = nrow(SL_df_new),height = 1500,
  #                            width = 1500, fps = 20, duration = 20, start_pause = 10, end_pause = 20,  rewind = T)
  #
  # NAmap_animation <- animate(NAmap_animated, nframes = nrow(SL_df_raw),height = 1500,
  #                            width = 1500, fps = 20, duration = 20, start_pause = 10, end_pause = 20,  rewind = T)
  # #renderer = gifski_renderer("fig/gganim.gif"))
  anim_save("fig/Animation/RSL_anim_v0.1.gif", NAmap_animated)
  #ggsave(NAmap_animated, file = 'fig/Animation/RSL_anim.pdf', width = 10, height = 6)
  
  #----------------Animation Notes---------------
  #theme_set(theme_bw())
  #animate_RSL<- NAmap_RSL + transition_time(Age) +
  #labs(title = "Year: {frame_time}")
  #Need to fix where its begin saved to
  # gif_RSL<-animate(animate_RSL,nframes = 200, renderer = gifski_renderer("gganim.gif"))
  #anim_save(gif_RSL, animation = last_animation(),path=NULL) #Not working yet
  
  cat("Animation of SL estimates saved to figure folder \n")
}
