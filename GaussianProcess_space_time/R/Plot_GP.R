plot_gpdata<-function(SL_df,title,save_location)
{
  #------Ordering the sites-------
  original_test_1<- SL_df %>%  unite("data_site",Latitude:Longitude,remove=FALSE) %>%  #Uniting 2 columns
    mutate(site = sprintf("%02d", as.integer(as.factor(data_site))))
  SL_df <- cbind(SL_df,data_site = original_test_1$data_site)
  order_sites_again <- SL_df %>%  group_by(SiteName,data_site) %>%
    dplyr::summarise(n = n()) %>%
    arrange(desc(data_site))
  SL_df$SiteName <- factor(SL_df$SiteName,levels = unique(order_sites_again$SiteName))
  
  plot1<-ggplot(SL_df)+
    ggtitle(title)+
    facet_wrap(~SiteName,scales = "free")+
    geom_rect(aes(xmin=Age*1000 - Age_er_average*1000, 
                  xmax=Age*1000 + Age_er_average*1000,
                  ymin=RSL - RSL_er_min, ymax=RSL + RSL_er_max,
                  fill="Observed Uncertainty"),alpha = 0.4)+
    labs(x = "Age (CE)", y = "RSL (m MTL)")+
    #geom_point(aes(x=Age*1000,y=RSL,colour = data_type_id),size = 1)+
    geom_point(aes(x=Age*1000,y=RSL,colour = "black"),size = 0.5)+
    theme_bw()+
    theme(strip.background =element_rect(fill="white"))+
    labs(color="")+
    scale_colour_manual(values = c("black"),
                         labels=c("Proxy Data"),
                        guide = guide_legend(override.aes = list(
                          size = 2)))+
    scale_fill_manual(" ",values = 'grey',
                      guide = guide_legend(override.aes = list(alpha = 0.28)))+
    theme(legend.position="bottom", legend.box = "horizontal")+
    theme(plot.title = element_text(size=20,face="bold"),
          axis.title=element_text(size=12,face="bold"),
          legend.text=element_text(size=10))+
    guides(color = guide_legend(override.aes = list(size=2)))
  plot1
  ggsave(plot1, file =save_location, width = 10, height = 6)

  cat("Plot of map saved to figure folder \n")
}

plot_gpdata_map<-function(SL_df,save_location)
{
  library("sf")
  library("ggspatial")
  library("ggrepel")
  library(rnaturalearth)#devtools::install_github("ropensci/rnaturalearthhires")
  cand_usa_states <- ne_states(country = c('United States of America',"Canada"), returnclass = 'sf')
  SL_df<- SL_df %>%  dplyr::select(Longitude,Latitude, SiteName,Region) %>% 
    mutate(Latitude = round(SL_df$Latitude,digits=1), 
           Longitude = round(SL_df$Longitude,digits=1)) %>% unique()
  SL_df <- SL_df %>% 
    mutate(SiteName = case_when(SiteName == "Southern New Jersey - CMC" ~ "Southern New Jersey",
                                TRUE ~ as.character(SiteName)))
  
  NAmap_new <- ggplot(data=cand_usa_states,size = 0.5, alpha = 0) +
    geom_sf(color="grey35",fill="darkseagreen2") +
    geom_label_repel(data=SL_df,aes(x=Longitude,y=Latitude,label = SiteName), max.overlaps = 50,fontface = "bold", size = 3)+
    #geom_text(aes(x=longitude,y=latitude,label = name),fontface = "bold",
    #           position=position_jitter(width=cand_usa_states$jit,height=cand_usa_states$jit),size = 2)+#+position=position_jitter(width=0.1,height=0.1)
    theme_minimal()+
    coord_sf(xlim = c(-85, -50), ylim = c(24, 52), expand = FALSE) +
    geom_point(data=SL_df,aes(x=Longitude,y=Latitude),colour="red",
               shape = 15,
               size = 3)+
    # geom_text(data=SL_df,aes(x=Longitude,y=Latitude,label=Region),size = 2.5,hjust=2, vjust=1)+#position = position_nudge(y = -0.1,x=2))+
    #geom_point(data=SL_df,aes(x=Longitude,y=Latitude),size=4,colour='black',shape = 1)+ #,position=position_jitter(width=1,height=3)
    #labs(colour="Data sites being examined",x="Longitude",y="Latitude",
    #     shape="Data sites being examined")+
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme(panel.grid.major = element_line(color = "gray20",
                                          linetype = "dashed", size = 0.05),
          panel.background = element_rect(fill ="lightcyan"),# "lightblue1"),
          legend.position = "none",
          panel.border = element_rect(colour = "black", fill=NA, size=1))
  NAmap_new
  ggsave(NAmap_new, file = "fig/NA_map_new.pdf", width = 10, height = 7)

  # world <- map_data("world")
  # canada <- map_data("worldHires", "Canada")
  # usa<- map_data("usa")
  # NAmap <- ggplot() + geom_polygon(data = usa,
  #                                  aes(x=long, y = lat, group = group),
  #                                  fill = "white",
  #                                  color="grey") +
  #   geom_polygon(data = canada, aes(x=long, y = lat, group = group),
  #                fill = "white", color="grey") +
  #   geom_point(data=SL_df,aes(x=Longitude,y=Latitude,colour=SiteName),size = 3)+
  #   coord_fixed(xlim = c(-85, -50),  ylim = c(25, 55), ratio = 1.2)+
  #   labs(colour="Data sites being examined",x="Longitude",y="Latitude",
  #        shape="Data sites being examined")+
  #   theme_bw()
  # NAmap
  cat("Plot of raw data saved to figure folder \n")
  #ggsave(NAmap, file = save_location, width = 10, height = 6)

}


plot_gpres<-function(SL_df,JAGSoutput,title_name,save_name,pred_loop_output,final_df)
{
  #----Loading in the JAGS output------
  mod <- readRDS(JAGSoutput)
  #plot(mod)
  #mod$model
  #-----Checking convergence of model using shiney stan-------
  mcmc.array<-mod$BUGSoutput$sims.array
  mcmc.samps<-mod$BUGSoutput$sims.list

  #---------Setting up the magic gausian equation for spacial & time element----------
  #-----Distance elements-----
  full_s_star<-expand.grid(pretty(SL_df$Longitude,n=10),
                           pretty(SL_df$Latitude,n=10))#Big grid of values
  loc_mat <- as.matrix(SL_df %>% dplyr::select(Longitude,Latitude))
  full_dist.s.star<-rdist(loc_mat, full_s_star)#distance from grid to data
  fun = function(x){any(x<2.5)}#i.e the distance from the grid to actually data point
  plot(loc_mat[,1],loc_mat[,2])
  points(full_s_star$Var1,full_s_star$Var2)
  #full_dist.s.star<-rdist(loc_mat, full_s_star)#distance from grid to data
  keep<-apply(full_dist.s.star,2,fun)
  s_star<-full_s_star[keep,]# grid of locations that are beinging kept

  #---Time element for non normalised data---
  t_grid<-seq(min(SL_df$Age),max(SL_df$Age),length=10)

  #----Total matrix----
  s_t_grid <-as.matrix(expand_grid(s_star,t_grid))
  new_matrix<-as.matrix(SL_df %>% dplyr::select(Longitude,Latitude,Age))
  names(new_matrix)<-NULL
  #---Joining the real data with grids----
  s_t_grid<-rbind(s_t_grid,new_matrix)
  ngrid<-nrow(s_t_grid)#total number of rows
  #----Plotting grid & true data ----
  plot(s_t_grid[,1:2])
  points(SL_df$Longitude,SL_df$Latitude,col="red")

  #----Setting up the grids for spacial elements---
  dist.star.star<-rdist(s_t_grid[,1:2],s_t_grid[,1:2])#dist grid to grid
  dist.s.s<-rdist(loc_mat,loc_mat)#dist data to data
  dist.s.star<-rdist(s_t_grid[,1:2],loc_mat)#dist to grid to data

  #-----Setting up the grids for time elements-----
  dist.tg.tg<-outer(s_t_grid[,3], s_t_grid[,3],'-')^2 # --> dist grid to grid
  dist.t<-outer(SL_df$Age, SL_df$Age, '-')^2 # dist from data to data
  dist.tg.t<-outer(s_t_grid[,3],SL_df$Age, '-')^2#distance from grid to data
  #----Parameters for pred loop ----------------
  var.sts<-mod$BUGSoutput$sims.list[["sigma_st"]]^2
  var.ys<-mod$BUGSoutput$sims.list[["sigma.y"]]^2#tau is going to come in here
  rhosq.t<-mod$BUGSoutput$sims.list[["rho_t"]]^2 #Need for time & space
  rhosq.s<-mod$BUGSoutput$sims.list[["rho_s"]]^2 #Need for time & space
  alpha_post<-mod$BUGSoutput$sims.list$alpha
  #sd.y <- SL_df$RSL_er_max
  #extra <- SL_df$extra
  n <- nrow(SL_df)
  #-------Pred loop: looping over the number of posterior samples--------
  #pred <- matrix(NA,ncol=ngrid,nrow=length(var.gs))
  pred <- matrix(NA,ncol=ngrid,nrow=100)
  for(i in 1:dim(pred)[1]) {
    V.y <- var.sts[i]*exp(-rhosq.s[i]*dist.s.s)*exp(-rhosq.t[i]*dist.t)+diag(var.ys[i],n)#+diag(sd.y^2)+ diag(extra^2)
    V.pred <- var.sts[i]*exp(-rhosq.s[i]*dist.star.star)*exp(-rhosq.t[i]*dist.tg.tg)+diag(0.00001,ngrid)
    V.y.pred <- var.sts[i]*exp(-rhosq.s[i]*dist.s.star)*exp(-rhosq.t[i]*dist.tg.t)
    #Does work
    pred[i,] <- rmvnorm(1,alpha_post[i]+(V.y.pred)%*%solve(V.y,SL_df$RSL-alpha_post[i]),V.pred-V.y.pred%*%solve(V.y,t(V.y.pred)))
    #This works
    #pred[i,] <- rmvnorm(1,alpha_post[i]+(V.y.pred)%*%solve(V.y,SL_df$RSL-alpha_post[i]),V.pred-t(V.y.pred)%*%solve(V.y,V.y.pred))
    #Old format
    #pred[i,] <- rmvnorm(1,t(V.y.pred)%*%solve(V.y,SL_df$RSL),V.pred-t(V.y.pred)%*%solve(V.y,V.y.pred))#Does linear alegbraic trick to avoid inversion of matrix etc
  }

  #---------Get estimates and uncertainty bounds------------
    s_med<-apply(pred,2,median,na.rm=TRUE)
    s_upr<-apply(pred,2,quantile,probs=0.025,na.rm=TRUE)
    s_lwr<-apply(pred,2,quantile,probs=0.975,na.rm=TRUE)


    # #-------Resulting RSL values & Age for different Long & Lat values----------
    s_t_res <- as_tibble(s_t_grid) %>%
                mutate(RSL_mod = s_med) %>%
                mutate(RSL_upr = s_upr) %>%
                mutate(RSL_lwr = s_lwr) %>%
      dplyr::rename(Longitude_mod=Var1,Latitude_mod=Var2,Age_mod=t_grid)
    #write_csv(s_t_res,pred_loop_output)
    cat("Pred loop finished and data saved in csv file \n")
    #s_t_res<-read_csv(pred_loop_output)

    #-----Uniting original dataset and model run to give a site index to model_result data set-----
    model_test_1<- s_t_res %>% unite("model_site",Latitude_mod:Longitude_mod,remove = FALSE) %>%  #Uniting 2 columns
      mutate(site = sprintf("%02d", as.integer(as.factor(model_site))))
    original_test_1<- SL_df %>%  unite("data_site",Latitude:Longitude,remove=FALSE) %>%  #Uniting 2 columns
      mutate(site = sprintf("%02d", as.integer(as.factor(data_site))))

    #-------Joining the data sets together------
    db0<-left_join(model_test_1,original_test_1[,c( 'data_site','SiteName' , 'RSL', 'Age','RSL_er_average','Age_er_average')],by = c("model_site"="data_site"))
    db0_new<- db0 %>% mutate_if(is.factor, as.character)%>%
      unite("united_sites",site:SiteName,remove=FALSE,na.rm=TRUE)
    db0_new<-db0_new %>% filter(! is.na(SiteName))
    #------Ordering the sites-------
    order_sites_again <- db0_new %>%  group_by(SiteName,united_sites) %>%
      dplyr::summarise(n = n()) %>%
      arrange(desc(united_sites))
    db0_new$SiteName <- factor(db0_new$SiteName,levels = unique(order_sites_again$SiteName))

    #----------For every location have a RSL vs Age time series-----
    plot_RSL_mod<- 
      ggplot(data = db0_new, aes(x=Age*1000,y=RSL))+
      geom_point(data = db0_new, aes(x=Age*1000,
                                     y=RSL,colour = "black"),size = 0.5)+
      # annotate("rect", data=db0_new,aes( xmin = Age*1000 - 2*Age_er_average*1000, 
      #                                    xmax = Age*1000 + 2*Age_er_average*1000,
      #                                    ymin = RSL - RSL_er_average, 
      #                                    ymax = RSL + RSL_er_average), fill = "gray", alpha = 0.02) +
      # geom_rect(data=db0_new,aes(xmin = Age*1000 - Age_er_average*1000,
      #                xmax = Age*1000 + Age_er_average*1000,
      #                ymin = RSL - RSL_er_average,
      #                ymax = RSL + RSL_er_average,fill = "gray80"),
      #           colour = "gray90",alpha=0.8)+
      geom_ribbon(aes(ymin=RSL_lwr,ymax=RSL_upr,x=Age_mod*1000,fill="aquamarine3"),
                  alpha = 0.5 )+
      geom_line(aes(x=Age_mod*1000,y=RSL_mod,colour='aquamarine4'))+
      theme(legend.position = "none")+
      ggtitle(title_name)+
      theme_bw()+
      ylab('RSL (m MTL)')+
      theme(plot.title = element_text(size=18,face="bold"),
            axis.title=element_text(size=15,face="bold"))+
      theme(strip.background =element_rect(fill="white"),
            legend.text=element_text(size=12),
            legend.position="bottom")+
      scale_colour_manual(name= "",
                          values = c("aquamarine4","black"),
                          labels=c("Model fit","Proxy Data"),
                          guide = guide_legend(override.aes = list(
                            linetype = c( "solid","blank"),
                            shape = c( NA,16),
                            size = c(1,3))))+
      scale_fill_manual(values = c("aquamarine3"),
                        labels = c("95% Confidence Interval"),
                        guide = guide_legend(override.aes = list(alpha = 0.5)),name= "")+
      xlab('Time (CE)')+
      facet_wrap(~SiteName,scales = 'free')
    plot_RSL_mod
    ggsave(plot_RSL_mod, file = save_name, width = 10, height = 6)
    cat("Plot of SL estimates saved to figure folder \n")
    #-------Writting final dataframe-----
    write_csv(db0_new,final_df)
    cat("Pred loop finished and data saved in csv file \n")
    s_t_res<-read_csv(pred_loop_output)

}

