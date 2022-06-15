RunGPModel<-function(model,
                   SL_df,
                   useriter=10000,
                   userburnin=2000,
                   userthin=10,
                   save_name = save_name,
                   userchainnum=4){
  # Create a directory "fig" and "modeloutput" in current working directory
  dir.create("fig", showWarnings = FALSE)
  dir.create("output", showWarnings = FALSE)

  cat("Stage 1: Model Started \n")

  #------------------Data-------------
  #---Time element--
  t.i<-SL_df$Age #Non-normalised
  #t.i<-SL_df$AgeNorm

  #---Space element---
  #loc_mat <- data.frame(SL_df$LongNorm,SL_df$LatNorm)
  loc_mat <- as.matrix(SL_df %>% dplyr::select(Longitude,Latitude)) #Non-normalised

  #---Sea Level---
  #y.i<-SL_df$RSLNorm
  y.i<-SL_df$RSL #Non-normalised

  #Total number
  n<-nrow(SL_df)

  #------For GP examining difference between the individual time points-------
  dist_t<-outer(t.i, t.i, '-')^2
  #------For GP examining difference between the individual space points-------
  dist_s<-rdist.earth(loc_mat) #Euclidian distance between all the location of all samples


  ##The required data
  jags_pars <- c("sigma.y",
                 "sigma_st",
                 'mu.g',
                 "rho_t",
                 "rho_s",
                 "alpha")

  jags_data <- list(t.i=t.i,
                    #sd.y = SL_df$RSL_er_Norm,
                    sd.y = SL_df$RSL_er_max,
                    x.i=loc_mat,
                    n=n,
                    dist_s = dist_s,
                    dist_t=dist_t,
                    #extra=SL_df$extra,
                    y.i=y.i)
  ########Run the model########
  # mod <- jags(data=jags_data,
  #             parameters.to.save=jags_pars,
  #             model.file=model,
  #             n.chains=userchainnum,
  #             n.iter=useriter,
  #             n.burnin=userburnin,
  #             n.thin=userthin)
  mod <- jags.parallel(data=jags_data,
              parameters.to.save=jags_pars,
              model.file=model,
              n.chains=userchainnum,
              n.iter=useriter,
              n.burnin=userburnin,
              n.thin=userthin)
  plot(mod)
  print(mod)


  saveRDS(mod,file = save_name)
  cat("Stage 1: Model Finished \n")
  return(NULL)
}
