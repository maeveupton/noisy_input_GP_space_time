#---------- Stage 2: Produce predictions for the data points and calculate derivatives-----------
AddNoise_GP<- function(JAGSoutput,SL_df,name_new_df,test_plot){
  mod <- readRDS(JAGSoutput)
  #mod<-JAGSoutput
  post_means <- mod$BUGSoutput$mean
  N <- nrow(SL_df)
  y <- SL_df$RSL
  t <- SL_df$Age
  sd.y = SL_df$RSL_er_average
  sd.t = SL_df$Age_er_average
  loc_mat <- as.matrix(SL_df %>% dplyr::select(Longitude,Latitude))
  #------Euclidian distance between all the location of all samples-----
  dist_measure<-rdist.earth(loc_mat)#rdist.earth
  #---------Calculate predicted mean via a function----------
  pred_mean_calc = function(t_new) {
    N_new = length(t_new) # Number of new predictions
    loc_mat <- as.matrix(SL_df%>% dplyr::select(Longitude,Latitude)) # Old locations
    Mu = rep(post_means$alpha, N) # Original GP mean
    Mu_new = rep(post_means$alpha, N_new) # New GP mean
    K_new = (post_means$sigma_st[1])^2 * exp(-(post_means$rho_t[1])^2 * outer(t, t_new, '-')^2 ) * exp(-(post_means$rho_s[1])^2*rdist.earth(loc_mat[,1:2],loc_mat[,1:2])) # Cross-covariance of new to old
    K = (post_means$sigma.y[1])^2 * diag(N)+ diag(sd.y^2) + post_means$sigma_st[1]^2 * exp(-(post_means$rho_t[1])^2 * outer(t,t,'-')^2 ) * exp(- (post_means$rho_s[1])^2*rdist.earth(loc_mat[,1:2],loc_mat[,1:2]))# Old variance matrix
    return(Mu_new + t(K_new)%*%solve(K, y - Mu)) # Return the predictions
  }

  #-------Now create derivatives----
  h = 0.01
  t = SL_df$Age
  deriv = (pred_mean_calc(t+h) - pred_mean_calc(t-h))/(2*h)
  #plot(t,deriv)
  #-----Add this new term in - this is the extra standard deviation on each term----
  SL_df$extra <- sqrt(deriv^2 %*% SL_df$Age_er_average^2)[,1]
  write_csv(SL_df,name_new_df)
  #------Look at results so far for each loction-----
  res_dat <- tibble(Age = SL_df$Age, RSL = SL_df$RSL, pred = pred_mean_calc(t), loc = SL_df$RegionName1, deriv=deriv)
  p_s1 <- ggplot(res_dat, aes(x = Age, y = RSL)) +
    geom_point() +
    geom_line(aes(x = Age, y = pred))+
    facet_wrap(~ loc)
  ggsave(p_s1, file = test_plot, width = 10, height = 6)
}
