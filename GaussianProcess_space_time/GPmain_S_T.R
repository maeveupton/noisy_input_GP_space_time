##### ----- SPACE & Time Gaussian process model-------
rm(list=ls())
#---------Set working directory--------------
setwd('/users/research/mupton/Quaternary_Science_Chp/GaussianProcess_space_time')
#----------Load packages---------------------
library(tidyverse)
library(R2jags)
library(mvtnorm)
library('spectralGP')# for rdist.earth
library(fields)# for rdist
library(geosphere)#distance between long lat points
#-----------Call functions--------------------
source("R/Plot_GP.R")
source("R/Run_GP.R")
source("R/Run_GP_noise.R")
source("R/AddNoise_GP.R")
#------------Read in SL data---------------------
#SL_df <- read_csv("data/proxy_dataset_4_sites.csv")# new updated Kemp data
SL_df <- read_csv("data/proxy_dataset_east_coast_USA.csv")# new updated Kemp data
SL_df<- SL_df %>%  filter(SiteName %in% c("Placentia,\n Newfoundland",
                                          "Big River Marsh,\n Newfoundland",
                                          "Nassau,\n Florida",
                                          "Little Manatee River,\n Florida"))
#-----Plot RSL vs Age for each data site on East Coast of USA-------
plot_gpdata(SL_df = SL_df,
            #save_location = "fig/all_data_RSL_4sites.pdf",
            save_location = "fig/all_data_RSL_updated_4sites.pdf",
            title = "Proxy Record for East Coast of North America") 
#-----Plot Map of All location data on East coast-----
plot_gpdata_map(SL_df,
                save_location = "fig/Map_datacollection_updated_4sites.pdf")
                #save_location = "fig/Map_datacollection_4sites.pdf")

#------------------Stage 1: Run model without input noise---------------------------------------------
RunGPModel(SL_df=SL_df, model="model/GP_model_s_t.txt",
           #save_name = "output/JAGSmod_S_T_PART1_4sites.rds")
           save_name = "output/JAGSmod_S_T_PART1_updated_4sites.rds")

#----------------------Stage 1: Plotting results of model without noise------------------------------------
plot_gpres(SL_df = SL_df,
           #JAGSoutput = "output/JAGSmod_S_T_PART1_4sites.rds",
           JAGSoutput = "output/JAGSmod_S_T_PART1_updated_4sites.rds",
           title_name = "Spatial Temporal Gaussian Process using a Fully Bayesian Approach \n for East Coast of USA",
           #save_name = "fig/GP_st_4sites_no_noise.pdf",
           save_name = "fig/GP_st_updated_4sites_no_noise.pdf",
           #pred_loop_output = "output/pred_loop_data/pred_loop_no_noise.csv",
           pred_loop_output = "output/pred_loop_data/updated_pred_loop_no_noise.csv",
           #final_df = "output/pred_loop_data/final_df_no_noise.csv")
           final_df = "output/pred_loop_data/final_df_no_noise_updated.csv")


# #------------------Stage 2: Produce predictions for the data points and calculate derivatives---------------
# JAGSoutput = readRDS("output/JAGSmod_S_T_PART1_4sites.rds")
# AddNoise_GP(SL_df =SL_df,JAGSoutput = "output/JAGSmod_S_T_PART1_4sites.rds",
#              name_new_df = "data/Stage2_Noisy_data/SL_df_extra_4sites.csv",
#              test_plot="fig/test_noise_4sites.pdf")
# #---New dataframes with the "extra" uncertainty included-----
# SL_df <- read_csv("data/Stage2_Noisy_data/SL_df_extra_4sites.csv")
# #plot_gpdata(SL_df = SL_df,save_name = "fig/testplease.pdf")
#-------------------------------Stage 3 fit this new model---------------------------------------------------
# RunGPModel_noise(SL_df=SL_df,model="model/GP_model_s_t_extra.txt",
#                   save_name = "output/JAGSmod_S_T_PART2_4sites.rds")

#-------------------------------------Plotting Results------------------------------------------------------
# plot_gpres(SL_df = SL_df,JAGSoutput = "output/JAGSmod_S_T_PART2_4sites.rds",
#            title_name = "Noisy Input Spatial Temporal Gaussian Process for East Coast of North America",
#            save_name = "fig/NIGP_st_4sites.pdf",
#            pred_loop_output = "data/pred_loop_data/pred_loop_ouput_noise_4sites.csv",
#            final_df = "data/final_output_data/final_df_noise_4sites.csv")
#



