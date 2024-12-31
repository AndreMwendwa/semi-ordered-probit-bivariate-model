library(maxLik)
library(mvtnorm)
library(MASS)
library(tidyverse)
library(pbivnorm)
library(readxl)
set.seed(2)

pers2011 <- read_csv('C:\\Users\\kikomwen\\Documents\\pers_2011_modified3.csv')
hhlds_2011 <- read_csv('C:\\Users\\kikomwen\\Documents\\hhld_2011_1.csv')
accessibility <- read_excel('C:\\Users\\kikomwen\\Documents\\Files_to_send_to_DMG_server\\TAZ_Transport_accessibility_1971-2016.xlsx')
popn_density <- read_excel('C:\\Users\\kikomwen\\Documents\\Code\\popn_density_2\\popn_density_2021.xlsx')
popn_density_16_11 <- read_excel('C:\\Users\\kikomwen\\Documents\\Code\\popn_density_2016_2011\\popn_density_2016_2011c.xlsx')
distance_to_downtown <- read_excel('C:\\Users\\kikomwen\\Documents\\Code\\distance_to_downtown_calc\\Distance_to_downtown.xlsx')
activity_clusters_lu_entropy <- read_csv('C:\\Users\\kikomwen\\Documents\\Code\\ForKiko_11112024.csv')

pers_hhlds_2011 <- inner_join(pers2011, hhlds_2011, join_by(hhld_num == hhld_num))

pers_hhlds_2011 <- pers_hhlds_2011 %>% 
  filter(!is.na(entropy))

## CreatinG n_vehicle >= 1 variables
pers_hhlds_2011 <-pers_hhlds_2011 %>% 
  mutate(
    one_veh = as.integer(n_vehicle == 1),
    more_than_one_veh = as.integer(n_vehicle > 1),
    n_veh_modified = if_else(n_vehicle > 2, 2, n_vehicle)
  )

# Putting income class 7 (Don't know) as the median class === 4.
## I'll replace this later
pers_hhlds_2011 <- pers_hhlds_2011 %>% 
  mutate(
    # income = if_else(income == 7, 4, income),
    driver_lic_modified = if_else((driver_lic == "Y"), 1, 0),
    trans_pass_modified = if_else((tran_pass == "M"), 1, 0),
    free_park_modified = if_else(free_park == 'Y', 1, 0),
    occ_g = if_else(occupation == 'G', 1, 0),
    occ_m = if_else(occupation == 'M', 1, 0),
    occ_s = if_else(occupation == 'S', 1, 0),
    occ_p = if_else(occupation == 'P', 1, 0),
    stu_full = if_else(stu_stat == 'S', 1, 0),
    emp_out_of_home = if_else((emp_stat == 'F')|(emp_stat == 'P'), 1, 0),
    # income_1 = if_else(income == 1, 1, 0), 
    # income_2 = if_else(income == 2, 1, 0),
    # income_3 = if_else(income == 3, 1, 0),
    # income_4 = if_else(income == 4, 1, 0),
    # income_5 = if_else(income == 5, 1, 0),
    # income_6 = if_else(income == 6, 1, 0),
    # region_hhld = as.factor(region_hhld),
    region_hhld_to = if_else(region_hhld == 1, 1, 0),
    region_emp_to = if_else(region_emp == 1, 1, 0),
    region_sch_to = if_else(region_sch == 1, 1, 0),
    region_hhld_york = if_else(region_hhld == 3, 1, 0),
    region_emp_york = if_else(region_emp == 3, 1, 0),
    region_sch_york = if_else(region_sch == 3, 1, 0),
    region_hhld_peel = if_else(region_hhld == 4, 1, 0),
    region_emp_peel = if_else(region_emp == 4, 1, 0),
    region_sch_peel = if_else(region_sch == 4, 1, 0),
    one_licence = if_else(n_licence == 1, 1, 0), 
    two_licence = if_else(n_licence == 2, 1, 0),
    three_or_more_licence = if_else(n_licence >= 3, 1, 0),
    over_three_licence = if_else(n_licence >= 3, n_licence - 3, 0),
    age_16_18 = if_else((age >= 16)&(age < 18), 1, 0),
    age_18_20 = if_else((age >= 18)&(age < 20), 1, 0),
    age_21_25 = if_else((age >= 21)&(age < 25), 1, 0),
    age_26_35 = if_else((age >= 26)&(age < 35), 1, 0),
    age_46_55 = if_else((age >= 46)&(age < 55), 1, 0),
    age_56_65 = if_else((age >= 56)&(age < 65), 1, 0),
    age_66_75 = if_else((age >= 66)&(age < 75), 1, 0),
    age_76_plus = if_else((age >= 76), 1, 0),
    pd_emp_1 = if_else((pd_emp == 1), 1, 0)
  )

hhld_vars <- pers_hhlds_2011 %>% 
  group_by(hhld_num) %>% 
  summarise(
    entropy_hhld = mean(entropy, na.rm = TRUE),
    teleworkers = sum(emp_stat == 'H', na.rm = TRUE),
    cant_drive = sum(age < 16, na.rm = TRUE),
    licenced = sum(driver_lic == 'Y', na.rm = TRUE),
    sum_pers_trips = sum(n_pers_trip),
    sum_purp_orig_W = sum(purp_orig_W),
    sum_purp_orig_F = sum(purp_orig_F)
    # hhld_num = hhld_num
  )


pers_hhlds_2011 <- pers_hhlds_2011 %>% 
  inner_join(hhld_vars)

pers_hhlds_2011 <- pers_hhlds_2011 %>% 
    inner_join(distance_to_downtown, join_by(gta06_hhld == gta06_net0))

# pers_hhlds_2011 <- pers_hhlds_2011 %>% 
#   inner_join(accessibility, join_by(gta01_hhld == TAZ_O))    # Not sure if this is right, this just generates more data after merge
# # as opposed to using gta06_hhld
# 
# # # Random sample for now to speed things up
# # pers_hhlds_2011 <- sample_n(pers_hhlds_2011, 10000)
# 
# # Reducing size of accessibility variables (currently too large)
# pers_hhlds_2011 <- pers_hhlds_2011 %>%
#   mutate(
#     ACAR16 = ACAR16 / 10000,
#     ATRAN16 = ATRAN16 / 10000,
#     JACAR16 = JACAR16 / 10000,
#     JATRAN16 = JATRAN16 / 10000,
#     TTDIFF16 = TTCAR16 - TTRAN16,
#     ADIFF16 = ACAR16 - ATRAN16,
#     JDIFF16 = JACAR16 - JATRAN16
#   )

# Joining to population density
popn_density_16_11 <- popn_density_16_11 %>% 
  dplyr::select(GTA06, popdens_11) 
pers_hhlds_2011 <- pers_hhlds_2011 %>% 
  inner_join(popn_density_16_11, join_by(gta06_hhld == GTA06))
pers_hhlds_2011 <- pers_hhlds_2011 %>% 
  mutate(
    popn_dens_divided = popdens_11 / 100, 
    log_popn_dens = if_else(popdens_11 > 0, log(popdens_11), -10),
    ones = 1, 
    random = rnorm(n = nrow(pers_hhlds_2011)),
    random2 = runif(n = nrow(pers_hhlds_2011), 0, 1),
    log_distance_to_downtown = if_else(HubDist != 0, log(HubDist), -10)
  )


## Merging with activity_cluster_lu_entropy 
activity_clusters_lu_entropy <- activity_clusters_lu_entropy %>% 
  mutate(
    log_Rela_Dist_Commercial = log(Rela_Dist_Commercial)
  )

pers_hhlds_2011 <- pers_hhlds_2011 %>% 
  inner_join(activity_clusters_lu_entropy, join_by(gta06_hhld == GTA06))

## Actual data
ind_pt_owner <- as.matrix(pers_hhlds_2011[, c("n_veh_modified","age", "driver_lic_modified", "entropy_hhld",
                                              "free_park_modified", "occ_m", "occ_g", "occ_s", "occ_p", "emp_out_of_home", "n_licence", "log_distance_to_downtown", "log_popn_dens"
                                              ,"region_hhld_to", "region_sch_to", "trip_week",
                                              "purp_orig_W", "purp_orig_F", "pd_emp_1"
                                              , "LU_Entropy"
                                              # ,"region_hhld_york", "region_emp_york", "region_sch_york",
                                              # "region_hhld_peel", "region_emp_peel", "region_sch_peel"
                          )])
ind_no_veh <- as.matrix(pers_hhlds_2011[, c("entropy_hhld", "teleworkers", "licenced", "free_park_modified", "log_distance_to_downtown", "log_popn_dens"
                                            ,"region_hhld_to", "region_emp_to", "region_sch_to", "trip_week", 
                                            "sum_purp_orig_W", "sum_purp_orig_F"
                                            , "LU_Entropy"
                                            # ,"region_hhld_york", "region_emp_york", "region_sch_york",
                                            # "region_hhld_peel", "region_emp_peel", "region_sch_peel"
                          )])
# For calculating LL0
# ind_pt_owner <- as.matrix(pers_hhlds_2011[, c("random2")])
# ind_no_veh <- as.matrix(pers_hhlds_2011[, c("random2")])


trans_pass_modified <- pers_hhlds_2011 %>% 
  dplyr::select(trans_pass_modified)
n_veh_modified <- pers_hhlds_2011 %>% 
  dplyr::select(n_veh_modified)

loglike <- function(theta){
  beta_pt <- theta[1:(ncol(ind_pt_owner) + 1)]
  beta_car <- theta[(ncol(ind_pt_owner) + 2): (ncol(ind_pt_owner) + 2 + ncol(ind_no_veh))]
  cut2 <- theta[ncol(ind_pt_owner) + ncol(ind_no_veh) + 3]
  # cut12diff <- theta[ncol(ind_pt_owner) + ncol(ind_no_veh) + 4]
  rho <- tanh(theta[ncol(ind_pt_owner) + ncol(ind_no_veh) + 4])
  # rho <- 1 - 2/(exp(theta[4]) + 1)
  
  # cut2 <- cut1 + exp(cut12diff)
  # browser()
  
  # Linear combinations
  lincomb_pt <- beta_pt[1] + ind_pt_owner %*% beta_pt[2:(1 + ncol(ind_pt_owner))]
  lincomb_no_cars <- beta_car[1] + ind_no_veh %*% beta_car[2:(1 + ncol(ind_no_veh))]
  
  # Probabilities
  p00 <- pbivnorm(matrix(c(-lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho)
  p10 <- pbivnorm(matrix(c(cut2 - lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho) - pbivnorm(matrix(c(-lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho)
  p20 <- pnorm(-lincomb_pt) - pbivnorm(matrix(c(cut2 - lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho)
  
  p01 <- pnorm(-lincomb_no_cars) - pbivnorm(matrix(c(-lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho)
  p11 <- pnorm(cut2 - lincomb_no_cars) - pnorm(-lincomb_no_cars) - pbivnorm(matrix(c(cut2 - lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho) + pbivnorm(matrix(c(-lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho)
  p21 <- 1 - pnorm(cut2 - lincomb_no_cars) - pnorm(-lincomb_pt) + pbivnorm(matrix(c(cut2 - lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho)
  
  # browser()
  
  loglikelihood <- sum(log(case_when(
    (trans_pass_modified == 0 & n_veh_modified == 0) ~ p00 / 2,
    (trans_pass_modified == 0 & n_veh_modified == 1) ~ p10 / 2,
    (trans_pass_modified == 0 & n_veh_modified == 2) ~ p20 / 2,
    (trans_pass_modified == 1 & n_veh_modified == 0) ~ p01 / 2,
    (trans_pass_modified == 1 & n_veh_modified == 1) ~ p11 / 2,
    (trans_pass_modified == 1 & n_veh_modified == 2) ~ p21 / 2,
  )))
  return(loglikelihood)
}

# #### Running code #####
# start <- Sys.time()
# start_a <- rep(0.1, ncol(ind_no_veh) + ncol(ind_pt_owner) + 2)
# start_b <- c(0.001, 0)
# start_vals = c(start_a, start_b)
# # est <- maxLik(loglike, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.001, 0), method = "BFGS")
# est <- maxLik(loglike, start = start_vals, method = "BFGS")
# end <- Sys.time()
# summary(est)
# print(end - start)



### Running code: starting with params of individual models ####
pt_model_data <- as.data.frame(cbind(trans_pass_modified, ind_pt_owner))
bin_probit <- glm(trans_pass_modified ~ ., data=pt_model_data)
coef_bin <- coef(summary(bin_probit))[, 1]

ord_model_data <- as.data.frame(cbind(n_veh_modified, ind_no_veh)) %>% 
  mutate(
    n_veh_modified = as.factor(n_veh_modified)
  )
ord_probit <- polr(n_veh_modified ~ ., data = ord_model_data, method='probit')
coef_ord <- coef(summary(ord_probit))[1:ncol(ord_model_data) - 1, 1]    # Ignore thresholds since my model formulation includes a constant

other_start_params <- c(2.5, 0)

start <- Sys.time()
start_vals_b <- c(coef_bin, c(0), coef_ord, other_start_params)
est <- maxLik(loglike, start = start_vals_b, method = "BFGS", control = list(printLevel=4, reltol=0, gradtol=0))
end <- Sys.time()
summary(est)
print(end - start)


### Storing results
sink('output_25_10_24_combined.txt', append = TRUE)
print(summary(bin_probit))
print(summary(ord_probit))
print(summary(est))
sink()

# # Iterating
# (coef_model <- coef(summary(est))[, 1])
# start <- Sys.time()
# est2 <- maxLik(loglike, start = coef_model, method = "NM", control = list(printLevel=4, reltol=0, gradtol=0))
# end <- Sys.time()
# summary(est2)
# print(end - start)
# 
# loglike_bhhh <- function(theta){
#   beta_pt <- theta[1:(ncol(ind_pt_owner) + 1)]
#   beta_car <- theta[(ncol(ind_pt_owner) + 2): (ncol(ind_pt_owner) + 2 + ncol(ind_no_veh))]
#   cut2 <- theta[ncol(ind_pt_owner) + ncol(ind_no_veh) + 3]
#   # cut12diff <- theta[ncol(ind_pt_owner) + ncol(ind_no_veh) + 4]
#   rho <- tanh(theta[ncol(ind_pt_owner) + ncol(ind_no_veh) + 4])
#   # rho <- 1 - 2/(exp(theta[4]) + 1)
#   
#   # cut2 <- cut1 + exp(cut12diff)
#   # browser()
#   
#   # Linear combinations
#   lincomb_pt <- beta_pt[1] + ind_pt_owner %*% beta_pt[2:(1 + ncol(ind_pt_owner))]
#   lincomb_no_cars <- beta_car[1] + ind_no_veh %*% beta_car[2:(1 + ncol(ind_no_veh))]
#   
#   # Probabilities
#   p00 <- pbivnorm(matrix(c(-lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho)
#   p10 <- pbivnorm(matrix(c(cut2 - lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho) - pbivnorm(matrix(c(-lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho)
#   p20 <- pnorm(-lincomb_pt) - pbivnorm(matrix(c(cut2 - lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho)
#   
#   p01 <- pnorm(-lincomb_no_cars) - pbivnorm(matrix(c(-lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho)
#   p11 <- pnorm(cut2 - lincomb_no_cars) - pnorm(-lincomb_no_cars) - pbivnorm(matrix(c(cut2 - lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho) + pbivnorm(matrix(c(-lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho)
#   p21 <- 1 - pnorm(cut2 - lincomb_no_cars) - pnorm(-lincomb_pt) + pbivnorm(matrix(c(cut2 - lincomb_no_cars, -lincomb_pt), ncol = 2), rho = rho)
#   
#   # browser()
#   
#   loglikelihood <- (log(fifelse(
#     (trans_pass_modified == 0 & n_veh_modified == 0), p00 / 2,
#     fifelse((trans_pass_modified == 0 & n_veh_modified == 1), p10 / 2,
#             fifelse((trans_pass_modified == 0 & n_veh_modified == 2), p20 / 2,
#                     fifelse((trans_pass_modified == 1 & n_veh_modified == 0), p01 / 2,
#                             fifelse((trans_pass_modified == 1 & n_veh_modified == 1), p11 / 2, p21 / 2
#   )))))))
#   return(loglikelihood)
# }
# 
# 
# ## BHHH iteration
# (coef_model <- coef(summary(est))[, 1])
# start <- Sys.time()
# est3 <- maxLik(loglike_bhhh, start = coef_model, method = "BHHH", control = list(reltol=0, gradtol=0))
# end <- Sys.time()
# summary(est3)
# print(end - start)
# 
# 
# # coefs <- coef(summary(est))[, 1]
# # est <- maxLik(loglike, start = coefs, method = "BFGS", control = list(printLevel=4, reltol=0, gradtol=0))
# 
# # #################################
# # ## Parameterized thresholds #####
# # #################################
# # loglike <- function(theta){
# #   beta_pt <- theta[1:(ncol(ind_pt_owner) + 1)]
# #   beta_car <- theta[(ncol(ind_pt_owner) + 2): (ncol(ind_pt_owner) + 2 + ncol(ind_no_veh))]
# #   cut2 <- theta[ncol(ind_pt_owner) + ncol(ind_no_veh) + 3]
# #   # cut12diff <- theta[ncol(ind_pt_owner) + ncol(ind_no_veh) + 4]
# #   rho <- tanh(theta[ncol(ind_pt_owner) + ncol(ind_no_veh) + 4])
# #   
# #   # cut2 <- cut1 + exp(cut12diff)
# #   # browser()
# #   
# #   # Linear combinations
# #   lincomb_pt <- beta_pt[1] + ind_pt_owner %*% beta_pt[2:(1 + ncol(ind_pt_owner))]
# #   lincomb_no_cars <- beta_car[1] + ind_no_veh %*% beta_car[2:(1 + ncol(ind_no_veh))]
# #   
# #   # Probabilities
# #   p00 <- pbivnorm(matrix(c(-lincomb_no_cars, -lincomb_pt), ncol = 2), rho = -rho)
# #   p10 <- pbivnorm(matrix(c(cut2 - lincomb_no_cars, -lincomb_pt), ncol = 2), rho = -rho) - pbivnorm(matrix(c(-lincomb_no_cars, -lincomb_pt), ncol = 2), rho = -rho)
# #   p20 <- 1 - pbivnorm(matrix(c(cut2 - lincomb_no_cars, -lincomb_pt), ncol = 2), rho = -rho)
# #   
# #   p01 <- pbivnorm(matrix(c(-lincomb_no_cars, lincomb_pt), ncol = 2), rho = rho)
# #   p11 <- pbivnorm(matrix(c(cut2 - lincomb_no_cars, lincomb_pt), ncol = 2), rho = rho) - pbivnorm(matrix(c(-lincomb_no_cars, lincomb_pt), ncol = 2), rho = rho)
# #   p21 <- 1 - pbivnorm(matrix(c(cut2 - lincomb_no_cars, lincomb_pt), ncol = 2), rho = rho)
# #   
# #   loglikelihood <- sum(log(case_when(
# #     (trans_pass_modified == 0 & n_veh_modified == 0) ~ p00,
# #     (trans_pass_modified == 0 & n_veh_modified == 1) ~ p10,
# #     (trans_pass_modified == 0 & n_veh_modified == 2) ~ p20,
# #     (trans_pass_modified == 1 & n_veh_modified == 0) ~ p01,
# #     (trans_pass_modified == 1 & n_veh_modified == 1) ~ p11,
# #     (trans_pass_modified == 1 & n_veh_modified == 2) ~ p21,
# #   )))
# #   return(loglikelihood)
# # }
# # 
# # #### Running code #####
# # start <- Sys.time()
# # start_a <- rep(0.1, ncol(ind_no_veh) + ncol(ind_pt_owner) + 2)
# # start_b <- c(0.001, 0)
# # start_vals = c(start_a, start_b)
# # # est <- maxLik(loglike, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.001, 0), method = "BFGS")
# # est <- maxLik(loglike, start = start_vals, method = "BFGS")
# # end <- Sys.time()
# # summary(est)
# # print(end - start)

