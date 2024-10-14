library(simASAP)
library(dplyr)
library(here)
#split_survey is code from Chris Legault (@cmlegault) for splitting a survey.
# Used withing the sim_then_split_ASAP functions

split_survey <- function(datf, surveynumber, yearsplit){
  orig <- ReadASAP3DatFile(datf)
  mod <- orig
  
  
  mod$dat$n_indices <- mod$dat$n_indices + 2 
  ni <- mod$dat$n_indices # number of indices with split indices added  
  nim1 <- mod$dat$n_indices-1 # number of indices minus 1
  
  mod$survey.names[nim1] <- paste0(mod$survey.names[surveynumber], "-early")
  mod$survey.names[ni] <- paste0(mod$survey.names[surveynumber], "-", yearsplit, "-onward")
  
  mod$dat$index_units[nim1:ni] <- mod$dat$index_units[surveynumber]
  mod$dat$index_acomp_units[nim1:ni] <- mod$dat$index_acomp_units[surveynumber]
  mod$dat$index_WAA_pointers[nim1:ni] <- mod$dat$index_WAA_pointers[surveynumber]
  mod$dat$index_month[nim1:ni] <- mod$dat$index_month[surveynumber]
  mod$dat$index_sel_choice[nim1:ni] <- mod$dat$index_sel_choice[surveynumber]
  mod$dat$index_sel_option[nim1:ni] <- mod$dat$index_sel_option[surveynumber]
  mod$dat$index_sel_start_age[nim1:ni] <- mod$dat$index_sel_start_age[surveynumber]
  mod$dat$index_sel_end_age[nim1:ni] <- mod$dat$index_sel_end_age[surveynumber]
  mod$dat$use_index_acomp[nim1:ni] <- mod$dat$use_index_acomp[surveynumber]
  mod$dat$use_index_acomp[surveynumber] <- 0 # turn off the original survey
  mod$dat$use_index[nim1:ni] <- mod$dat$use_index[surveynumber]
  mod$dat$use_index[surveynumber] <- 0 # turn off the original survey
  
  mod$dat$index_sel_ini[[nim1]] <- mod$dat$index_sel_ini[[surveynumber]]
  mod$dat$index_sel_ini[[ni]] <- mod$dat$index_sel_ini[[surveynumber]]
  
  inddat <- mod$dat$IAA_mats[[surveynumber]]
  inddatncols <- length(inddat[1,])
  inddatblank <- inddat
  inddatblank[,2] <- -999
  inddatblank[,inddatncols] <- 0
  inddatearly <- inddat
  inddatearly[(inddatearly[,1] >= yearsplit),2] <- -999  
  inddatearly[(inddatearly[,1] >= yearsplit),inddatncols] <- 0
  inddatlate <- inddat
  inddatlate[(inddatlate[,1] < yearsplit),2] <- -999
  inddatlate[(inddatlate[,1] < yearsplit),inddatncols] <- 0
  mod$dat$IAA_mats[[surveynumber]] <- inddatblank
  mod$dat$IAA_mats[[nim1]] <- inddatearly
  mod$dat$IAA_mats[[ni]] <- inddatlate
  
  mod$dat$lambda_index[nim1:ni] <- mod$dat$lambda_index[surveynumber]
  
  mod$dat$lambda_q[nim1:ni] <- mod$dat$lambda_q[surveynumber]
  mod$dat$cv_q[nim1:ni] <- mod$dat$cv_q[surveynumber]
  mod$dat$lambda_q_devs[nim1:ni] <- mod$dat$lambda_q_devs[surveynumber]
  mod$dat$cv_q_devs[nim1:ni] <- mod$dat$cv_q_devs[surveynumber]
  
  mod$dat$q_ini[nim1:ni] <- mod$dat$q_ini[surveynumber]
  
  xx <- 1:length(mod$comments)
  cut1 <- xx[mod$comments == paste0("# Index-", ni-2, " Selectivity Data  ")]
  cut2 <- xx[mod$comments == paste0("# Index-", ni-2, " Data  ")]
  modcomments <- c(mod$comments[1:cut1], "# early split index Selectivity Data ", "# late split index Selectivity Data",
                   mod$comments[(cut1+1):cut2], "# early split Index Data ", "# late split Index Data",
                   mod$comments[(cut2+1):length(mod$comments)])
  mod$comments <- modcomments
  
  return(mod)
}


#################################################################################
# This function splits a second survey if you've already used the split_survey
# function once. there are sometimes issues with the "comments" section of this 
# function. if you get an error about "length of cut1" or something along those 
#lines it's related to those lines at the bottom of the function here. 
# Check the number of spaces after/before phrases in ""
split_survey.2 <- function(datf, surveynumber, yearsplit){
  orig <- ReadASAP3DatFile(datf)
  mod <- orig
  
  
  mod$dat$n_indices <- mod$dat$n_indices + 2 
  ni <- mod$dat$n_indices # number of indices with split indices added  
  nim1 <- mod$dat$n_indices-1 # number of indices minus 1
  
  mod$survey.names[nim1] <- paste0(mod$survey.names[surveynumber], "-early")
  mod$survey.names[ni] <- paste0(mod$survey.names[surveynumber], "-", yearsplit, "-onward")
  
  mod$dat$index_units[nim1:ni] <- mod$dat$index_units[surveynumber]
  mod$dat$index_acomp_units[nim1:ni] <- mod$dat$index_acomp_units[surveynumber]
  mod$dat$index_WAA_pointers[nim1:ni] <- mod$dat$index_WAA_pointers[surveynumber]
  mod$dat$index_month[nim1:ni] <- mod$dat$index_month[surveynumber]
  mod$dat$index_sel_choice[nim1:ni] <- mod$dat$index_sel_choice[surveynumber]
  mod$dat$index_sel_option[nim1:ni] <- mod$dat$index_sel_option[surveynumber]
  mod$dat$index_sel_start_age[nim1:ni] <- mod$dat$index_sel_start_age[surveynumber]
  mod$dat$index_sel_end_age[nim1:ni] <- mod$dat$index_sel_end_age[surveynumber]
  mod$dat$use_index_acomp[nim1:ni] <- mod$dat$use_index_acomp[surveynumber]
  mod$dat$use_index_acomp[surveynumber] <- 0 # turn off the original survey
  mod$dat$use_index[nim1:ni] <- mod$dat$use_index[surveynumber]
  mod$dat$use_index[surveynumber] <- 0 # turn off the original survey
  
  mod$dat$index_sel_ini[[nim1]] <- mod$dat$index_sel_ini[[surveynumber]]
  mod$dat$index_sel_ini[[ni]] <- mod$dat$index_sel_ini[[surveynumber]]
  
  inddat <- mod$dat$IAA_mats[[surveynumber]]
  inddatncols <- length(inddat[1,])
  inddatblank <- inddat
  inddatblank[,2] <- -999
  inddatblank[,inddatncols] <- 0
  inddatearly <- inddat
  inddatearly[(inddatearly[,1] >= yearsplit),2] <- -999  
  inddatearly[(inddatearly[,1] >= yearsplit),inddatncols] <- 0
  inddatlate <- inddat
  inddatlate[(inddatlate[,1] < yearsplit),2] <- -999
  inddatlate[(inddatlate[,1] < yearsplit),inddatncols] <- 0
  mod$dat$IAA_mats[[surveynumber]] <- inddatblank
  mod$dat$IAA_mats[[nim1]] <- inddatearly
  mod$dat$IAA_mats[[ni]] <- inddatlate
  
  mod$dat$lambda_index[nim1:ni] <- mod$dat$lambda_index[surveynumber]
  
  mod$dat$lambda_q[nim1:ni] <- mod$dat$lambda_q[surveynumber]
  mod$dat$cv_q[nim1:ni] <- mod$dat$cv_q[surveynumber]
  mod$dat$lambda_q_devs[nim1:ni] <- mod$dat$lambda_q_devs[surveynumber]
  mod$dat$cv_q_devs[nim1:ni] <- mod$dat$cv_q_devs[surveynumber]
  
  mod$dat$q_ini[nim1:ni] <- mod$dat$q_ini[surveynumber]

  xx <- 1:length(mod$comments)
  cut1 <- xx[mod$comments == "# late split index Selectivity Data "]
  cut2 <- xx[mod$comments == "# late split Index Data "]
  modcomments <- c(mod$comments[1:cut1], "# 2nd early split index Selectivity Data ", "# 2nd late split index Selectivity Data",
                   mod$comments[(cut1+1):cut2], "# 2nd early split Index Data ", "# 2nd late split Index Data",
                   mod$comments[(cut2+1):length(mod$comments)])
  mod$comments <- modcomments
  
  return(mod)
}
#################################################################################
###############################################################################
# This function simulates ASAP files then splits two surveys the optionally 
# fits those files
# There's definitely some hard coded stuff in here. For example, the split survey function
# embedded in this function is hard coded to split the first two surveys in the data file
# at the year 2009. Also may have hard coded a directory or two in here
sim_then_split_ASAP <- function(wd, asap.name, nsim, od=file.path(wd, "sim"), runflag=FALSE){
  
  # error checks for missing files
  if (!file.exists(file.path(wd, paste0(asap.name, ".dat")))){
    return(paste0("Error: ", asap.name, ".dat not located in ", wd))
  }
  
  if (!file.exists(file.path(wd, paste0(asap.name, ".rdat")))){
    return(paste0("Error: ", asap.name, ".rdat not located in ", wd))
  }
  
  if (runflag == TRUE & !file.exists(file.path(wd, "ASAP3.exe"))){
    return(paste0("Error: ASAP3.exe not located in ", wd))
  }
  
  if (!dir.exists(od)){
    dir.create(od)
  }
  
  # get the dat and rdat files
  asap.dat <- ReadASAP3DatFile(file.path(wd, paste0(asap.name, ".dat")))
  asap <- dget(file.path(wd, paste0(asap.name, ".rdat")))
  
  # begin simulation loop to create data sets
  for (isim in 1:nsim){
    
    sim.dat <- asap.dat
    
    # handle each fleet one at a time
    for (ifleet in 1:asap$parms$nfleets){
      
      # generate new total catch observations
      ctotfleet <- asap$catch.pred[ifleet, ]
      sigma <- sqrt(log(1 + asap$control.parms$catch.tot.cv[, ifleet] ^ 2))
      randomval <- stats::rnorm(length(ctotfleet))
      ctotnew <- ctotfleet * exp(randomval * sigma)
      
      # generate new catch at age proportions
      myval <- (ifleet - 1) * 4 + 2 # obs and pred for catch and discards for each fleet, catch pred 2nd
      caafleet <- asap$catch.comp.mats[[myval]]
      caanew <- caafleet
      ess <- asap$fleet.catch.Neff.init[ifleet, ]
      for (icount in 1:length(ess)){
        if (ess[icount] > 0){
          sumcaa <- sum(caafleet[icount, ])
          if (sumcaa > 0){
            myprob <- caafleet[icount, ] / sumcaa
            caanew[icount, ] <- stats::rmultinom(1, ess[icount], prob=myprob)
          } else {
            caanew[icount, ] <- rep(0, length(caafleet[icount, ]))
          }
        }
      }
      
      # put new values into sim.dat object
      sim.dat$dat$CAA_mats[[ifleet]] <- cbind(caanew, ctotnew)
    }
    
    #--------------------------------
    # handle each index one at a time
    for (ind in 1:asap$parms$nindices){
      
      iaa_mat <- asap.dat$dat$IAA_mats[[ind]]
      sim_mat <- iaa_mat
      pred_index <- asap$index.pred[[ind]]
      
      # generate new index observations, only replace positive values
      indval <- iaa_mat[,2]
      sigma <- sqrt(log(1 + iaa_mat[,3] ^ 2))
      posvalcount <- 0
      for (icount in 1:length(indval)){
        if (indval[icount] > 0){
          posvalcount <- posvalcount + 1
          randomval <- stats::rnorm(1)
          sim_mat[icount, 2] <- pred_index[posvalcount] * exp(randomval * sigma[icount])
        }
      }
      
      # generate new index at age proportions for years with ess > 0
      ess <- iaa_mat[, (asap$parms$nages + 4)]
      mycols <- seq(4, (length(iaa_mat[1,]) - 1))
      index_agecomp <- asap$index.comp.mats[[(ind * 2)]] # comp mats have both obs and prd
      for (icount in 1:length(ess)){
        if (ess[icount] > 0){
          iaavals <- index_agecomp[icount, ]
          sumiaavals <- sum(iaavals)
          if (sumiaavals > 0){
            myprob <- iaavals / sumiaavals
            sim_mat[icount, mycols] <- stats::rmultinom(1, ess[icount], prob=myprob)
          }
        }
      }
      
      # put into sim.dat object
      sim.dat$dat$IAA_mats[[ind]] <- sim_mat
    }
    
    #--------------------------
    # write this simulated data
    fname <- file.path(od, paste0(asap.name, "_sim", isim, ".dat"))
    header.text <- "File created with SimASAP"
    WriteASAP3DatFile(fname,sim.dat,header.text)
    
    #----------------------------
    #do split_survey once
    pre_split <- file.path(od, paste0(asap.name, "_sim", isim, ".dat"))
    split1 <- split_survey(pre_split,1,2009)
    fname <- file.path(od, paste0(asap.name, "_sim", isim, ".dat"))
    header.text <- "File created with sim_then_split_ASAP"
    WriteASAP3DatFile(fname,split1,header.text)
    
    #again!
    pre_split2 <- file.path(od, paste0(asap.name, "_sim", isim, ".dat"))
    split2 <- split_survey.2(pre_split2,2,2009)
    fname <- file.path(od, paste0(asap.name, "_sim", isim, ".dat"))
    header.text <- "File created with sim_then_split_ASAP"
    WriteASAP3DatFile(fname,split2,header.text)
    
  } # end of isim data file creation loop
  
  #-----------------------------------
  # optionally run all the simulations
  if (runflag == TRUE){
    file.copy(from = file.path(wd, "ASAP3.EXE"), to = od, overwrite = FALSE)
    orig.dir <- getwd()
    setwd(od)
    
    for (isim in 1:nsim){
      setwd("C:/Users/ccarrano/Documents/simulation & self testing/R/Cross testing/Plaice cross/sim")
      sname <- paste0(asap.name, "_sim", isim)
      dname <- paste0(sname, ".dat")
      shell("del asap3.rdat", intern = TRUE)
      shell("del asap3.std", intern = TRUE)
      shell(paste("ASAP3.exe -ind", dname), intern=TRUE)
      # use presence of .std file to indicate converged run
      if (file.exists("asap3.std")){
        shell(paste("copy asap3.rdat", paste0(sname, ".rdat")), intern=TRUE)
        asap <- dget("asap3.rdat")
        objfxn <- asap$like$lk.total
        print(paste("simulation", isim, "complete, objective function =", objfxn))
      }else{
        print(paste("simulation", isim, "did not converge"))
      }
    }
    setwd(orig.dir)
  }
  
  return("OK")
}


############################################################################

#       Unsplitting function                                              #

# datf=asap file to be split (with .dat extension)
# surveynumber=two surveys to be combined
# yearsplit= year to combine/unsplit

# What this will actually do is add two new indices and fill them with the data combined from the split indices
# Definitely could have named it "combine_survey"... but here we are
# There is also hard coding or assumptions in here. For example, I think it assumes
# two split indices which are listed in a specifc order: season1_old, season2_old, season1_new, season2_new
unsplit_survey <- function(datf, surveynumber, yearsplit){
  orig <- ReadASAP3DatFile(datf)
  mod <- orig
  
  
  mod$dat$n_indices <- mod$dat$n_indices + 2 
  ni <- mod$dat$n_indices # number of indices with new indices added  
  nim1 <- mod$dat$n_indices-1 # number of indices minus 1
  
  mod$survey.names[nim1] <- "Spring-FULL" # Second to last index will be spring full
  mod$survey.names[ni] <- "Fall-FULL"      # Last index will be fall full
  
  mod$dat$index_units[nim1:ni] <- mod$dat$index_units[surveynumber]  #use the same index units as the split
  mod$dat$index_acomp_units[nim1:ni] <- mod$dat$index_acomp_units[surveynumber] #use the same age comp units as the split
  mod$dat$index_WAA_pointers[nim1] <- mod$dat$index_WAA_pointers[surveynumber] #use the same WAA matrix as the "surveynumber index" i.e. 1
  mod$dat$index_WAA_pointers[ni] <- mod$dat$index_WAA_pointers[surveynumber+1] #use the same WAA matrix as the "surveynumber +1 index" i.e. 2
  mod$dat$index_month[nim1] <- mod$dat$index_month[surveynumber] #use the same month  as the "surveynumber index" i.e. 1
  mod$dat$index_month[ni] <- mod$dat$index_month[surveynumber+1] #use the same month as the "surveynumber +1 index" i.e. 2
  mod$dat$index_sel_choice[nim1] <- mod$dat$index_sel_choice[surveynumber]
  mod$dat$index_sel_choice[ni] <- mod$dat$index_sel_choice[surveynumber+1]
  mod$dat$index_sel_option[nim1] <- mod$dat$index_sel_option[surveynumber]
  mod$dat$index_sel_option[ni] <- mod$dat$index_sel_option[surveynumber+1]
  mod$dat$index_sel_start_age[nim1] <- mod$dat$index_sel_start_age[surveynumber]
  mod$dat$index_sel_start_age[ni] <- mod$dat$index_sel_start_age[surveynumber+1]
  mod$dat$index_sel_end_age[nim1] <- mod$dat$index_sel_end_age[surveynumber]
  mod$dat$index_sel_end_age[ni] <- mod$dat$index_sel_end_age[surveynumber+1]
  mod$dat$use_index_acomp[nim1] <- mod$dat$use_index_acomp[surveynumber]
  mod$dat$use_index_acomp[ni] <- mod$dat$use_index_acomp[surveynumber+1]
  mod$dat$use_index_acomp[surveynumber:(surveynumber+3)] <- 0 # turn off the original surveys
  mod$dat$use_index[nim1:ni] <- 1
  mod$dat$use_index[surveynumber:(surveynumber+3)] <- 0 # turn off the original surveys
  
  mod$dat$index_sel_ini[[nim1]] <- mod$dat$index_sel_ini[[surveynumber]]
  mod$dat$index_sel_ini[[ni]] <- mod$dat$index_sel_ini[[surveynumber+1]]
  
  
  #combine index[surveynumber] and index [surveynumber+2] into one matrix
      #should be an albatross and a bigelow from the same season (spring)
  inddat1 <- rbind(subset(mod$dat$IAA_mats[[surveynumber]],mod$dat$IAA_mats[[surveynumber]][,1] < yearsplit),
                   subset(mod$dat$IAA_mats[[surveynumber+2]],mod$dat$IAA_mats[[surveynumber+2]][,1] >= yearsplit))
  #combine index[surveynumber+1] and index [surveynumber+3] into one matrix
    #should be an albatross and a bigelow from the same season (fall)
  inddat2 <- rbind(subset(mod$dat$IAA_mats[[surveynumber+1]],mod$dat$IAA_mats[[surveynumber+1]][,1] < yearsplit),
                   subset(mod$dat$IAA_mats[[surveynumber+3]],mod$dat$IAA_mats[[surveynumber+3]][,1] >= yearsplit))
  mod$dat$IAA_mats[[nim1]] <- inddat1
  mod$dat$IAA_mats[[ni]] <- inddat2

  mod$dat$lambda_index[nim1] <- mod$dat$lambda_index[surveynumber]
  mod$dat$lambda_index[ni] <- mod$dat$lambda_index[surveynumber+1]
  
  mod$dat$lambda_q[nim1] <- mod$dat$lambda_q[surveynumber]
  mod$dat$lambda_q[ni] <- mod$dat$lambda_q[surveynumber+1]
  mod$dat$cv_q[nim1] <- mod$dat$cv_q[surveynumber]
  mod$dat$cv_q[ni] <- mod$dat$cv_q[surveynumber+1]
  mod$dat$lambda_q_devs[nim1] <- mod$dat$lambda_q_devs[surveynumber]
  mod$dat$lambda_q_devs[ni] <- mod$dat$lambda_q_devs[surveynumber+1]
  mod$dat$cv_q_devs[nim1] <- mod$dat$cv_q_devs[surveynumber]
  mod$dat$cv_q_devs[ni] <- mod$dat$cv_q_devs[surveynumber+1]
  
  mod$dat$q_ini[nim1] <- mod$dat$q_ini[surveynumber]
  mod$dat$q_ini[ni] <- mod$dat$q_ini[surveynumber+1]
  
  xx <- 1:length(mod$comments)
  cut1 <- xx[mod$comments == paste0("# Index-", ni-2, " Selectivity Data  ")]
  cut2 <- xx[mod$comments == paste0("# Index-", ni-2, " Data  ")]
  modcomments <- c(mod$comments[1:cut1], "# Spring-FULL index Selectivity Data ", "# Fall-FULL index Selectivity Data",
                   mod$comments[(cut1+1):cut2], "# Spring-FULL Index Data ", "# Fall-FULL Index Data",
                   mod$comments[(cut2+1):length(mod$comments)])
  mod$comments <- modcomments
  
  return(mod)
}
##############################################################################
##############################################################################

#This function simulates ASAP files, then "unsplits" (aka combines,
# idk why I named it "unsplit") four indices into two full indices, then 
# optionally fits those input files in ASAP
# Watch out for hard coding of directories but more likely of survey numbers and
# implied data structures of the ASAP files

sim_then_unsplit_ASAP <- function(wd, asap.name, nsim, od=file.path(wd, "sim"), runflag=FALSE){
  
  # error checks for missing files
  if (!file.exists(file.path(wd, paste0(asap.name, ".dat")))){
    return(paste0("Error: ", asap.name, ".dat not located in ", wd))
  }
  
  if (!file.exists(file.path(wd, paste0(asap.name, ".rdat")))){
    return(paste0("Error: ", asap.name, ".rdat not located in ", wd))
  }
  
  if (runflag == TRUE & !file.exists(file.path(wd, "ASAP3.exe"))){
    return(paste0("Error: ASAP3.exe not located in ", wd))
  }
  
  if (!dir.exists(od)){
    dir.create(od)
  }
  
  # get the dat and rdat files
  asap.dat <- ReadASAP3DatFile(file.path(wd, paste0(asap.name, ".dat")))
  asap <- dget(file.path(wd, paste0(asap.name, ".rdat")))
  
  # begin simulation loop to create data sets
  for (isim in 1:nsim){
    
    sim.dat <- asap.dat
    
    # handle each fleet one at a time
    for (ifleet in 1:asap$parms$nfleets){
      
      # generate new total catch observations
      ctotfleet <- asap$catch.pred[ifleet, ]
      sigma <- sqrt(log(1 + asap$control.parms$catch.tot.cv[, ifleet] ^ 2))
      randomval <- stats::rnorm(length(ctotfleet))
      ctotnew <- ctotfleet * exp(randomval * sigma)
      
      # generate new catch at age proportions
      myval <- (ifleet - 1) * 4 + 2 # obs and pred for catch and discards for each fleet, catch pred 2nd
      caafleet <- asap$catch.comp.mats[[myval]]
      caanew <- caafleet
      ess <- asap$fleet.catch.Neff.init[ifleet, ]
      for (icount in 1:length(ess)){
        if (ess[icount] > 0){
          sumcaa <- sum(caafleet[icount, ])
          if (sumcaa > 0){
            myprob <- caafleet[icount, ] / sumcaa
            caanew[icount, ] <- stats::rmultinom(1, ess[icount], prob=myprob)
          } else {
            caanew[icount, ] <- rep(0, length(caafleet[icount, ]))
          }
        }
      }
      
      # put new values into sim.dat object
      sim.dat$dat$CAA_mats[[ifleet]] <- cbind(caanew, ctotnew)
    }
    
    #--------------------------------
    # handle each index one at a time
    for (ind in 1:asap$parms$nindices){
      
      iaa_mat <- asap.dat$dat$IAA_mats[[ind]]
      sim_mat <- iaa_mat
      pred_index <- asap$index.pred[[ind]]
      
      # generate new index observations, only replace positive values
      indval <- iaa_mat[,2]
      sigma <- sqrt(log(1 + iaa_mat[,3] ^ 2))
      posvalcount <- 0
      for (icount in 1:length(indval)){
        if (indval[icount] > 0){
          posvalcount <- posvalcount + 1
          randomval <- stats::rnorm(1)
          sim_mat[icount, 2] <- pred_index[posvalcount] * exp(randomval * sigma[icount])
        }
      }
      
      # generate new index at age proportions for years with ess > 0
      ess <- iaa_mat[, (asap$parms$nages + 4)]
      mycols <- seq(4, (length(iaa_mat[1,]) - 1))
      index_agecomp <- asap$index.comp.mats[[(ind * 2)]] # comp mats have both obs and prd
      for (icount in 1:length(ess)){
        if (ess[icount] > 0){
          iaavals <- index_agecomp[icount, ]
          sumiaavals <- sum(iaavals)
          if (sumiaavals > 0){
            myprob <- iaavals / sumiaavals
            sim_mat[icount, mycols] <- stats::rmultinom(1, ess[icount], prob=myprob)
          }
        }
      }
      
      # put into sim.dat object
      sim.dat$dat$IAA_mats[[ind]] <- sim_mat
    }
    
    #--------------------------
    # write this simulated data
    fname <- file.path(od, paste0(asap.name, "_sim", isim, ".dat"))
    header.text <- "File created with SimASAP"
    WriteASAP3DatFile(fname,sim.dat,header.text)
    
    #----------------------------
    #"Unsplit" by adding in the full indices
    pre_unsplit <- file.path(od, paste0(asap.name, "_sim", isim, ".dat"))
    full.mod <- unsplit_survey(pre_unsplit,1,2009)
    fname <- file.path(od, paste0(asap.name, "_sim", isim, ".dat"))
    header.text <- "File created with sim_then_split_ASAP"
    WriteASAP3DatFile(fname,full.mod,header.text)
    
    
  } # end of isim data file creation loop
  
  #-----------------------------------
  # optionally run all the simulations
  if (runflag == TRUE){
    file.copy(from = file.path(wd, "ASAP3.EXE"), to = od, overwrite = FALSE)
    orig.dir <- getwd()
    setwd(od)
    
    for (isim in 1:nsim){
      setwd(od)
      sname <- paste0(asap.name, "_sim", isim)
      dname <- paste0(sname, ".dat")
      shell("del asap3.rdat", intern = TRUE)
      shell("del asap3.std", intern = TRUE)
      shell(paste("ASAP3.exe -ind", dname), intern=TRUE)
      # use presence of .std file to indicate converged run
      if (file.exists("asap3.std")){
        shell(paste("copy asap3.rdat", paste0(sname, ".rdat")), intern=TRUE)
        asap <- dget("asap3.rdat")
        objfxn <- asap$like$lk.total
        print(paste("simulation", isim, "complete, objective function =", objfxn))
      }else{
        print(paste("simulation", isim, "did not converge"))
      }
    }
    setwd(orig.dir)
  }
  
  return("OK")
}


################################################################################
#         edit plot_sim_asap function
################################################################################

#getting rid of the extra "sim" in filepath because my wd is going to be the sim folder
PlotSimSplitASAP <- function(wd, asap.name, whichsim, od=file.path(wd), asapretro=FALSE, save.plots=FALSE, returnwhat="nothing"){
  
  # check for valid returnwhat value
  validoptions <- c("results", "plot", "both", "nothing")
  if (!(returnwhat %in% validoptions)) {
    return(paste("Error: returnwhat must be one of:", paste(validoptions, collapse = ", ")))
  }
  
  # check for files
  rdat_ext <- ifelse(asapretro == FALSE, ".rdat", "_000.rdat")
  
  if (!file.exists(file.path(wd, paste0(asap.name, rdat_ext)))) {
    return(paste0("Error: ", asap.name, rdat_ext, " not located in ", wd))
  }
  
  simrdats <- FALSE
  for (isim in 1:length(whichsim)) {
    if(file.exists(file.path(od, paste0(asap.name, "_sim", isim, rdat_ext)))) {
      simrdats <- TRUE
    }
  }
  if (simrdats == FALSE) {
    return(paste0("Error: no files ", asap.name, "_sim(whichsim)", rdat_ext," located in ", od))
  }
  
  # get true values
  asap <- dget(file.path(wd, paste0(asap.name, rdat_ext)))
  years <- seq(asap$parms$styr, asap$parms$endyr)
  nyears <- length(years)
  res <- data.frame(Source = "True",
                    metric = rep(c("SSB", "Freport", "Recruits"), each=nyears),
                    Year = rep(years, 3),
                    value = c(asap$SSB, asap$F.report, asap$N.age[,1]))
  
  # get simulated values
  for (isim in 1:length(whichsim)) {
    mysim <- whichsim[isim]
    fname <- file.path(od, paste0(asap.name, "_sim", mysim, rdat_ext))
    if (file.exists(fname)) {
      asap <- dget(fname)
      simres <- data.frame(Source = paste0("Sim", mysim),
                           metric = rep(c("SSB", "Freport", "Recruits"), each=nyears),
                           Year = rep(years, 3),
                           value = c(asap$SSB, asap$F.report, asap$N.age[,1]))
      res <- rbind(res, simres)
    }
  }
  
  # make plot and optionally save
  p <- ggplot2::ggplot(res, aes(x=Year, y=value, color=Source)) +
    geom_line() +
    geom_point(data=dplyr::filter(res, Source == "True")) +
    facet_wrap(~metric, ncol = 1, scales = "free_y") +
    expand_limits(y=0) +
    theme_bw()
  
  if (length(unique(res$Source)) > 5){
    p <- p + theme(legend.position = "none")
  }
  
  print(p)
  if (save.plots == TRUE){
    ggplot2::ggsave(filename = file.path(od, "comparisonplots.png"), p)
  }
  
  myreturn <- NULL
  if (returnwhat == "results"){
    myreturn <- res
  } else if (returnwhat == "plot"){
    myreturn <- p
  } else if (returnwhat == "both"){
    myreturn <- list(res = res, p = p)
  }
  return(myreturn)
}


##############################################################################
##############################################################################

#     Function to pull out and plot q's?

# wd <- where sim files are stored, make sure original (true) file is in there too
# asap.name <-  name of true file
Plot_q_simASAP <- function(wd, asap.name, whichsim, od=file.path(wd), asapretro=FALSE, save.plots=FALSE, returnwhat="nothing"){
  
  
  
  # check for files
  rdat_ext <- ifelse(asapretro == FALSE, ".rdat", "_000.rdat")
  
  if (!file.exists(file.path(wd, paste0(asap.name, rdat_ext)))) {
    return(paste0("Error: ", asap.name, rdat_ext, " not located in ", wd))
  }
  
  simrdats <- FALSE
  for (isim in 1:length(whichsim)) {
    if(file.exists(file.path(od, paste0(asap.name, "_sim", isim, rdat_ext)))) {
      simrdats <- TRUE
    }
  }
  if (simrdats == FALSE) {
    return(paste0("Error: no files ", asap.name, "_sim(whichsim)", rdat_ext," located in ", od))
  }
  
  # get true q values
  asap <- dget(file.path(wd, paste0(asap.name, rdat_ext)))
  years <- seq(asap$parms$styr, asap$parms$endyr)
  nyears <- length(years)
  trueq <- data.frame(Source = "True",
                      index_name = names(asap$index.year),
                      value = asap$q.indices)
  
  # get simulated q values
  allQs <- trueq
  for (isim in 1:length(whichsim)) {
    mysim <- whichsim[isim]
    fname <- file.path(od, paste0(asap.name, "_sim", mysim, rdat_ext))
    if (file.exists(fname)) {
      asap <- dget(fname)
      simq <- data.frame(Source = paste0("Sim", mysim),
                           index_name = rep(names(asap$index.year)),
                           value = asap$q.indices)}
      allQs <- rbind(allQs, simq)
  }
  return(allQs)
  }

##############################################################################
##############################################################################

#     Function to pull out and plot index sel

# wd <- where sim files are stored, make sure original (true) file is in there too
# asap.name <-  name of true file
Plot_isel_simASAP <- function(wd, asap.name, whichsim, od=file.path(wd), asapretro=FALSE, save.plots=FALSE, returnwhat="nothing"){
  
  
  
  # check for files
  rdat_ext <- ifelse(asapretro == FALSE, ".rdat", "_000.rdat")
  
  if (!file.exists(file.path(wd, paste0(asap.name, rdat_ext)))) {
    return(paste0("Error: ", asap.name, rdat_ext, " not located in ", wd))
  }
  
  simrdats <- FALSE
  for (isim in 1:length(whichsim)) {
    if(file.exists(file.path(od, paste0(asap.name, "_sim", isim, rdat_ext)))) {
      simrdats <- TRUE
    }
  }
  if (simrdats == FALSE) {
    return(paste0("Error: no files ", asap.name, "_sim(whichsim)", rdat_ext," located in ", od))
  }
  
  # get true sel values
  asap <- dget(file.path(wd, paste0(asap.name, rdat_ext)))
  years <- seq(asap$parms$styr, asap$parms$endyr)
  nyears <- length(years)
  truesel <- data.frame(Source = "True",
                      index_name = names(asap$index.year),
                      value = asap$index.sel)
  
  # get simulated sel values
  allsel <- truesel
  for (isim in 1:length(whichsim)) {
    mysim <- whichsim[isim]
    fname <- file.path(od, paste0(asap.name, "_sim", mysim, rdat_ext))
    if (file.exists(fname)) {
      asap <- dget(fname)
      simsel <- data.frame(Source = paste0("Sim", mysim),
                         index_name = rep(names(asap$index.year)),
                         value = asap$index.sel)}
    allsel <- rbind(allsel, simsel)
  }
  return(allsel)
}
#############################################################################
###############################################################################

# sim then split and change M function

sim_split_M <- function(wd, asap.name, nsim, M_value, od=file.path(wd, "sim"), runflag=FALSE){
  
  # error checks for missing files
  if (!file.exists(file.path(wd, paste0(asap.name, ".dat")))){
    return(paste0("Error: ", asap.name, ".dat not located in ", wd))
  }
  
  if (!file.exists(file.path(wd, paste0(asap.name, ".rdat")))){
    return(paste0("Error: ", asap.name, ".rdat not located in ", wd))
  }
  
  if (runflag == TRUE & !file.exists(file.path(wd, "ASAP3.exe"))){
    return(paste0("Error: ASAP3.exe not located in ", wd))
  }
  
  if (!dir.exists(od)){
    dir.create(od)
  }
  
  # get the dat and rdat files
  asap.dat <- ReadASAP3DatFile(file.path(wd, paste0(asap.name, ".dat")))
  asap <- dget(file.path(wd, paste0(asap.name, ".rdat")))
  
  # begin simulation loop to create data sets
  for (isim in 1:nsim){
    
    sim.dat <- asap.dat
    
    # add new M
    n.rows <- length(asap.dat$dat$M[,1])
    n.cols <- length(asap.dat$dat$M[1,])
    sim.dat$dat$M <- matrix(rep(M_value,n.rows*n.cols), nrow = n.rows, ncol = n.cols)
    
    # handle each fleet one at a time
    for (ifleet in 1:asap$parms$nfleets){
      
      # generate new total catch observations
      ctotfleet <- asap$catch.pred[ifleet, ]
      sigma <- sqrt(log(1 + asap$control.parms$catch.tot.cv[, ifleet] ^ 2))
      randomval <- stats::rnorm(length(ctotfleet))
      ctotnew <- ctotfleet * exp(randomval * sigma)
      
      # generate new catch at age proportions
      myval <- (ifleet - 1) * 4 + 2 # obs and pred for catch and discards for each fleet, catch pred 2nd
      caafleet <- asap$catch.comp.mats[[myval]]
      caanew <- caafleet
      ess <- asap$fleet.catch.Neff.init[ifleet, ]
      for (icount in 1:length(ess)){
        if (ess[icount] > 0){
          sumcaa <- sum(caafleet[icount, ])
          if (sumcaa > 0){
            myprob <- caafleet[icount, ] / sumcaa
            caanew[icount, ] <- stats::rmultinom(1, ess[icount], prob=myprob)
          } else {
            caanew[icount, ] <- rep(0, length(caafleet[icount, ]))
          }
        }
      }
      
      # put new values into sim.dat object
      sim.dat$dat$CAA_mats[[ifleet]] <- cbind(caanew, ctotnew)
    }
    
    #--------------------------------
    # handle each index one at a time
    for (ind in 1:asap$parms$nindices){
      
      iaa_mat <- asap.dat$dat$IAA_mats[[ind]]
      sim_mat <- iaa_mat
      pred_index <- asap$index.pred[[ind]]
      
      # generate new index observations, only replace positive values
      indval <- iaa_mat[,2]
      sigma <- sqrt(log(1 + iaa_mat[,3] ^ 2))
      posvalcount <- 0
      for (icount in 1:length(indval)){
        if (indval[icount] > 0){
          posvalcount <- posvalcount + 1
          randomval <- stats::rnorm(1)
          sim_mat[icount, 2] <- pred_index[posvalcount] * exp(randomval * sigma[icount])
        }
      }
      
      # generate new index at age proportions for years with ess > 0
      ess <- iaa_mat[, (asap$parms$nages + 4)]
      mycols <- seq(4, (length(iaa_mat[1,]) - 1))
      index_agecomp <- asap$index.comp.mats[[(ind * 2)]] # comp mats have both obs and prd
      for (icount in 1:length(ess)){
        if (ess[icount] > 0){
          iaavals <- index_agecomp[icount, ]
          sumiaavals <- sum(iaavals)
          if (sumiaavals > 0){
            myprob <- iaavals / sumiaavals
            sim_mat[icount, mycols] <- stats::rmultinom(1, ess[icount], prob=myprob)
          }
        }
      }
      
      # put into sim.dat object
      sim.dat$dat$IAA_mats[[ind]] <- sim_mat
    }
    
    #--------------------------
    # write this simulated data
    fname <- file.path(od, paste0(asap.name, "_sim", isim, ".dat"))
    header.text <- "File created with SimASAP"
    WriteASAP3DatFile(fname,sim.dat,header.text)
    
    # #----------------------------
    #do split_survey once
    #pre_split <- file.path(od, paste0(asap.name, "_sim", isim, ".dat"))
    split1 <- split_survey(fname,1,2009)
    fname <- file.path(od, paste0(asap.name, "_sim", isim, ".dat"))
    header.text <- "File created with sim_then_split_ASAP"
    WriteASAP3DatFile(fname,split1,header.text)
    
    #again!
    pre_split2 <- file.path(od, paste0(asap.name, "_sim", isim, ".dat"))
    split2 <- split_survey.2(pre_split2,2,2009)
    fname <- file.path(od, paste0(asap.name, "_sim", isim, ".dat"))
    header.text <- "File created with sim_then_split_ASAP"
    WriteASAP3DatFile(fname,split2,header.text)
    
  } # end of isim data file creation loop
  
  #-----------------------------------
  # optionally run all the simulations
  if (runflag == TRUE){
    file.copy(from = file.path(wd, "ASAP3.EXE"), to = od, overwrite = FALSE)
    orig.dir <- getwd()
    setwd(od)
    
    for (isim in 1:nsim){
      setwd("C:/Users/ccarrano/Documents/simulation & self testing/R/Cross testing/Plaice cross/Plaice_M/sim")
      sname <- paste0(asap.name, "_sim", isim)
      dname <- paste0(sname, ".dat")
      shell("del asap3.rdat", intern = TRUE)
      shell("del asap3.std", intern = TRUE)
      shell(paste("ASAP3.exe -ind", dname), intern=TRUE)
      # use presence of .std file to indicate converged run
      if (file.exists("asap3.std")){
        shell(paste("copy asap3.rdat", paste0(sname, ".rdat")), intern=TRUE)
        asap <- dget("asap3.rdat")
        objfxn <- asap$like$lk.total
        print(paste("simulation", isim, "complete, objective function =", objfxn))
      }else{
        print(paste("simulation", isim, "did not converge"))
      }
    }
    setwd(orig.dir)
  }
  
  return("OK")
}
