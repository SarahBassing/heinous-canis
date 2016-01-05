###################################################################################################
################ Data Wrangling to get Hunter Survey Data from AESRD Organized ####################
################### to Plot Across Study Area and make Encounter Histories ########################
##################################################################################################


  setwd("C:/Sarah B/Thesis POM/Scripts")
  library(dplyr)
  library(tidyr)
  

###################################################################################################
############### Starting with 2012 Hunter Survey Data: Sept. 1- Dec. 31, 2012 #####################
###################################################################################################


  Hunt12 <- read.csv("C:/Sarah B/Thesis POM/Data/Hunter_Surveys/Hunter_Survey_2012.csv")
  
  #tbl_df(Hunt12)   # View large dataset without printing entire thing
  df12 <- Hunt12[-c(1,3:19,27:28,35:36,43:44,51:52,59:60,67:68,75:76)]  # Remove uneeded columns (hunter days, etc.)
  
  # Rename remaining columns to something less stupid
  colnames(df12) <- c("Hunter.ID", "See.Wolves", "Month", "Days", "Wolves.Obs", "Section", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Range", "Township")
  
  df12.Nono <- df12[!df12$See.Wolves=="No",]   # Remove all hunters that did not report seeing wolves
  df12.NoSee <- df12.Nono[-2]  # Remove column asking whether wolves were observed
  
  # Function to find and replace characters in a column
  gsr <- function(Source, Search, Replace) 
  { 
    if (length(Search) != length(Replace)) 
      stop("Search and Replace Must Have Equal Number of Items\n") 
    
    Changed <- as.character(Source) 
    
    for (i in 1:length(Search)) 
    { 
      cat("Replacing: ", Search[i], " With: ", Replace[i], "\n") 
      Changed <- replace(Changed, Changed == Search[i], Replace[i]) 
    } 
    
    cat("\n") 
    
    Changed 
  }
  
  # Finding and replacing the " to " in Days column into weeks 1-4 (the spaces make things complicated later on)
  df12.NoSee$Days <- gsr(df12.NoSee$Days, 
                        c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df12.NoSee$Days.1 <- gsr(df12.NoSee$Days.1, 
                          c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df12.NoSee$Days.2 <- gsr(df12.NoSee$Days.2, 
                          c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df12.NoSee$Days.3 <- gsr(df12.NoSee$Days.3, 
                          c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df12.NoSee$Days.4 <- gsr(df12.NoSee$Days.4, 
                          c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df12.NoSee$Days.5 <- gsr(df12.NoSee$Days.5, 
                          c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df12.NoSee$Days.6 <- gsr(df12.NoSee$Days.6, 
                          c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))

  # Laying some serious pipe to unite blocks of columns pertaining to individual wolf observations
  df12.unite <- df12.NoSee %>% unite(Observation1, Month, Days, Wolves.Obs, Section, Range, Township, sep="_", remove=TRUE) %>%
    unite(Observation2, Month.1, Days.1, Wolves.Obs.1, Section.1, Range.1, Township.1, sep="_", remove=TRUE) %>%
    unite(Observation3, Month.2, Days.2, Wolves.Obs.2, Section.2, Range.2, Township.2, sep="_", remove=TRUE) %>%
    unite(Observation4, Month.3, Days.3, Wolves.Obs.3, Section.3, Range.3, Township.3, sep="_", remove=TRUE) %>%
    unite(Observation5, Month.4, Days.4, Wolves.Obs.4, Section.4, Range.4, Township.4, sep="_", remove=TRUE) %>%
    unite(Observation6, Month.5, Days.5, Wolves.Obs.5, Section.5, Range.5, Township.5, sep="_", remove=TRUE) %>%
    unite(Observation7, Month.6, Days.6, Wolves.Obs.6, Section.6, Range.6, Township.6, sep="_", remove=TRUE)
  
  
  ### Wrangling data from wide to long format while removing NA observations and single wolf sightings ###
  
  # Gathers all the united observation data columns into individual rows by single observation
  df12.gather <- gather(df12.unite, "Observation", "Info", 2:8)  
  
  # Removes all rows with NA's for observation data
  df12.noNA <- df12.gather[!df12.gather$Info=="__NA_NA_NA_NA",]  
  
  # Separates united observation data back into individual columns by each data point
  df12.sep <- separate(df12.noNA, Info, c("Month", "Week", "Wolves.Obs", "SEC", "RGE", "TWP"), remove=TRUE, convert=TRUE)   
  
  # Removes all single wolf observations
  # HS.12 is the final dataframe for spatial work
  HS.12 <- df12.sep[!df12.sep$Wolves.Obs=="1",]    
  
  # Can I pipe all of this so it's all one motion instead of saving all the inbetween states? Even pipe starting with unite()?
  #HS.12 <- df12.unite %>% gather("Observation", "Info", 2:8) %>%   # Gathers all the united observation data columns into rows by individual observation
    #df12.unite[!Hdf12.unite$Info=="__NA_NA_NA_NA_",] %>%  # Removes all rows with NA's for observation data
    #separate(Info, c("Month", "Week", "Wolves.Obs", "Section", "Range", "Township"), remove=TRUE, convert=TRUE) %>%   # Separates united observation data back into individual columns by each data point
    #df12.unite[!df12.unite$Wolves.Obs=="1",] # Removes single wolf sightings
  
###################################################################################################
################### Next up: 2013 Hunter Survey Data: Sept. 1- Dec. 31, 2013 ######################
###################################################################################################
  
  Hunt13 <- read.csv("C:/Sarah B/Thesis POM/Data/Hunter_Surveys/Hunter_Survey_2013.csv")
  
  #tbl_df(Hunt13)   # View large dataset without printing entire thing- don't run unless you really want to look at it
  df13 <- Hunt13[-c(1,3:38,46:47,54:55,62:63,70:71,78:79,86:87)]  # Remove uneeded columns (hunter days, etc.)
  
  # Rename remaining columns to something less stupid
  colnames(df13) <- c("Hunter.ID", "See.Wolves", "Month", "Days", "Wolves.Obs", "Section", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Range", "Township")
  
  df13.Nono <- df13[!df13$See.Wolves=="No",]   # Remove all hunters that did not report seeing wolves
  df13.NoSee <- df13.Nono[-2]  # Remove column asking whether wolves were observed
  
  # Finding and replacing the " to " in Days column into weeks 1-4 (the spaces make things complicated later on)
  # MUST have already run 2012 code above with find&replace function
  df13.NoSee$Days <- gsr(df13.NoSee$Days, 
                         c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df13.NoSee$Days.1 <- gsr(df13.NoSee$Days.1, 
                           c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df13.NoSee$Days.2 <- gsr(df13.NoSee$Days.2, 
                           c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df13.NoSee$Days.3 <- gsr(df13.NoSee$Days.3, 
                           c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df13.NoSee$Days.4 <- gsr(df13.NoSee$Days.4, 
                           c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df13.NoSee$Days.5 <- gsr(df13.NoSee$Days.5, 
                           c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  
  
  # Laying some serious pipe to unite blocks of columns pertaining to individual wolf observations
  # Observation1, etc. is the new column name, Month etc. are all being united together under each new column
  df13.unite <- df13.NoSee %>% unite(Observation1, Month, Days, Wolves.Obs, Section, Range, Township, sep="_", remove=TRUE) %>%
    unite(Observation2, Month.1, Days.1, Wolves.Obs.1, Section.1, Range.1, Township.1, sep="_", remove=TRUE) %>%
    unite(Observation3, Month.2, Days.2, Wolves.Obs.2, Section.2, Range.2, Township.2, sep="_", remove=TRUE) %>%
    unite(Observation4, Month.3, Days.3, Wolves.Obs.3, Section.3, Range.3, Township.3, sep="_", remove=TRUE) %>%
    unite(Observation5, Month.4, Days.4, Wolves.Obs.4, Section.4, Range.4, Township.4, sep="_", remove=TRUE) %>%
    unite(Observation6, Month.5, Days.5, Wolves.Obs.5, Section.5, Range.5, Township.5, sep="_", remove=TRUE)

  
  ### Wrangling data from wide to long format while removing NA observations and single wolf sightings ###
  
  # Gathers all the united observation data columns into individual rows by single observation
  df13.gather <- gather(df13.unite, "Observation", "Info", 2:7)  
  
  # Removes all rows with NA's for observation data
  df13.noNA <- df13.gather[!df13.gather$Info=="__NA_NA_NA_NA",]  
  
  # Separates united observation data back into individual columns by each data point
  df13.sep <- separate(df13.noNA, Info, c("Month", "Week", "Wolves.Obs", "SEC", "RGE", "TWP"), remove=TRUE, convert=TRUE)   
  
  # Removes all single wolf observations
  # HS.13 is the final dataframe for spatial work
  HS.13 <- df13.sep[!df13.sep$Wolves.Obs=="1",]
  
  
  # Can I pipe all of this so it's all one motion instead of saving all the inbetween states? Even pipe starting with unite()?
  #HS.13 <- df13.unite %>% gather("Observation", "Info", 2:7) %>%   # Gathers all the united observation data columns into rows by individual observation
  #df13.unite[!Hdf13.unite$Info=="__NA_NA_NA_NA",] %>%  # Removes all rows with NA's for observation data
  #separate(Info, c("Month", "Week", "Wolves.Obs", "Section", "Range", "Township"), remove=TRUE, convert=TRUE) %>%   # Separates united observation data back into individual columns by each data point
  #df13.unite[!df13.unite$Wolves.Obs=="1",] # Removes single wolf sightings
  
  
###################################################################################################
##############Last but not least: 2014 Hunter Survey Data: Sept. 1- Dec. 31, 2013 #################
################################################################################################### 
  
  Hunt14 <- read.csv("C:/Sarah B/Thesis POM/Data/Hunter_Surveys/Hunter_Survey_2014.csv")
  
  #tbl_df(Hunt14)   # View large dataset without printing entire thing
  df14 <- Hunt14[-c(1,3:39,48:49,57:58,66:67,75:76,84:85)]  # Remove uneeded columns (hunter days, etc.)
  
  # Rename remaining columns to something less stupid
  colnames(df14) <- c("Hunter.ID", "See.Wolves", "Month", "Days", "Wolves.Obs", "Section", "Meridian", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Meridian", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Meridian", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Meridian", "Range", "Township",
                      "Month", "Days", "Wolves.Obs", "Section", "Meridian", "Range", "Township")
  
  df14.Nono <- df14[!df14$See.Wolves=="No",]   # Remove all hunters that did not report seeing wolves
  df14.NoSee <- df14.Nono[-2]  # Remove column asking whether wolves were observed
  
  # Finding and replacing the " to " in Days column into weeks 1-4 (the spaces make things complicated later on)
  # MUST have already run 2012 code above with find&replace function
  df14.NoSee$Days <- gsr(df14.NoSee$Days, 
                         c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df14.NoSee$Days.1 <- gsr(df14.NoSee$Days.1, 
                           c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df14.NoSee$Days.2 <- gsr(df14.NoSee$Days.2, 
                           c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df14.NoSee$Days.3 <- gsr(df14.NoSee$Days.3, 
                           c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  df14.NoSee$Days.4 <- gsr(df14.NoSee$Days.4, 
                           c("1 to 7", "8 to 14", "15 to 21", "22 to 31"), c("1", "2", "3", "4"))
  
  # Laying some serious pipe to unite blocks of columns pertaining to individual wolf observations
  df14.unite <- df14.NoSee %>% unite(Observation1, Month, Days, Wolves.Obs, Section, Meridian, Range, Township, sep="_", remove=TRUE) %>%
    unite(Observation2, Month.1, Days.1, Wolves.Obs.1, Section.1, Meridian.1, Range.1, Township.1, sep="_", remove=TRUE) %>%
    unite(Observation3, Month.2, Days.2, Wolves.Obs.2, Section.2, Meridian.2, Range.2, Township.2, sep="_", remove=TRUE) %>%
    unite(Observation4, Month.3, Days.3, Wolves.Obs.3, Section.3, Meridian.3, Range.3, Township.3, sep="_", remove=TRUE) %>%
    unite(Observation5, Month.4, Days.4, Wolves.Obs.4, Section.4, Meridian.4, Range.4, Township.4, sep="_", remove=TRUE) 

  
  ### Wrangling data from wide to long format while removing NA observations and single wolf sightings ###
  
  # Gathers all the united observation data columns into individual rows by single observation
  df14.gather <- gather(df14.unite, "Observation", "Info", 2:6)  
  
  # Removes all rows with NA's for observation data
  df14.noNA <- df14.gather[!df14.gather$Info=="__NA_NA__NA_NA",]  
  
  # Separates united observation data back into individual columns by each data point
  df14.sep <- separate(df14.noNA, Info, c("Month", "Week", "Wolves.Obs", "SEC", "M", "RGE", "TWP"), remove=TRUE, convert=TRUE)   
  
  # Removes all single wolf observations
  # HS.14 is the final dataframe for spatial work
  HS.14 <- df14.sep[!df14.sep$Wolves.Obs=="1",]
  
  
  # Can I pipe all of this so it's all one motion instead of saving all the inbetween states? Even pipe starting with unite()?
  #HS.14 <- df14.unite %>% gather("Observation", "Info", 2:6) %>%   # Gathers all the united observation data columns into rows by individual observation
  #df14.unite[!Hdf14.unite$Info=="__NA_NA__NA_NA",] %>%  # Removes all rows with NA's for observation data
  #separate(Info, c("Month", "Week", "Wolves.Obs", "Section", "Meridian", "Range", "Township"), remove=TRUE, convert=TRUE) %>%   # Separates united observation data back into individual columns by each data point
  #df14.unite[!df14.unite$Wolves.Obs=="1",] # Removes single wolf sightings
  