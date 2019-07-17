library(tidyverse)
library(dplyr)
library(ggrepel)
library(stringr)


# This assumes that you are in a named folder in the root directory of the
# GitHub project.
housing = read.csv("../data/train.csv")
#housing = read.csv("../data/test.csv")



my_slice = housing[,1:21]

#### LotArea ####
# Removed LotFrontage, added area to Lot Area.
my_slice$LotFrontage[is.na(my_slice$LotFrontage)] = 0

my_slice$LotArea = my_slice$LotFrontage + my_slice$LotArea

my_slice$LotFrontage = NULL

#### Street ####
#new street values are 0 for Gravel and 1 for Pavement
my_slice$Street = ifelse(my_slice$Street == "Grvl", 0, 1)


#### Alley ####
#impute alley NA's.
my_slice$Alley = as.character(my_slice$Alley)
my_slice$Alley[is.na(my_slice$Alley)] = "None"

#1 = T, 0 = F. If both are 0 it implies there is no alley.
my_slice$Alley_Grvl = ifelse(my_slice$Alley == "Grvl", 1, 0)
my_slice$Alley_Pave = ifelse(my_slice$Alley == "Pave", 1, 0)
my_slice$Alley = NULL


#### House Style ####
my_slice$HouseStyle = NULL


#### Utilities ####
#Currently:
#         0 = Not All Included
#         1 = All Included
#
my_slice$Utilities = ifelse(my_slice$Utilities == "AllPub", 1, 0)

#### Land Slope ####
#both 0 if Gentle Slope, 1 otherwise.
my_slice$LandSlope_Sev = ifelse(my_slice$LandSlope == "Sev", 1, 0)
my_slice$LandSlope_Mod = ifelse(my_slice$LandSlope == "Mod", 1, 0)
my_slice$LandSlope = NULL



##### Lot Shapes ####
#Currently:
#
#         0 = Reg
#         1 = IR1
#         2 = IR2
#         3 = IR3
#
lot_list = unique(as.character(my_slice$LotShape))
my_slice$LotShape = ifelse(my_slice$LotShape == "Reg", 0,
              ifelse(my_slice$LotShape == "IR1", 1, 
                     ifelse(my_slice$LotShape == "IR2", 2, 3)))


#### Land Contour ####
#Currently:
#
#         0 = Level
#         1 = Bank
#         2 = Low
#         3 = HLS
#
contour_list = unique(as.character(my_slice$LandContour))

my_slice$LandContour = ifelse(my_slice$LandContour == contour_list[1], 0,
                           ifelse(my_slice$LandContour == contour_list[2], 1, 
                                  ifelse(my_slice$LandContour == contour_list[3], 2, 3)))

#### Lot Config ####
#elim FR3 category and cast rest to dummies, perhaps consider making this
#1 = CulDSac, 0 = everything else
#Currently:
#
#         0 = "Inside Lot"
#         1 = "Frontage on at least 2 Sides"
#         2 = "Corner Lot"
#         3 = "CulDSac"
#

my_slice$LotConfig = as.character(my_slice$LotConfig)
my_slice$LotConfig[my_slice$LotConfig == "FR3"] = "FR2"
config_list = unique(my_slice$LotConfig)
my_slice$LotConfig = ifelse(my_slice$LotConfig == config_list[1], 0,
                           ifelse(my_slice$LotConfig == config_list[2], 1, 
                                  ifelse(my_slice$LotConfig == config_list[3], 2, 3)))

#### BldgType ####
#consider eliminating the Twnhs or TwnhsE category, essentially similar
#otherwise, encoding is:
#
#         0 = 1 Family
#         1 = 2 Family Conversion
#         2 = Duplex
#         3 = Townhouse End Unit
#         4 = Townhouse Inside Unit
#         
bldg_list = unique(as.character(my_slice$BldgType))
my_slice$BldgType = ifelse(my_slice$BldgType == bldg_list[1], 0,
                            ifelse(my_slice$BldgType == bldg_list[2], 1, 
                                   ifelse(my_slice$BldgType == bldg_list[3], 2, 
                                          ifelse(my_slice$BldgType == bldg_list[4], 3, 4))))


#### MS Zoning ####
#Currently:
#
#         0 = Commercial
#         1 = Residential (Low Density)
#         2 = Residential (Medium Density)
#         3 = Residential (High Density)
#         4 = Floating Village Residential
#
zoning_list = unique(as.character(my_slice$MSZoning))
my_slice$MSZoning = ifelse(my_slice$MSZoning == zoning_list[3], 0,
                           ifelse(my_slice$MSZoning == zoning_list[1], 1, 
                                  ifelse(my_slice$MSZoning == zoning_list[2], 2, 
                                         ifelse(my_slice$MSZoning == zoning_list[5], 3, 4))))


#### Conditions ####
# -maybe consider turning this into a tuple?
# This block handles BOTH Condition1 and Condition2
# We paired down the nuance of the chart in the following way:
#
# NearStreet <- Artery, Feedr
# NearTrain <- RRNn, RRNe, RRAn, RRAe
# Pos_Att <- PosN, PosA
# Currently:
#
#         0 = Normal
#         1 = NearStreet
#         2 = NearTrain
#         3 = Residential (Low Density)
#
my_slice$Condition1 = as.character(my_slice$Condition1)
my_slice$Condition2 = as.character(my_slice$Condition2)

#consolidate
nearTrain = c("RRNn", "RRNe", "RRAn", "RRAe")
nearStreet = c("Artery", "Feedr")
pos_att = c("PosN", "PosA")
my_slice$Condition1[my_slice$Condition1 %in% nearTrain] = "nearTrain"
my_slice$Condition1[my_slice$Condition1 %in% nearStreet] = "nearStreet"
my_slice$Condition1[my_slice$Condition1 %in% pos_att] = "pos"
my_slice$Condition2[my_slice$Condition2 %in% nearTrain] = "nearTrain"
my_slice$Condition2[my_slice$Condition2 %in% nearStreet] = "nearStreet"
my_slice$Condition2[my_slice$Condition2 %in% pos_att] = "pos"

#dummy
cond_list = unique(my_slice$Condition1)
my_slice$Condition1 = ifelse(my_slice$Condition1 == cond_list[1], 0,
                            ifelse(my_slice$Condition1 == cond_list[2], 1, 
                                   ifelse(my_slice$Condition1 == cond_list[3], 2, 3)))

my_slice$Condition2 = ifelse(my_slice$Condition2 == cond_list[1], 0,
                             ifelse(my_slice$Condition2 == cond_list[2], 1, 
                                    ifelse(my_slice$Condition2 == cond_list[3], 2, 3)))

#making all lowercase, save to CSV
colnames(my_slice) = tolower(colnames(my_slice))

write.csv(my_slice, "clean_data_1_20.csv")
