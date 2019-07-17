Sam's Data Readme!

MS Zoning (mszoning)
Currently:
   0 = Commercial
   1 = Residential (Low Density)
   2 = Residential (Medium Density)
   3 = Residential (High Density)
   4 = Floating Village Residential

mssubclass: Identifies the type of dwelling involved in the sale.

   20	1-STORY 1946 & NEWER ALL STYLES
   30	1-STORY 1945 & OLDER
   40	1-STORY W/FINISHED ATTIC ALL AGES
   45	1-1/2 STORY - UNFINISHED ALL AGES
   50	1-1/2 STORY FINISHED ALL AGES
   60	2-STORY 1946 & NEWER
   70	2-STORY 1945 & OLDER
   75	2-1/2 STORY ALL AGES
   80	SPLIT OR MULTI-LEVEL
   85	SPLIT FOYER
   90	DUPLEX - ALL STYLES AND AGES
  120	1-STORY PUD (Planned Unit Development) - 1946 & NEWER
  150	1-1/2 STORY PUD - ALL AGES
  160	2-STORY PUD - 1946 & NEWER
  180	PUD - MULTILEVEL - INCL SPLIT LEV/FOYER
  190	2 FAMILY CONVERSION - ALL STYLES AND AGES


Lot Area (lotarea)
  Area of lot + frontage. Has been rolled into one category. LotFrontage has been dropped.

Street (street)
  0 = Gravel
  1 = Pavement

Lot Shape (lotshape)
  0 = Normal
  1 = IR1
  2 = IR2
  3 = IR3

Land Contour (landcontour)
   0 = Level
   1 = Bank
   2 = Low
   3 = HLS

Utilities (utilities)
   0 = Not All Included
   1 = All Included

Lot Config (lotconfig)
   0 = "Inside Lot"
   1 = "Frontage on at least 2 Sides"
   2 = "Corner Lot"
   3 = "CulDSac"

neighborhood
  Didn't know what to do with this one

conditions (both 1 & 2)
 We paired down the nuance of the chart in the following way:

 NearStreet <- Artery, Feedr
 NearTrain <- RRNn, RRNe, RRAn, RRAe
 Pos_Att <- PosN, PosA
 Currently:

   0 = Normal
   1 = NearStreet
   2 = NearTrain
   3 = Residential (Low Density)

Building Type (bldgtype)
   0 = 1 Family
   1 = 2 Family Conversion
   2 = Duplex
   3 = Townhouse End Unit
   4 = Townhouse Inside Unit

HouseStyle has been DROPPED, information is contained in MSSubClass

overallqual, overallcond
  Rating from 1 to 10 of quality and condition.

yearbuilt
  Year the building was constructed.

yearremodadd
  Year the building was remodeled

Alley
  alley_grvl : 0 if false, 1 if true
  alley_pave : 0 if false, 1 if true
  if both are 0, then there is no alley

LandSlope
  landslope_sev : 0 if false, 1 if true (severe slope)
  landslope_mod : 0 if false, 1 if true (moderate slope)
  if both are 0, then it is a gentle slope
