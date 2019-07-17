

############# Columns: 61 - 79 #################

Added Columns
DtSold <- Combination of YrSold and MoSold as datetime (the day of each date was arbitrarily chosen as 1)



Dropped Columns
MoSold <- information is contained within DtSold
YrSold <- information is contained within DtSold

Dummifyied every catagorical column
MiscFeature <- overwrote two cells of named features that were priced zero. Classified as none.
    The named observations comprise less than 4% of the total data, so that column is not very valuable.
GarageCond, GarageQual, GarageFinish, PoolQC were labeled 'None' for missing obs that were matched with Zero valuations.
