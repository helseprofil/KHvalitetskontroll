.usebranch(branch = "timeseries")

DUMPS <- NULL

# Example data files

# HKR 2024 vs 2023

ReadFiles(dfnew = "QC_HKR_2023-11-15-08-45",
          foldernew = "QC",
          modusnew = "KH",
          dfold = "HKR_2022-12-14-12-18",
          folderold = "2023",
          modusold = "KH",
          recodeold = TRUE,
          recodenew = FALSE)

# Create flagged files
FormatData()
