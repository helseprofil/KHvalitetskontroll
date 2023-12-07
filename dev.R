.usebranch(branch = "bydelsplott")

DUMPS <- NULL

# Example data files

# HKR 2024 vs 2023
ReadFiles(dfnew = "QC_HKR_2023-11-21-12-01",
          foldernew = "QC",
          modusnew = "KH",
          dfold = "HKR_2022-12-14-12-18",
          folderold = "KH2023NESSTAR_PreAllvis",
          modusold = "KH",
          recodeold = TRUE,
          recodenew = FALSE)

# KUHR 2024 vs 2023
ReadFiles(dfnew = "QC_KUHR_2023-12-08-12-43",
          foldernew = "QC",
          modusnew = "KH",
          dfold = "KUHR_2022-11-23-15-11",
          folderold = "KH2023NESSTAR_PreAllvis",
          modusold = "KH",
          recodeold = TRUE,
          recodenew = FALSE)

# Create flagged files
FormatData()
