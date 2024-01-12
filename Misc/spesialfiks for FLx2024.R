# Adds age column to old file for matching with new file.

ReadFiles(dfnew = "QC_FLx_utdn_KH_2023-12-13-16-26",
          foldernew = "QC",
          modusnew = "KH",
          dfold = "e30_utdn_KH_2023-01-12-16-39",
          folderold = "2023",
          modusold = "KH",
          recodeold = TRUE,
          recodenew = FALSE) 

dfold[, ALDER := 30]
FormatData(compareKUBE_name = "compare_vs_e30",
           dfold_flag_name = "e30_utdn_KH_2023-01-12-16-39_(old)_FLAGGED",
           overwrite = T)

ReadFiles(dfnew = "QC_FLx_utdn_KH_2023-12-13-16-26",
          foldernew = "QC",
          modusnew = "KH",
          dfold = "e0_KH_2023-01-12-16-39",
          folderold = "2023",
          modusold = "KH",
          recodeold = TRUE,
          recodenew = FALSE) 

dfold[, ALDER := 0]
FormatData(compareKUBE_name = "compare_vs_e0",
           dfold_flag_name = "e0_KH_2023-01-12-16-39_(old)_FLAGGED",
           overwrite = T)


ReadFiles(dfnew = "QC_FLx_UTDN_DIFF_KH_2023-12-13-16-26",
          foldernew = "QC",
          modusnew = "KH",
          dfold = "e30_UTDN_DIFF_KH_2023-01-12-16-39",
          folderold = "2023",
          modusold = "KH",
          recodeold = TRUE,
          recodenew = FALSE) 

dfold[, ALDER := 30]
FormatData(overwrite = T)
