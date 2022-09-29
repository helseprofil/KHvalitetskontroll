## Input

#- All inputs are provided in the code chunk below.
#
#[**Define:**]{.underline}
#
#-   **dfnew**: New KUBE to be checked
#-   **dfold**: Old KUBE for comparison
#
#In `ReadFile()`, specify KUBE name, bank ("KH" or "NH"), and FOLDER (4-digit year, or "DATERT"). The function locates the correct folder, and looks for the csv-file matching the KUBE name. If there are more than one KUBE with the same name, all potential matches are listed and the KUBE name must be further specified to read a specific file.
#
#-   **STANDARDdims**: All standard dimensions in KUBE
#
#-   **EXTRAdims** (If none, write `NULL`)
#
#-   **PRIKKval**: The target value column for PRIKKING
#
#-   **PRIKKlimit**: Threshold for PRIKKING
#
#-   **COMPAREval**: Which variable should be compared across LAND, FYLKE, KOMMUNE, BYDEL, etc.
#
#-   **GROUPdims**: Dimensions to group comparison output by
#
#[**Input types:**]{.underline} - `*dims` refer to dimension columns - `*val` refer to value columns - `*limit` refer to thresholds, e.g. limit for PRIKKING