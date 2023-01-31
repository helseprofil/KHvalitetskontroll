#Load packages
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(data.table)
library(DT)
library(gert)
library(rmarkdown)
library(tools)
library(ggplot2)
library(ggh4x)
library(norgeo)

# Load internal functions
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/misc.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_step1.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_step2.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_interactive.R")

# Load list of all dimensions, and make hidden object
source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R") 
.ALL_DIMENSIONS <- ALL_DIMENSIONS
rm(ALL_DIMENSIONS)

# Set ggplot theme and color palette
theme_set(theme_bw())
theme_update(legend.position = "top", 
             panel.grid.minor = element_blank(),
             text = element_text(color = "black"),
             plot.margin = margin(t = 1, b = 1, r = 1, unit = "cm"))


# Set global options
PROFILEYEAR <- 2023  # For saving in correct folder
DUMPS <- c("dfnew_flag", "dfold_flag", "compareKUBE") # Default is to create all file dumps

# Identify small/large kommune

# Run to update list with new BEFOLK_GK, current list generated from BEFOLK_GK_2023-01-04-10-25

#.SmallLargeKommune()
.smallkommune <- c(1111, 1112, 1114, 1133, 1134, 1135, 1144, 1145, 1151, 1160, 1511, 1514, 
                   1515, 1516, 1517, 1525, 1528, 1531, 1532, 1535, 1539, 1547, 1554, 1557, 
                   1560, 1563, 1566, 1573, 1576, 1578, 1811, 1812, 1813, 1815, 1816, 1818, 
                   1820, 1822, 1825, 1826, 1827, 1828, 1832, 1834, 1835, 1836, 1837, 1838, 
                   1839, 1840, 1841, 1845, 1848, 1851, 1853, 1856, 1857, 1859, 1865, 1866, 
                   1867, 1868, 1871, 1874, 1875, 3011, 3012, 3013, 3015, 3016, 3017, 3018, 
                   3032, 3037, 3038, 3039, 3040, 3041, 3042, 3043, 3044, 3045, 3046, 3050, 
                   3051, 3052, 3053, 3054, 3412, 3414, 3415, 3416, 3417, 3418, 3419, 3421, 
                   3422, 3423, 3424, 3425, 3426, 3427, 3428, 3429, 3430, 3431, 3432, 3433, 
                   3434, 3435, 3436, 3437, 3438, 3439, 3440, 3441, 3447, 3448, 3449, 3450, 
                   3451, 3452, 3453, 3454, 3812, 3815, 3816, 3818, 3819, 3820, 3821, 3822, 
                   3823, 3824, 3825, 4201, 4206, 4207, 4211, 4212, 4213, 4214, 4216, 4217, 
                   4218, 4219, 4220, 4221, 4222, 4224, 4226, 4227, 4228, 4611, 4612, 4615, 
                   4616, 4619, 4620, 4622, 4623, 4625, 4628, 4629, 4630, 4632, 4633, 4634, 
                   4635, 4636, 4637, 4638, 4639, 4641, 4642, 4643, 4644, 4645, 4646, 4648, 
                   4649, 4650, 4651, 5014, 5020, 5021, 5022, 5025, 5026, 5027, 5029, 5032, 
                   5033, 5034, 5036, 5041, 5042, 5043, 5044, 5045, 5046, 5047, 5049, 5052, 
                   5053, 5054, 5055, 5056, 5058, 5060, 5061, 5404, 5405, 5411, 5412, 5413, 
                   5414, 5415, 5416, 5417, 5418, 5419, 5420, 5422, 5423, 5424, 5425, 5426, 
                   5427, 5428, 5429, 5430, 5432, 5433, 5434, 5435, 5436, 5437, 5438, 5439, 
                   5440, 5441, 5442, 5443, 5444)
.largekommune <- c(301, 1101, 1103, 1106, 1108, 1119, 1120, 1121, 1122, 1124, 1127, 1130, 
                   1146, 1149, 1505, 1506, 1507, 1520, 1577, 1579, 1804, 1806, 1824, 1833, 
                   1860, 1870, 3001, 3002, 3003, 3004, 3005, 3006, 3007, 3014, 3019, 3020, 
                   3021, 3022, 3023, 3024, 3025, 3026, 3027, 3028, 3029, 3030, 3031, 3033, 
                   3034, 3035, 3036, 3047, 3048, 3049, 3401, 3403, 3405, 3407, 3411, 3413, 
                   3420, 3442, 3443, 3446, 3801, 3802, 3803, 3804, 3805, 3806, 3807, 3808, 
                   3811, 3813, 3814, 3817, 4202, 4203, 4204, 4205, 4215, 4223, 4225, 4601, 
                   4602, 4613, 4614, 4617, 4618, 4621, 4624, 4626, 4627, 4631, 4640, 4647, 
                   5001, 5006, 5007, 5028, 5031, 5035, 5037, 5038, 5057, 5059, 5401, 5402, 
                   5403, 5406, 5421)
