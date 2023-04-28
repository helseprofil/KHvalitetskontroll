# Load list of all dimensions, and make hidden object
source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R") 
.ALL_DIMENSIONS <- ALL_DIMENSIONS
rm(ALL_DIMENSIONS)

# Set ggplot theme and color palette
ggplot2::theme_set(theme_bw())
ggplot2::theme_update(legend.position = "top", 
                      panel.grid.minor = element_blank(),
                      text = element_text(color = "black"),
                      plot.margin = margin(t = 1, b = 1, r = 1, unit = "cm"))


# Set global options
PROFILEYEAR <- 2023  # For saving in correct folder
DUMPS <- c("dfnew_flag", "dfold_flag", "compareKUBE") # Default is to create all file dumps

# Identify small/large kommune

# Run to update list with new BEFOLK_GK, current list generated from BEFOLK_GK_2023-01-04-10-25

#.SmallLargeKommune()

.allgeos <- c(0, 3, 11, 15, 18, 30, 34, 38, 42, 46, 50, 54, 
              301, 1101, 1103, 1106, 1108, 1111, 1112, 1114, 1119, 1120, 1121, 1122, 1124, 
              1127, 1130, 1133, 1134, 1135, 1144, 1145, 1146, 1149, 1151, 1160, 1505, 1506, 
              1507, 1511, 1514, 1515, 1516, 1517, 1520, 1525, 1528, 1531, 1532, 1535, 1539, 
              1547, 1554, 1557, 1560, 1563, 1566, 1573, 1576, 1577, 1578, 1579, 1804, 1806,
              1811, 1812, 1813, 1815, 1816, 1818, 1820, 1822, 1824, 1825, 1826, 1827, 1828, 
              1832, 1833, 1834, 1835, 1836, 1837, 1838, 1839, 1840, 1841, 1845, 1848, 1851, 
              1853, 1856, 1857, 1859, 1860, 1865, 1866, 1867, 1868, 1870, 1871, 1874, 1875, 
              3001, 3002, 3003, 3004, 3005, 3006, 3007, 3011, 3012, 3013, 3014, 3015, 3016, 
              3017, 3018, 3019, 3020, 3021, 3022, 3023, 3024, 3025, 3026, 3027, 3028, 3029, 
              3030, 3031, 3032, 3033, 3034, 3035, 3036, 3037, 3038, 3039, 3040, 3041, 3042, 
              3043, 3044, 3045, 3046, 3047, 3048, 3049, 3050, 3051, 3052, 3053, 3054, 3401, 
              3403, 3405, 3407, 3411, 3412, 3413, 3414, 3415, 3416, 3417, 3418, 3419, 3420, 
              3421, 3422, 3423, 3424, 3425, 3426, 3427, 3428, 3429, 3430, 3431, 3432, 3433, 
              3434, 3435, 3436, 3437, 3438, 3439, 3440, 3441, 3442, 3443, 3446, 3447, 3448, 
              3449, 3450, 3451, 3452, 3453, 3454, 3801, 3802, 3803, 3804, 3805, 3806, 3807, 
              3808, 3811, 3812, 3813, 3814, 3815, 3816, 3817, 3818, 3819, 3820, 3821, 3822, 
              3823, 3824, 3825, 4201, 4202, 4203, 4204, 4205, 4206, 4207, 4211, 4212, 4213, 
              4214, 4215, 4216, 4217, 4218, 4219, 4220, 4221, 4222, 4223, 4224, 4225, 4226, 
              4227, 4228, 4601, 4602, 4611, 4612, 4613, 4614, 4615, 4616, 4617, 4618, 4619, 
              4620, 4621, 4622, 4623, 4624, 4625, 4626, 4627, 4628, 4629, 4630, 4631, 4632, 
              4633, 4634, 4635, 4636, 4637, 4638, 4639, 4640, 4641, 4642, 4643, 4644, 4645, 
              4646, 4647, 4648, 4649, 4650, 4651, 5001, 5006, 5007, 5014, 5020, 5021, 5022, 
              5025, 5026, 5027, 5028, 5029, 5031, 5032, 5033, 5034, 5035, 5036, 5037, 5038, 
              5041, 5042, 5043, 5044, 5045, 5046, 5047, 5049, 5052, 5053, 5054, 5055, 5056, 
              5057, 5058, 5059, 5060, 5061, 5401, 5402, 5403, 5404, 5405, 5406, 5411, 5412, 
              5413, 5414, 5415, 5416, 5417, 5418, 5419, 5420, 5421, 5422, 5423, 5424, 5425, 
              5426, 5427, 5428, 5429, 5430, 5432, 5433, 5434, 5435, 5436, 5437, 5438, 5439, 
              5440, 5441, 5442, 5443, 5444, 
              30101, 30102, 30103, 30104, 30105, 30106, 30107, 30108, 30109, 30110, 30111, 
              30112, 30113, 30114, 30115, 110301, 110302, 110303, 110304, 110305, 110306, 
              110307, 110308, 110309, 460101, 460102, 460103, 460104, 460105, 460106, 460107, 
              460108, 500101, 500102, 500103, 500104)

.allweights <- c(5425270, 699827, 485797, 265848, 240190, 1269230, 371253, 424832, 311134, 
                 641292, 474131, 241736, 699827, 14860, 144699, 37444, 81305, 3281, 3178, 
                 2789, 19296, 20163, 19353, 12131, 27568, 11454, 13268, 2534, 3784, 4525, 
                 523, 855, 11283, 42541, 188, 8775, 24013, 32002, 67114, 3045, 2422, 8765, 
                 8557, 5126, 10833, 4467, 7558, 9547, 8597, 6936, 7019, 3518, 5828, 2669, 
                 2960, 6932, 5849, 2120, 3384, 10809, 2491, 13287, 52803, 21530, 1406, 1981, 
                 7777, 1175, 462, 1825, 7333, 2257, 13233, 1461, 1273, 1369, 1698, 4420, 
                 26092, 1869, 450, 1153, 6214, 1894, 1012, 4617, 9603, 1869, 2591, 1976, 
                 1334, 469, 678, 1216, 11566, 9724, 8107, 2565, 4458, 10468, 4572, 982, 
                 2708, 31444, 50290, 58182, 83892, 102273, 27879, 31011, 4741, 1315, 3578, 
                 45608, 3846, 8312, 7633, 5913, 18699, 61032, 20780, 16084, 19939, 128982, 
                 96088, 17754, 19024, 11249, 44693, 89095, 24947, 6989, 41565, 23898, 26716, 
                 15074, 2905, 6859, 1057, 3273, 4667, 2611, 4650, 4504, 3492, 2189, 14273, 
                 20044, 27584, 2720, 1370, 2455, 6908, 9144, 17949, 31999, 28425, 30267, 
                 35073, 7715, 21156, 5016, 7978, 6032, 4548, 7211, 3597, 21435, 6603, 4195, 
                 2318, 1722, 1253, 1551, 5581, 2445, 1530, 1855, 2498, 1986, 2151, 2211, 3591, 
                 5628, 5531, 3064, 4385, 5082, 6079, 14827, 13572, 13633, 5535, 6577, 2889, 
                 1256, 6354, 2111, 3252, 1587, 27502, 25681, 57794, 64943, 47777, 36624, 
                 55513, 13029, 27165, 2349, 14056, 10351, 4093, 6494, 10539, 5512, 1562, 
                 2889, 2452, 1414, 1198, 2140, 3755, 6735, 24017, 45509, 113737, 23147, 
                 9622, 9048, 2427, 2131, 6115, 6098, 11279, 5342, 1801, 1323, 3653, 1134, 
                 1169, 935, 15123, 912, 10480, 1704, 5883, 1810, 286930, 17131, 4043, 5775, 
                 12061, 18919, 3117, 2883, 13017, 10881, 937, 1051, 15875, 8497, 2501, 25213, 
                 5283, 39032, 29816, 3867, 378, 8131, 29593, 2889, 502, 1629, 2230, 768, 
                 1290, 3965, 2560, 12097, 1766, 2117, 5204, 5246, 2951, 2901, 22116, 3521, 
                 9527, 5875, 7207, 210496, 24004, 15001, 5265, 904, 7066, 2443, 5572, 1953, 
                 6120, 17123, 8360, 14425, 4090, 750, 2399, 24287, 2608, 20171, 14955, 2033, 
                 1309, 441, 818, 2287, 1193, 3817, 1101, 570, 6794, 9899, 5884, 5156, 10371, 
                 4252, 18502, 9732, 1980, 77544, 24804, 21144, 1897, 5568, 11274, 2789, 4201, 
                 1289, 1070, 970, 3993, 2087, 6599, 3414, 1068, 14738, 5576, 2179, 2729, 
                 1836, 2012, 2804, 4746, 1159, 2877, 859, 964, 1162, 2947, 3904, 2584, 1221, 
                 1057, 906, 2821, 854, 2165, 9925, 60209, 63891, 46424, 39066, 59026, 34896,
                 50784, 53109, 35117, 27457, 33259, 49373, 50837, 52595, 39037, 13197, 15571,
                 24683, 21817, 17767, 19945, 23082, 3235, 5221, 13802, 42505, 44148, 30479, 
                 40272, 30484, 42950, 41953, 52298, 59073, 55418, 43420)

.weightsdata <- data.table::data.table(GEO = .allgeos, WEIGHTS = .allweights)

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