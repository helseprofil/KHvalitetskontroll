# When only 1 year available, plot to check whether data on bydel is distributed around kommune

d <- dfnew[GEO %in% c(301, 1103, 4601, 5001) | GEO > 9999]
d[, kommune := character()]
d[, kommune := fcase(grepl("^301", GEO), "Oslo",
                     grepl("^4601", GEO), "Bergen",
                     grepl("^1103", GEO), "Stavanger",
                     grepl("^5001", GEO), "Trondheim")]
d[, geoniv := fcase(GEO < 9999, "Kommune",
                    default = "Bydel")]


komm <- unique(d$kommune)

for(i in 1:4){
print(ggplot(d[kommune == komm[i]], aes(x = AAR, y = MEIS, color = geoniv, size = geoniv)) + 
  geom_point()+ 
  facet_wrap(facets = vars(TRINN, FERDNIVAA), ncol = 8, labeller = labeller(.multi_line = F)) + 
  labs(subtitle = komm[i]))  
}
