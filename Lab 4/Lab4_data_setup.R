babies = read.csv("http://kbodwin.web.unc.edu/files/2017/11/babynames_mini-1.csv", header = TRUE)
dim(babies)

babies_sub <- babies %>% filter(Name %in% c("Kelly", "Jim", "Greg", "Diane"))

write.csv(babies_sub, "/Users/kellybodwin/Dropbox/Teaching/ShinyLabs/Datasets/babies_sub.csv", row.names = FALSE)
