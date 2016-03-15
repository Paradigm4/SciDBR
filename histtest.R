library(scidb)
scidbconnect()

ISLANDS_SCIDB = as.scidb(islands)
foo1 <- hist(islands, right = TRUE,breaks= c(  12.0,1709.6, 3407.2,  5104.8,  6802.4,  8500.0, 10197.6, 11895.2, 13592.8, 15290.4, 16988.0),col = "gray", labels = TRUE)
foo2 <- hist(ISLANDS_SCIDB, right=TRUE, breaks=10,materialize = TRUE,plot = TRUE, col = "gray", labels = TRUE)

foo1$breaks 
foo2$breaks

foo1$counts 
foo2$counts