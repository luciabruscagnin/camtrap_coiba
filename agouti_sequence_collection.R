# d <- data.frame(matrix(ncol = 4, nrow = 0))
# x <- c("agouti_sequence", "image_id", "individual" , "notes")
# colnames(d) <- x
# d
# d[norw(d+1),] <- c(https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1479a34c-f9d5-4697-b26d-58b9372c7510,
#                    ,
#                    ,
#                    )
# agouti <- function(sequence=0 , image_id=0 , individual=0 , notes=0){
#      
# }
# 
# d <- data.frame(matrix(ncol = 2, nrow = 0))
# x <- c("agouti_sequence", "image_id", "individual" , "notes")
# colnames(d) <- x
# 
# agouti <-
#      function(...) {
#           
#           url <- as.character(readline(prompt="Paste the Agouti Url Here: "))
#           seq <- as.numeric(readline(prompt="What is the sequence number?: "))
#           #id <- as.character(readline(prompt="Who is the individual?"))
#           #com <- as.character(readline(prompt="Additional comments"))
#           print( cbind(url,seq) )
#      }
#           
#    
# agouti()       

d <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("agouti_sequence", "image_id", "individual" , "notes")
colnames(d) <- x

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9a05da25-8886-4124-bfe3-f076c8727f23"
                      , 195 ,
                      "ABE" , 
                      NA
                      )

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1479a34c-f9d5-4697-b26d-58b9372c7510"
                       , 124 ,
                       "ABE" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/496dd7b9-9121-4c29-b3af-c79f4d1b2668"
                       , 8 ,
                       "ABE" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9a05da25-8886-4124-bfe3-f076c8727f23"
                       , 369 ,
                       "SMG" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9a05da25-8886-4124-bfe3-f076c8727f23"
                       , 369 ,
                       "SMG" , 
                       NA
)

     
d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1479a34c-f9d5-4697-b26d-58b9372c7510"
                       , 259 ,
                       "SMG" , 
                       NA
)     

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9a05da25-8886-4124-bfe3-f076c8727f23"
                       , 190 ,
                       "TOM" , 
                       "standing behind juvenile"
) 

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1479a34c-f9d5-4697-b26d-58b9372c7510"
                       , 124 ,
                       "TOM" , 
                       NA
) 

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/40043c07-31d3-4db9-9941-039839a8a1b0"
                       , 22 ,
                       "TOM" , 
                       NA
) 

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9a05da25-8886-4124-bfe3-f076c8727f23"
                       , 558 ,
                       "CYS" , 
                       NA
)    

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/e2b46e04-5933-44a8-907e-e914a05655c9"
                       , 558 ,
                       "CYS" , 
                       NA
)    

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/a3918cc9-2c3c-4302-b52b-20206950ee02"
                       , 1 ,
                       "CYS" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/e657fcd0-9141-40cf-9270-7c4e73fc20ec"
                       , 1 ,
                       "MRE" , 
                       NA
)   

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9a05da25-8886-4124-bfe3-f076c8727f23"
                       , 193 ,
                       "BEA" , 
                       NA
)   

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1479a34c-f9d5-4697-b26d-58b9372c7510"
                       , 124 ,
                       "BEA" , 
                       NA
)   

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/f256a3fa-7f73-455a-b2ee-3a288a4e8ee4"
                       , 1 ,
                       "BEA" , 
                       NA
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/f256a3fa-7f73-455a-b2ee-3a288a4e8ee4"
                       , 1 ,
                       "BEA" , 
                       NA
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/91502947-533d-4347-a05f-74bff21b5670"
                       , 140 ,
                       "LEO" , 
                       "with infant and juvenile"
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/982c3760-80b9-42a5-a228-b90ddf6a8ea5"
                       , 17 ,
                       "LEO" , 
                        NA                       
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9a05da25-8886-4124-bfe3-f076c8727f23"
                       , 52 ,
                       "LEO" , 
                       NA                       
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/5583d3eb-49c1-467d-b738-cabb60064fea"
                       , 52 ,
                       "LEO" , 
                       NA                       
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9a05da25-8886-4124-bfe3-f076c8727f23"
                       , 161 ,
                       "DOT" , 
                       "snoutier than LEO"                       
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1479a34c-f9d5-4697-b26d-58b9372c7510"
                       , 80 ,
                       "DOT" , 
                       "snoutier than LEO"                       
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1479a34c-f9d5-4697-b26d-58b9372c7510"
                       , 178 ,
                       "SAD" , 
                       NA                       
) 

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/a3e11e2f-b2a6-425b-9b54-3c38fab8d930"
                       , 1 ,
                       "SAD" , 
                       NA                       
) 

###########NEED INSPECTION
d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/4c037780-57e1-41cf-b82e-aa866ad67eb8"
                       , 33 ,
                       "LEO" , 
                       "inspect this"                       
)  


d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/5583d3eb-49c1-467d-b738-cabb60064fea"
                       , 1 ,
                       "LEO" , 
                       "NA"                       
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9a05da25-8886-4124-bfe3-f076c8727f2"
                       , 161 ,
                       "DOT" , 
                       "Snoutier than Leona"                       
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1479a34c-f9d5-4697-b26d-58b9372c7510"
                       , 80 ,
                       "DOT" , 
                       "NA"                       
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/a3e11e2f-b2a6-425b-9b54-3c38fab8d930"
                       , 178 ,
                       "SAD" , 
                       "NA"                       
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/a3e11e2f-b2a6-425b-9b54-3c38fab8d930"
                       , 576 ,
                       "SAD" , 
                       "NA"                       
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/7af13f2e-db6f-493a-aff4-d61cb6e1eb81"
                       , 24 ,
                       "SAD" , 
                       "NA"                       
)  

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/a3e11e2f-b2a6-425b-9b54-3c38fab8d930"
                       , 1 ,
                       "SAD" , 
                       "NA"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/843c7c4a-9546-4b68-9288-9e31bff6de2c"
                       , 1 ,
                       "SPT" , 
                       "NA"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/7069f94a-73f7-4190-8d28-d5c2a85ab3b0"
                       , 21 ,
                       "SPT" , 
                       "NA"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1479a34c-f9d5-4697-b26d-58b9372c7510"
                       , 480 ,
                       "SPT" , 
                       "NA"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/f1c36f20-9aa0-41ee-bfeb-1ce7e421f54a"
                       , 1 ,
                       "SPT" , 
                       "NA"                       
)