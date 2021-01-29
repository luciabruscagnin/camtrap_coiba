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
                       "inspect this, might be younger female with messed up left ear"                       
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

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/2a3aaba4-3789-481b-b3aa-ace7a57fa7c1"
                       , 20 ,
                       "YOD" , 
                       "Second arrival"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/2a3aaba4-3789-481b-b3aa-ace7a57fa7c1"
                       , 1 ,
                       "LAR" , 
                       "Check, first arrival"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9a05da25-8886-4124-bfe3-f076c8727f23"
                       , 621 ,
                       "SKL" , 
                       "NA"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/e2b46e04-5933-44a8-907e-e914a05655c9"
                       , 1 ,
                       "SKL" , 
                       "On back of CYS"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/a3918cc9-2c3c-4302-b52b-20206950ee02"
                       , 1 ,
                       "SKL" , 
                       "Sex with CYS"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9a05da25-8886-4124-bfe3-f076c8727f23"
                       , 393 ,
                       "FRI" , 
                       "On anvil"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/cd493ab7-86e4-4d0c-add1-4b89c01704d2"
                       , 1 ,
                       "CYS" , 
                       "in color, blurred"                       
)


#####Cebus-01-R2
d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1d371307-63ee-4038-ad5f-60a6348e49e5"
                       , 1 ,
                       "CYS" , 
                       "in color, blurred"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/a703b801-51c8-4c8f-9948-a5cb88dbc5f8"
                       , 1 ,
                       "SPT" , 
                       NA                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/a703b801-51c8-4c8f-9948-a5cb88dbc5f8"
                       , 41 ,
                       "INK" , 
                       NA                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/08a234de-d247-4786-a796-f009e56471a8"
                       , 1 ,
                       "INK" , 
                       NA                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/08a234de-d247-4786-a796-f009e56471a8"
                       , 1 ,
                       "CYS" , 
                       NA                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/08a234de-d247-4786-a796-f009e56471a8"
                       , 30 ,
                       "TOM" , 
                       NA                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/08a234de-d247-4786-a796-f009e56471a8"
                       , 30 ,
                       "BEA" , 
                       "check this"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/982c3760-80b9-42a5-a228-b90ddf6a8ea5"
                       , 1 ,
                       "TOM" , 
                       NA                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/982c3760-80b9-42a5-a228-b90ddf6a8ea5"
                       , 1 ,
                       "CYS" , 
                       NA                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/982c3760-80b9-42a5-a228-b90ddf6a8ea5"
                       , 16 ,
                       "LEO" , 
                       NA                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/26fac7be-9162-440b-af0e-aa8973320a63"
                       , 16 ,
                       "TOM" , 
                       "2nd opinion wanted"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/bc0fbc2a-fc80-4881-8b3e-51120f5a13b8"
                       , 1 ,
                       "LEO" , 
                       "2nd opinion wanted"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/4d281531-fb8d-4f9d-b52a-f85ed1e0441b"
                       , 1 ,
                       "SPT" , 
                       NA                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/d90f1640-b5d1-47e3-bb0e-5724a6b94957"
                       , 1 ,
                       "SPT" , 
                       NA                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/d90f1640-b5d1-47e3-bb0e-5724a6b94957"
                       , 1 ,
                       "YOD" , 
                       "must check, no spots, tiny ear tufts, slight peak, fluffy forehead"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/cae5bca8-b5e4-4744-af80-803eeab88eda"
                       , 1 ,
                       "OLG" , 
                       "good ID sequence"                       
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/40043c07-31d3-4db9-9941-039839a8a1b0"
                       , 1 ,
                       "OLG" , 
                       "best sequence, see cavities etc"                       
)


### CEBUS-09 R4 ####

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/da7d243a-a58e-4fbe-9aba-749c3fb813d7"
                       , 1 ,
                       "SPT" , 
                       NA                 
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/bb19c2a4-c9f5-4cda-9420-0a622f264ecd"
                       , 1 ,
                       "ABE" , 
                       "check, could also be SMG"                 
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/dddae03a-b3b2-46ff-aa90-ba2eb6d3f42d"
                       , 12 ,
                       "SMG" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/e3b04ec6-d287-4f8c-a41c-3cd680ef7ac7"
                       , 1 ,
                       "LEO" , 
                      "with infant and two juveniles"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/f5f8532b-4af1-4756-a163-c8139dad4098"
                       , 12 ,
                       "CYS" , 
                       "check, need second opinion"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/f5f8532b-4af1-4756-a163-c8139dad4098"
                       , 87 ,
                       "LEO" , 
                       "check, with infant and two juveniles"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/f5f8532b-4af1-4756-a163-c8139dad4098"
                       , 1 ,
                       "ABE" , 
                       "check or SMG"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/801664b8-63d8-4652-85b3-581ab0d4b284"
                       , 1 ,
                       "LEO" , 
                       "90% positive it's her"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/59acfede-b855-4674-93a6-70eb898c19dc"
                       , 1 ,
                       "LEO" , 
                       "with infant and juvenile"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/e3d3d5c9-2651-4ed7-814a-1b6f490666a9"
                       , 62 ,
                       "LEO" , 
                       "with infant and juveniles"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9e5ad2dc-e2f2-449b-bc2b-b39ea4448c47"
                       , 1 ,
                       "LAR" , 
                       "dot left ear"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/d04e1f81-a109-4583-9e03-faf32baaed19"
                       , 1 ,
                       "SPT" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/0ba9b991-b28f-4486-8e4d-843fd0bbca48"
                       , 11 ,
                       "ABE" , 
                       "lower canine sticking out (left side)"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/85934761-9219-4b72-89dd-ec80366011a7"
                       , 1 ,
                       "LEO" , 
                       "with infant and juveniles"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/007dd511-58f3-4bbb-a768-066126b3618a"
                       , 1 ,
                       "LAR" , 
                       "dot left ear"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/e439a915-68c8-423a-ad47-9aaedfdeafb5"
                       , 1 ,
                       "INK" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1a2f2213-e8f8-45fa-a240-921aa299f780"
                       , 1 ,
                       "SPT" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1a2f2213-e8f8-45fa-a240-921aa299f780"
                       , 28 ,
                       "LEO" , 
                       "not sure, check"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/39b74e36-97fc-4345-a479-3caf30f0791e"
                       , 1 ,
                       "SPT" , 
                       "double-check"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/0b56aad7-683a-4315-9e99-6d72c9b6a37e"
                       , 1 ,
                       "CYS" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9da4c001-b343-41ce-aa93-fa1118e4db05"
                       , 25 ,
                       "LEO" , 
                       "double-check, with infant and juvenile with white on side of legs"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1458e88d-a015-450c-b783-05b3d1ad997c"
                       , 9 ,
                       "LAR" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1458e88d-a015-450c-b783-05b3d1ad997c"
                        , 1 ,
                       "MRE" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/c1097711-4b1b-4ec6-b107-3ea30e419ff7"
                       , 11 ,
                       "SPT" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/90e4bac3-4324-45af-aa80-7bdd229f8027"
                       , 1 ,
                       "LAR" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/c2bbfe5e-a881-4028-940d-59d7d618dfa9"
                       , 1 ,
                       "LAR" , 
                       NA
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/fc07d53a-35cd-44ad-9fba-7017f0b3d33e"
                       , 1 ,
                       "LAR" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/feed2759-63d0-4d20-bafc-0a230c2340c7"
                       , 1 ,
                       "LAR" , 
                       "continuation of previous sequence"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/2c01125b-8fd9-499d-9259-9601b791fd3a"
                       , 1 ,
                       "LAR" , 
                       "continuation of previous two sequences"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/d84a8702-2d32-4861-9e87-583f10c001be"
                       , 1 ,
                       "LAR" , 
                       "continuation of previous three sequences"
)


d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/5532097b-213f-4526-9cdc-ad45e67547ea"
                       , 1 ,
                       "MRE" , 
                       "Check ID, I am basing this on a hunch and one picture of the back of his head. Also check behavior? (vigilance?)"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/435b7356-e9a2-409d-91f8-46168f361285"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/dc882d0a-743b-4a57-8357-3301cb3b2e69"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/b50580ec-7841-4088-b117-d9393e4b1ec2"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/4481463f-aa03-412e-a705-7f8f5727a688"
                       , 11 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/d7ae35df-e761-4562-9c3b-3a7eac6d65e2"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/e351ee62-8552-45f1-8c8f-a9d17fe7af7e"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1c343392-0bf0-4169-8a3d-3a2ea0c611b0"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/48569810-10f6-498d-bf8b-f4c418d1239c"
                       , 1 ,
                       "SMG" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/3745b28a-1ab9-43f2-86b7-d9a4e56247bf"
                       , 1 ,
                       "OLG" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/528b40d2-c6df-433c-8cc4-35fa664147c3"
                       , 1 ,
                       "OLG" , 
                       "Or Bea, check!"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/013278da-eba0-44ae-874e-d3d4a453732d"
                       , 15 ,
                       "OLG" , 
                       "Check, can also be BEA"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/e455af8b-68ec-46e2-a66c-0eae9822001b"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/c986ac8c-1f31-4e5a-99f0-16672263439f"
                       , 1 ,
                       "SPT" , 
                       "Check, I am 95% sure (I think it's the same as the sequence before where I was confident it was spot)"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/4af9f216-8094-479d-b7bf-5ef7e68b4dc5"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/f8f76514-e85a-4717-b251-5bd21e846058"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/09f47023-7320-4e67-acb5-ae8020821cf0"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/4aa078ee-1ec2-4e9b-86b1-fc2a86d5e7e9"
                       , 1 ,
                       "ABE" , 
                       "CHECK! could also be CYS"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/b1331256-0a01-45d0-bed1-98df3f29331b"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/e71296fd-1d5a-4ee9-94a6-89b72f843456"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/b4e9ed50-5bde-4f55-88f4-9d73c54d737e"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/929a000c-59b5-420b-a359-fb379cbc3b6f"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9899d57b-02f8-4533-8f48-b84ff8f2a8a4"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/76acd22f-e972-46d7-8de6-07e2e170dff4"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9c5f1e2e-9434-4b41-adba-463738eb6285"
                       , 1 ,
                       "ABE" , 
                       "Check, could be CYS"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/7ccc9c7b-1595-4882-94b1-c6e4fb0f53c5"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/ba44923e-9594-49dc-a6ac-8459a3ae8824"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/df145510-65ac-4d9e-af32-322fb17c8307"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/e2a8bf5a-8dea-45cf-9f03-f4b9020e22e3"
                       , 1 ,
                       "LAR" , 
                       "Check! if not larry then also change 3 sequences after it"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/99d4567d-55cd-4fd9-9332-417b783564d9"
                       , 1 ,
                       "LAR" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/61bc966e-77b4-46df-b85d-ef0231368bce"
                       , 1 ,
                       "LAR" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/721da3ed-36c0-47ab-88c7-a6fb3ded120d"
                       , 1 ,
                       "LAR" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/88c02561-44a3-4802-befc-707fa5e1d461"
                       , 1 ,
                       "TOM" , 
                       "I wasn't sure, Check, I didn't code it yet (only as comment)"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/97818bc7-bc41-4b7e-85f4-9029845a79e5"
                       , 1 ,
                       "OLG" , 
                       "Pregnant. 80% sure, could be BEA"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/103c6fc8-3bcd-4ceb-9112-b79264741de5"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/4db9eccb-4629-4ad4-bcd2-c6222a9553e0"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/841463b5-6396-4cfb-9bd7-53becb077455"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/cd6ed99a-b819-423f-92ec-5ac5d3a0d0c9"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/c5d632f7-6143-4b4d-bcf6-b81d6ffde301"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/60972c7b-f2a3-4903-80fe-a00172bc51bc"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9dc4040e-342f-4518-a661-1a0a94110eb4"
                       , 1 ,
                       "TOM" , 
                       "Check this"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/46c0ef8e-f7f6-4df7-a0ee-dd83c74433d0"
                       , 1 ,
                       "TOM" , 
                       "Check this"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/bfc47d9d-0a32-48a1-a32b-b7b95832f7ea"
                       , 12 ,
                       "SMG" , 
                       "Check"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/c18c527b-fb96-4c94-92c2-4df3b2eda4d7"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/3115d838-2fa7-4c45-b55c-ad38d4e910db"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/4d21147b-070c-4771-8c05-8580e3ea3f19"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/4f14ea17-aa50-4476-af9a-45fbee7a8c65"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9a6883a4-7992-4ea2-a362-2c63a846b6ac"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/ab4eb164-6ee6-4296-90e7-c27425ee63ce"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/ab4eb164-6ee6-4296-90e7-c27425ee63ce"
                       , 11 ,
                       "TOM" , 
                       "Double check ID"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/47afcf18-4509-456b-905a-f8518f714920"
                       , 1 ,
                       "SMG" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/c1815ea7-1d79-4b9a-9c5e-c8d1307c13f6"
                       , 1 ,
                       "SPT" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/ac24c862-c925-4caf-8051-e6f7e31ad129"
                       , 1 ,
                       "LAR" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/5806521b-0ab2-4cd7-906d-35ba764db19a"
                       , 1 ,
                       "ABE" , 
                       ""
)



#######CEBUS_R1-01
d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/cac333e5-bda2-43e4-b805-35a8ff458a19"

                       , 1 ,
                       "LAR" , 
                       "asymetrical peak, white spot left ear"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/fbc0046d-157d-408c-a3f7-47067ef3ab94"
                       , 1 ,
                       "LAR" , 
                       "asymetrical peak, white spot left ear"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/fcc5d069-bb53-450d-9d51-ab7a6c881d21",
                       1 ,
                       "LAR" , 
                       "asymetrical peak, white spot left ear"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/4a9bb448-b963-452d-a300-bd0462c6942b",
                       1 ,
                       "TOM" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/15df25cb-1a18-4254-81b8-d999d7eedec5",
                       1 ,
                       "ABE" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/15df25cb-1a18-4254-81b8-d999d7eedec5",
                       1 ,
                       "BEA" , 
                       "check this!"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/ebff7d74-1b38-42c3-8af5-7c7651fc2516",
                        1 ,
                       "TOM" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/4e12c26d-aae9-472a-ad2d-2d1aed140ff2",
                        1 ,
                       "SAD" , 
                       ""
)
d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/4e12c26d-aae9-472a-ad2d-2d1aed140ff2",
                        1 ,
                       "SMG" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/4e12c26d-aae9-472a-ad2d-2d1aed140ff2",
                        1 ,
                       "ABE" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/fe86a534-456c-4ef0-9d11-b5e0df87b327",
                       1 ,
                       "SMG" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/fe86a534-456c-4ef0-9d11-b5e0df87b327",
                       1 ,
                       "TOM" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/158dd0f4-ae01-422f-ac17-e56cdf442d42",
                       1 ,
                       "TOM" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/158dd0f4-ae01-422f-ac17-e56cdf442d42",
                       1 ,
                       "SAD" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/c4ae1497-5825-4720-a1cf-72467417f752",
                       1 ,
                       "SAD" , 
                       ""
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/eabca757-1dbc-4419-a357-838005cdf8db",
                       1 ,
                       "SAD" , 
                       ""
)


#### From here on down notes (not yet identified individuals)
## Double white dots, dark temples, larger male:
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/a6ea1cc5-9ab9-4f1a-9554-dfc876663d91
# Possibly also https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/47f607e6-9e90-4007-8edd-418976464d7c around frame 90
  
##  Mystery Juveniles
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/91502947-533d-4347-a05f-74bff21b5670 dip up in right peak 304
  
##  Symmetrical peak juvenile squared off
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/7069f94a-73f7-4190-8d28-d5c2a85ab3b0 check if yoda beginning on anvil. Symmetrical peak squares off (not sharp point)
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9a05da25-8886-4124-bfe3-f076c8727f23 symmetrical squared off peak at frame 38
  
##  Mousey infant/juvenile
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/7069f94a-73f7-4190-8d28-d5c2a85ab3b0 check frame 38 on left 
  
##  Shark pinched head juvenile
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1479a34c-f9d5-4697-b26d-58b9372c7510 frame 111 
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/9c5f1e2e-9434-4b41-adba-463738eb6285 frame 104
  
##  Broad faced unknown juvenile
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/bf099bea-40df-4934-8cda-0e1799dbf04e
  
##  Female messed up left ear (not Sadie? New female?)
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/3b0cbd5d-8174-4a87-8f54-d7fe15c10610 frame 24 
  
##  SHARP PEAK, JUV TINY WHITE DOT ABOVE LEFT EAR
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/e5ea21ed-3177-41d5-ad66-30b41e22e287

###huvenile, yoda like ear tufts piched head? 
#https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/08a234de-d247-4786-a796-f009e56471a8 frame 21

##no peak larger sub adult https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/bacd5730-65fb-43f4-89c3-31192ce931dd
#sharp peak juv https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/da171c17-fd20-4266-a996-d92cece793a2
  
#SAD of effed up ear female https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/ac7c0cc2-644c-4b26-a2b9-0e28dd52d5b0

#flufy forehead, some peak, maybe YODA? https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/c9a13818-bce5-4d33-8437-3ca86cbbedc5

#flufy forehead, some peak, maybe YODA again? https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/8204de2d-0869-4abb-b3da-3d709510384a

# female with left rumpled ear. Cavity lady or BEA? https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/cf30b8a5-d102-4469-b24f-23ce39dae56f

# juvenile with white on the legs (with Leona often) 
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/1ba13b87-21ff-4f06-b0c3-b49affaf4e2b
# CHECK https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/e34bd385-842a-4e73-87c8-b0ed635e7edc
# CHECK is same one as sequence directly above https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/d153d6f1-05e5-4643-9f24-337fa577f4f0 
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/20ef8c2d-b060-45d2-8638-8b598504daa4 starting frame 11

# juvenile not larry or spot
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/ae7cdc87-439e-4099-96ad-a178316a54c9

# male juvenile/subadult, with pigmentation in left ear (not INK?)
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/da8a3d6f-a40c-4046-bdaf-5ffe8f155f65
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/72c9af83-dec1-45ce-9f7d-1ecf0aef0df6