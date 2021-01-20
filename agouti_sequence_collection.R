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
                       "Double check"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/feed2759-63d0-4d20-bafc-0a230c2340c7"
                       , 1 ,
                       "LAR" , 
                       "Double check, continuation of previous sequence"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/2c01125b-8fd9-499d-9259-9601b791fd3a"
                       , 1 ,
                       "LAR" , 
                       "Double check, continuation of previous two sequences"
)

d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/d84a8702-2d32-4861-9e87-583f10c001be"
                       , 1 ,
                       "LAR" , 
                       "Double check, continuation of previous three sequences"
)

         

#######CEBUS_R1-01
d[nrow(d) + 1 , ] <- c("https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/cac333e5-bda2-43e4-b805-35a8ff458a19"

                       , 1 ,
                       "LAR" , 
                       "asymetrical peak, white spot left ear"
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
  
##  Broad faced unknown juvenile
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/bf099bea-40df-4934-8cda-0e1799dbf04e
  
##  Old cavity lady (not Beatrice?)
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/40043c07-31d3-4db9-9941-039839a8a1b0 
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/cae5bca8-b5e4-4744-af80-803eeab88eda 
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/013278da-eba0-44ae-874e-d3d4a453732d
# https://www.agouti.eu/#/project/0e0508af-d3dc-4be6-9806-dbdba5ec3368/annotate/sequence/528b40d2-c6df-433c-8cc4-35fa664147c3
  
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
