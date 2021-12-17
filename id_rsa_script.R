library(encryptr)
files <- list.files(path = "./my_space/", full.names = T)
for (file in files) {
  encrypt_file(file)
  
}

decrypt_file("./my_space/shiny-survival.zip.encryptr.bin")

encrypt_file('./apriori.R')
