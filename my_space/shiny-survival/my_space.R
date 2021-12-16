library(encryptr)
genkeys()

write.csv(gp, "gp.csv")

encrypt_file("gp.csv")


decrypt_file("gp.csv.encryptr.bin", file_name = "gp2.csv")
