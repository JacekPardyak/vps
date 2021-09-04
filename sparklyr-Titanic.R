library(sparklyr)
library(dplyr)
library(tidyr)
library(titanic)
library(ggplot2)
library(purrr)

# Connect to local spark cluster and load data
sc <- spark_connect(master = "local", version = "2.0.0")
#spark_read_parquet(sc, name = "titanic", path = "titanic-parquet")
data("titanic_train"); data("titanic_test")
titanic_train


titanic_train_tbl <- copy_to(sc, titanic_train, overwrite = TRUE)
titanic_tbl <- tbl(sc, "titanic_train")

titanic_test_tbl <- copy_to(sc, titanic_test, overwrite = TRUE)
titanic_test_tbl <- tbl(sc, "titanic_test")

# Transform features with Spark SQL API
titanic2_tbl <- titanic_tbl %>% 
  mutate(Family_Size = SibSp + Parch + 1L) %>% 
  mutate(Pclass = as.character(Pclass)) %>%
  filter(!is.na(Embarked)) %>%
  mutate(Age = if_else(is.na(Age), mean(Age), Age)) %>%
  sdf_register("titanic2")

# Transform family size with Spark ML API
titanic_final_tbl <- titanic2_tbl %>%
  mutate(Family_Size = as.numeric(Family_size)) %>%
  sdf_mutate(
    Family_Sizes = ft_bucketizer(Family_Size, splits = c(1,2,5,12))
  ) %>%
  mutate(Family_Sizes = as.character(as.integer(Family_Sizes))) %>%
  sdf_register("titanic_final")
sdf_










data(beavers, package = "datasets")
beaver_tbl <- copy_to(sc, beaver1, "beaver")
beaver_tbl %>%
  mutate(squared = temp ^ 2) %>%
  ft_binarizer('squared', 'squared_bin', 1000) %>%
  sdf_register("mutated")

# view our newly constructed tbl
mutated_tbl <- tbl(sc, "mutated")
head(mutated_tbl)

# note that we have two separate tbls registered
dplyr::src_tbls(sc)
# }

?ft_binarizer

sc <- spark_connect(master = "local")
iris_tbl <- sdf_copy_to(sc, iris, name = "iris_tbl", overwrite = TRUE)

iris_tbl %>%
  ft_binarizer(
    input_col = "Sepal_Length",
    output_col = "Sepal_Length_bin",
    threshold = 5
  ) %>%
  select(Sepal_Length, Sepal_Length_bin, Species)

