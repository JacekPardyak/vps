x = ['Python', 'programming', 'is', 'awesome!']
print(sorted(x))

print(sorted(x, key=lambda arg: arg.lower()))

#from pyspark.sql import SparkSession

#spark = SparkSession.builder.getOrCreate()
