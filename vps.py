import pandas as pd

df = pd.read_csv(r'data/vps_churn_data.txt')
print (df)
df.to_json (r'data/vps_churn_data_py.json')

df = pd.read_csv(r'data/vps_test_data.txt')
print (df)
df.to_json (r'data/vps_test_data_py.json')
