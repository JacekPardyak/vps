import requests

req = requests.request(method = "GET", url='http://localhost:8086')
req
req.response()

curl -G 'http://localhost:8086/query?pretty=true' --data-urlencode "db=mydb" --data-urlencode "q=SELECT \"value\" FROM \"cpu_load_short\" WHERE \"region\"='us-west'"

import os

cmd = "curl -G 'http://localhost:8086/query?pretty=true'"

returned_value = os.system(cmd)  # returns the exit code in unix
print('returned value:', returned_value)
