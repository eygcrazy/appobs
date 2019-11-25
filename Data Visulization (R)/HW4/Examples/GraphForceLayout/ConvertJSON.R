
require(rjson)


json_data = fromJSON(file="GraphData.json")


data = data.frame(a=rnorm(100), b=rnorm(100))

dataJSON = toJSON(data)
