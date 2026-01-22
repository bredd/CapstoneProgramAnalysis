source("LoadData.r")
source("AnalysisFunctions.r")

q <- CountColumn(data, "state");

cat(length(q), "states\n");
cat(sum(q), "programs\n");
print(q);

print(CountColumn(data, "category"));