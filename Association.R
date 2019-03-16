library(arules)
library(arulesViz)
trans = read.transactions("transaction_dept_for_association.csv", 
                          format = "single", 
                          sep = ",", 
                          cols = c("VisitNumber", "DepartmentDescription"),rm.duplicates=FALSE)00
rule1 = apriori(trans, parameter=list(support=0.01, confidence=0.8));
inspect(head(sort(rule1, by="lift"),1));
rule2 = apriori(trans, parameter=list(support=0.05, confidence=0.8));
inspect(head(sort(rule2, by="lift"),1));
rule3 = apriori(trans, parameter=list(support=0.05, confidence=0.6));
inspect(head(sort(rule3, by="lift"),1));
rule4 = apriori(trans, parameter=list(support=0.01, confidence=0.7));
inspect(head(sort(rule4, by="lift"),1));
plot(head(sort(rule1, by="lift"),1), method="graph");
plot(head(sort(rule2, by="lift"),1), method="graph");
plot(head(sort(rule3, by="lift"),1), method="graph");
plot(head(sort(rule4, by="lift"),1), method="graph")