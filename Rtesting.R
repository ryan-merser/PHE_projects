library(RODBC)

myconn <- odbcDriverConnect("Driver={SQL Server};Server=DHBDB-SQL-07\\SQLSANDBOX2017;Database=Adameve_Storefront;;trusted_connection=true")

products <- sqlQuery(myconn, "select top 10 * from product")


#print(products)

summary(products)
