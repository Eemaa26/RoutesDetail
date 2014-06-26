## Open a connection
require(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="dwhlap"
                 ,host='127.0.0.1'
                 ,port='5433'
                 ,user='biadmin'
                 ,pass='ep8C61mUqAy3zVs')

QueryNamesRoutes <- paste("SELECT distinct rout.sk_customer, name
    FROM fact_routesqsrt rout 
    inner join dim_customer cust ON cust.sk_customer = rout.sk_customer")


rs1 <- dbSendQuery(con, QueryNamesRoutes )

df1 <- fetch(rs1,n=100)




QueryFactRoutes <- paste("SELECT 
cust.name,
routes.sk_machineid, 
routes.route,
machine.sregno,
pointstart, 
pointend, 
durationstart ,
displacement,
ftripdistance, 
lidrivingseconds,
listandingtime,
flitres,
performance,
ispeedoccurs,
ucmaxspeed,
lispeedtime,
irpmoccurs,
limaxrpm ,
lirpmtime ,
ibrakeoccurs, 
ucmaxbrake,
iacceloccurs,
ucmaxaccel,
iexidleoccurs, 
liexidletime, 
ligbtime ,
engineseconds, 
durationend ,
lispeedtimeper,
peridle ,
turbo ,
torque ,
per_fan ,
man_fan
FROM public.fact_routesqsrt routes
INNER JOIN public.dim_machine machine ON routes.sk_machineid = machine.sk_machineid
INNER JOIN public.dim_organizations org ON routes.sk_orgid = org.sk_orgid
INNER JOIN public.dim_customer cust ON routes.sk_customer = cust.sk_customer
INNER JOIN public.dim_date dates ON routes.sk_pointstartstdate = dates.sk_date
INNER JOIN public.dim_routes routesName ON routes.sk_routes = routesName.sk_routes
    WHERE  cust.sk_customer = 0                   ")

rs <- dbSendQuery(con, QueryFactRoutes )

df <- fetch(rs,n=-1)

str(df)

unique(df$pointstart)





