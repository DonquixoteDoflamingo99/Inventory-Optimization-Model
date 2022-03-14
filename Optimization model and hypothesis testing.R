#PART 1

AD	= 15000

UC=80

CCR=0.18

OC=220

HC=UC*CCR


OQ_2Q = sqrt(((2*AD*OC)/HC))
OQ_2Q

ILP_Q= OQ_2Q/2
ILP_Q

N = AD/OQ_2Q
N

Annual_OC = OC*N
Annual_OC

Annual_HC = ILP_Q*UC*CCR
Annual_HC

Total_Cost = Annual_OC + Annual_HC
Total_Cost

Inventory_level_Q=seq(100, 1000, by = 20)
Order_Quantity=2*Inventory_level_Q
Total_Cost_C_Q = (OC*(AD/Order_Quantity))+(Inventory_level_Q *UC*(CCR))

df <-
  data.frame(
    Inventory_level_Q,
    Order_Quantity,
    Total_Cost_C_Q
  )

observed_value <-
  df$Total_Cost_C_Q == min(df$Total_Cost_C_Q)

observed_Min_Quantity <- df[observed_value,]

observed_Min_Quantity

summary(df)

plot(Total_Cost, OQ_2Q)

plot(df$Order_Quantity,df$Total_Cost_C_Q, 'l', main = "Total Cost vs Order Quantity")


#PART 2

min=13000
max=17000
mode=15000

sim=runif(1000)

A =  min+sqrt((max-min)*(mode-min)*sim)

B = max-sqrt((max-min)*(max-mode)*(1-sim))

C = (mode-min)/(max-min)

x = ifelse(sim < C, A, B)

summary(x)


AD2=x
HC2=HC
OC2=OC
Annual_HC2=Annual_HC

Total_Cost_2=  ((AD2 * OC2) / HC2) + Annual_HC2
t.test(Total_Cost_2)
summary(Total_Cost_2)
df2=data.frame(AD2, Total_Cost_2)
summary(df2)
plot(df2)



Exp_OQ=sqrt((2 * OC2 * AD2) / HC2)
t.test(Exp_OQ)
summary(Exp_OQ)
df3=data.frame(AD2, Exp_OQ)
summary(df3)


Exp_AO <- AD2 / Exp_OQ
t.test(Exp_AO)
summary(Exp_AO)
df4 <- data.frame(AD2, Exp_AO)
summary(df4)

