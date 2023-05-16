#####################################################################################################################

data_mean  = usos_wide.iloc[:,2:18].apply(np.mean)
data_mean_array = np.array(data_mean)

data_std  = usos_wide.iloc[:,2:18].apply(np.std)
data_std_array = np.array(data_std)

print(data_mean)

data_std

cut_off = data_std_array * 3

cut_off

lower, upper = data_mean_array - cut_off, data_mean_array + cut_off

type((upper))

for num in range(5,8):
    print(usos_wide.iloc[num,])
    
for col in range(3,7):
    for row in range(0,10):
        print (usos_wide.iloc[row,col])
        
for col in range(1,15):
    print(usos_wide.iloc[1,col])
    
usos_wide.iloc[0,:]

### Iterar por filas

for index, row in usos_wide.iloc[:,3:5].iterrows():
    print(index, row[0])
    
atipicos = [] 

for row in usos_wide.iloc[:,3:5].itertuples():
    if row[1] > 10:
        atipicos.append("atipico")
    else:
        atipicos.append("tipico")
        
usos_wide.iloc[:,3:5].columns



atipicos_dic = {}
temp_list1 = []
temp_list2 = []
for row in usos_wide.iloc[:,3:5].itertuples():
    for j in range(1,3):
        if row[j] > 10:
            atipicos_dic[row[0]] = "atipicos"  
        else:
            atipicos_dic[row[0]]  ="tipicos"
            
usos_wide.iloc[:,2:12]



atipicos_dic = {}
temp_list1 = []
temp_list2 = []
temp_list3 = []
temp_list4 = []
temp_list5 = []
temp_list6 = []
temp_list7 = []
temp_list8 = []
temp_list9 = []
temp_list10 = []

for row in usos_wide.iloc[:,2:12].itertuples():
    for j in range(1,11):
        if j == 1 and (row[j] < lower[0] or row[j] > upper[0]):
            temp_list1.append("atipicos")  
        if j == 1 and (row[j] > lower[0] or row[j] < upper[0]):
            temp_list1.append("tipicos")
        if j == 2 and (row[j] < lower[0] or row[j] > upper[1]):
            temp_list2.append("atipicos") 
        if j == 2 and (row[j] > lower[0] or row[j] < upper[1]):
            temp_list2.append("tipicos")
        if j == 3 and (row[j] < lower[0] or row[j] > upper[2]):
            temp_list3.append("atipicos") 
        if j == 3 and (row[j] > lower[0] or row[j] < upper[2]):
            temp_list3.append("tipicos")
        if j == 4 and (row[j] < lower[0] or row[j] > upper[3]):
            temp_list4.append("atipicos") 
        if j == 4 and (row[j] > lower[0] or row[j] < upper[3]):
            temp_list4.append("tipicos")
        if j == 5 and (row[j] < lower[0] or row[j] > upper[4]):
            temp_list5.append("atipicos") 
        if j == 5 and (row[j] > lower[0] or row[j] < upper[4]):
            temp_list5.append("tipicos")
        if j == 6 and (row[j] < lower[0] or row[j] > upper[5]):
            temp_list6.append("atipicos") 
        if j == 6 and (row[j] > lower[0] or row[j] < upper[5]):
            temp_list6.append("tipicos")
        if j == 7 and (row[j] > lower[0] or row[j] < upper[6]):
            temp_list7.append("atipicos") 
        if j == 7 and (row[j] < lower[0] or row[j] > upper[6]):
            temp_list7.append("tipicos")
        if j == 8 and (row[j] < lower[0] or row[j] > upper[7]):
            temp_list8.append("atipicos") 
        if j == 8 and (row[j] > lower[0] or row[j] < upper[7]):
            temp_list8.append("tipicos")
        if j == 9 and (row[j] < lower[0] or row[j] > upper[8]):
            temp_list9.append("atipicos") 
        if j == 9 and (row[j] > lower[0] or row[j] < upper[8]):
            temp_list9.append("tipicos")
        if j == 10 and (row[j] < lower[0] or row[j] > upper[9]):
            temp_list10.append("atipicos") 
        if j == 10 and ((row[j] > lower[0]) or (row[j] < upper[9])):
            temp_list10.append("tipicos")
            
            
            
temp_list = []
def comparacion(row, upper,lower, rango):
        for num in range(rango):
                if j == (num+1) and (row[num+1] < lower[num] or row[num+1] > upper[num]):
                        return(temp_list.append("atipicos"))  
                if j == (num+1) and (row[num+1] > lower[num] or row[num+1] < upper[num]):
                        return (temp_list.append("tipicos"))
                    
for row in usos_wide.iloc[:,2:12].itertuples():
    for j in range(1,11):
        lista = comparacion(row,upper,lower,10)
        
print(lista)

for row in usos_wide.iloc[:,2:12].itertuples():
    for j in range(1,11):
        lista = comparacion(upper,lower,j)
        
for c in columns:
    usos_wide['COM'])
    
# Iterar por columnas
atipicos = []
for c in columns:
    if usos_wide[c] < 10:
        atipicos.append("No es outliers")
    if usos_wide[c] > 10:
        atipicos.append["es outliers"]
        
# Iterar por columnas con itertuples.
usos_wide.iloc[:,3:8].itertuples()

[x for x in np.array(usos_wide.iloc[:,3]) if x > upper.any()]
