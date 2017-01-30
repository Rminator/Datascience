#Author: Nitin Gaonkar
#Assigment:2

# Create the dataframes for the files downloaded from the SSCC
# Below code will create the dataframes for all three files
# Below are the data files:
#Below Dataframes are created
#DFmail
#DFcustomer
#DFitem
#pd.read("/Users/ngaonkar/Desktop/PREDICT-420/ngg135item.csv")
#return(pd.read)


import pandas as pd
import sqlite3
import numpy as np
import pickle

def main():
    path="/Users/ngaonkar/Desktop/PREDICT-420/"
    mailFile="ngg135mail.csv"
    itemFile="ngg135item.csv"
    customerFile="ngg135customer.csv"
    col='acctno'
    db='xyz.db'
    
    #function call 
    DFmailMain,DFitemMain,DFcustMain=readfiles(path,mailFile,itemFile,customerFile)
    print DFmailMain.head()
    print DFitemMain.head()
    print DFcustMain.head()
    
    checkcustomer(DFmailMain,DFitemMain,DFcustMain,col)
    loaddatabase(DFcustMain,DFitemMain,DFmailMain,db,path)
    DFnewcustomer,DFtotalmail=step9(DFcustMain,DFmailMain)
    print DFnewcustomer.head()
    print DFtotalmail.head()
    DFfinal=merge(DFnewcustomer,DFtotalmail)
    print DFfinal.head()
    crosstab(DFfinal)
   # object = DFfinal
    file = open('/Users/ngaonkar/Desktop/PREDICT-420/DFFINAL.csv', 'w')
    pickle.dump(DFfinal, file)
    finalimport(DFfinal,path,db)
    
    #for looop for all the columns, assign columnname  to col. Pass col in the argument
    
    #print DFCustomerClean.head()

def readfiles(path,mailFile,itemFile,customerFile):
        DFmail=pd.read_csv(path+mailFile,na_values='\N')
        DFitem=pd.read_csv(path+itemFile,na_values='\N')
        DFcustomer=pd.read_csv(path+customerFile,na_values='\N')
        col='acctno'
        
        DFCustomerClean=dups(DFcustomer,col)
        
        return DFmail,DFitem,DFCustomerClean
            

def dups(dF,colName):
     dF.duplicated(colName)
     DFcustomer_clean=dF.drop_duplicates()
     return DFcustomer_clean
    

def checkcustomer(dF,dF2,dF3,colName):
    dF=pd.DataFrame(dF.acctno.unique())
    dF.coloumns=[colName]
    dF2=pd.DataFrame(dF2.acctno.unique())
    dF.coloumns=[colName]
    dF3=pd.DataFrame(dF3.acctno.unique())
    dF.coloumns=[colName]
    print dF.count()
    print dF2.count()
    print dF3.count()
    print dF.isin(dF2).count()
    print "Hence proved"
    
def loaddatabase(dF,dF2,dF3,db,path):
    conn  = sqlite3.connect(path + db)
    dF.to_sql("customer1",conn, if_exists="replace")
    dF2.to_sql("item1",conn, if_exists="replace")
    dF3.to_sql("mail1",conn,if_exists="replace")
    print "tables are loaded successfully"     
    print pd.read_sql_query("select count(*) from customer1;", conn)
    print pd.read_sql_query("select count(*) from item;", conn)
    print pd.read_sql_query("select count(*) from mail;", conn)
    

def step9(dF,dF2):
    DFnewcustomer=dF[['acctno','ytd_transactions_2009','ytd_sales_2009','zhomeent','zmobav']].copy()
    DFnewcustomer['zhomment01']=np.where(DFnewcustomer.zhomeent=='Y',1,0)
    DFnewcustomer['zmobav01']=np.where(DFnewcustomer.zmobav=='Y',1,0)
    dF2['totalemail']=dF2.sum(axis=1)
    return DFnewcustomer,dF2
    
def merge(dF,dF2):
    DFfinal=pd.merge(dF,dF2,how='inner',on=['acctno','acctno'])
    DFfinal['criteria']=np.where(DFfinal.totalemail >=5,1,0)
    final=DFfinal['criteria']==1
    DFfinal=DFfinal[final]
    DFfinal=DFfinal[['acctno','ytd_transactions_2009','ytd_sales_2009','zhomeent','zmobav','zhomment01','zmobav01']].copy()
    DFfinal.to_csv('/Users/ngaonkar/Desktop/PREDICT-420/final_customers.csv')
    return DFfinal

def crosstab(dF):
    print pd.crosstab(dF.zhomeent,dF.zhomment01, margins=True)
    print pd.crosstab(dF.zmobav,dF.zmobav01, margins=True)

def finalimport(dF,path,db):
    conn  = sqlite3.connect(path + db)
    dF.to_sql("FINAL_CUSTOMER", conn, if_exists="replace")
    print "All task completed"
    

            
if __name__ == '__main__':
	 main()


    
 



    