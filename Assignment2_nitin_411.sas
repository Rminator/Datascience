libname mydata "/sscc/home/n/ngg135/assignment2/" access=readonly;

proc datasets library=mydata; 
run;
 quit;
 

data training;
set mydata.logit_insurance;
proc contents data=training;
run;

*///Exploratory data analysis///;

proc print data=training (obs=10);
run;

proc corr data=training;
with target_FLAG;
run;

proc means data=training ; 
run;

proc means data=training NMISS N; 
run;

proc corr data=traning;
with target_FLAG;
run;

proc univariate data=training;
histogram INCOME /normal;
run;

proc univariate data=training;
histogram MVR_PTS /normal;
run;

proc univariate data=training;
histogram HOMEKIDS /normal;
run;


proc univariate data=training;
histogram OLDCLAIM /normal;
run;

proc univariate data=training;
histogram car_type /normal;
run;



proc univariate data=training;
histogram HOME_VAL /normal;
run;



*///IMPUTATION////*;

data imp_training;
   set training;

    IMP_AGE = AGE;
    I_IMP_AGE = 0;
    if missing(IMP_AGE) then do;
        IMP_AGE = 44.7903127;
        I_IMP_AGE = 1;
    end;

    IMP_CAR_AGE = CAR_AGE;
    I_IMP_CAR_AGE = 0;
    if missing(IMP_CAR_AGE) then do;
        IMP_CAR_AGE = 8.3283231;
        I_IMP_CAR_AGE = 1;
    end;

    IMP_HOME_VAL = HOME_VAL;
    I_IMP_HOME_VAL = 0;
    if missing(IMP_HOME_VAL) then do;
        IMP_HOME_VAL = 154867.29;
        I_IMP_HOME_VAL = 1;
    end;

    IMP_INCOME = INCOME;
    I_IMP_INCOME = 0;
    if missing(IMP_INCOME) then do;
        IMP_INCOME = 61898.10;
        I_IMP_INCOME = 1;
    end;
    log_IMP_INCOME = log(IMP_INCOME);
    t_IMP_INCOME = IMP_INCOME / 10000;

    IMP_YOJ = YOJ;
    I_IMP_YOJ = 0;
    if missing(IMP_YOJ) then do;
        IMP_YOJ = 10.4992864;
        I_IMP_YOJ = 1;
    end;

    if IMP_HOME_VAL = 0 then I_HOMEOWN = 0;
        else I_HOMEOWN = 1;
        
proc means data=imp_training NMISS N; 
run;



proc univariate data=IMP_TRAINING;
histogram IMP_HOME_VAL /normal;
run;

proc univariate data=IMP_TRAINING;
histogram IMP_CAR_AGE /normal;
run;



proc corr data=imp_training;
with TARGET_FLAG;
  var TARGET_FLAG IMP_AGE BLUEBOOK IMP_CAR_AGE CLM_FREQ HOMEKIDS IMP_HOME_VAL IMP_INCOME MVR_PTS OLDCLAIM TIF TRAVTIME IMP_YOJ;


*///catergorical variables////*;

/* Categorical */

proc freq data=imp_training;
    tables CAR_TYPE CAR_USE EDUCATION JOB KIDSDRIV MSTATUS PARENT1 RED_CAR REVOKED SEX URBANICITY;

proc freq data=imp_training;    
table TARGET_FLAG*CAR_TYPE;
proc freq data=imp_training;
    table TARGET_FLAG*CAR_USE;
proc freq data=imp_training;
    table TARGET_FLAG*CLM_FREQ;
proc freq data=imp_training;
    table TARGET_FLAG*EDUCATION;
proc freq data=imp_training;
   table TARGET_FLAG*HOMEKIDS;
proc freq data=imp_training;
    table TARGET_FLAG*JOB;
proc freq data=imp_training;
    table TARGET_FLAG*KIDSDRIV;
proc freq data=imp_training;
    table TARGET_FLAG*MSTATUS;
proc freq data=imp_training;
    table TARGET_FLAG*MVR_PTS;
proc freq data=imp_training;
    table TARGET_FLAG*PARENT1;
proc freq data=imp_training;
    table TARGET_FLAG*RED_CAR;
proc freq data=imp_training;
    table TARGET_FLAG*REVOKED;
proc freq data=imp_training;
    table TARGET_FLAG*SEX;
proc freq data=imp_training;
    table TARGET_FLAG*URBANICITY;

data imp_ref_training;
    set imp_training;

    * Variable of reference: Panel Truck;
    if CAR_TYPE in ('Minivan' 'Panel Truck' 'Pickup' 'Sports Car' 'Van' 'z_SUV') then do;
        TYPE_MINI = (CAR_TYPE eq 'Minivan');
        TYPE_PICK = (CAR_TYPE eq 'Pickup');
        TYPE_SPOR = (CAR_TYPE eq 'Sports Car');
        TYPE_VAN = (CAR_TYPE eq 'Van');
        TYPE_SUV = (CAR_TYPE eq 'z_SUV');
    end;

    * Variable of reference: Commercial;
    if CAR_USE in ('Commercial' 'Private') then do;
        USE_P = (car_use eq 'Private');
    end;

    * Variable of reference: PhD;
    if EDUCATION in ('<High School' 'Bachelors' 'Masters' 'PhD' 'z_High School') then do;
        EDU_HS = (EDUCATION eq '<High School');
        EDU_BA = (EDUCATION eq 'Bachelors');
        EDU_MA = (EDUCATION eq 'Masters');
        EDU_ZHS = (EDUCATION eq 'z_High School');
    end;

    * Variable of reference: Doctor;
    if JOB in ('Clerical' 'Home Maker' 'Lawyer' 'Manager' 'Professional' 'Student' 'z_Blue Collar') then do;
        JOB_C = (JOB eq 'Clerical');
        JOB_HM = (JOB eq 'Home Maker');
        JOB_L = (JOB eq 'Lawyer');
        JOB_M = (JOB eq 'Manager');
        JOB_P = (JOB eq 'Professional');
        JOB_S = (JOB eq 'Student');
        JOB_BC = (JOB eq 'z_Blue Collar');
    end;

    if MSTATUS in ('Yes' 'z_No') then do;
        MARRIED_Y = (MSTATUS eq 'Yes');
    end;

    if PARENT1 in ('No' 'Yes') then do;
        PARTENT_S = (PARENT1 eq 'YES');
    end;

    if RED_CAR in ('no' 'yes') then do;
        RED_C = (RED_CAR eq 'yes');
    end;

    if REVOKED in ('No' 'Yes') then do;
        REV_L = (REVOKED eq 'Yes');
    end;

    * Variable of reference: Male;
    if SEX in ('M' 'z_F') then do;
        SEX_F = (SEX eq 'z_F');
  end;


proc print data=imp_ref_training (obs=100);
run;

        
proc means data=imp_ref_training NMISS N; 
run;





*///MODELS////*;


proc logistic data=imp_ref_training descending plots(only)=roc(id=prob);
    model TARGET_FLAG = IMP_AGE BLUEBOOK IMP_CAR_AGE CLM_FREQ HOMEKIDS IMP_INCOME MVR_PTS OLDCLAIM TRAVTIME
                        I_HOMEOWN
                        TYPE_MINI TYPE_PICK TYPE_SPOR TYPE_VAN TYPE_SUV USE_P EDU_HS EDU_BA EDU_MA EDU_ZHS
                        JOB_C JOB_HM JOB_L JOB_M JOB_P JOB_S JOB_BC MARRIED_Y PARTENT_S RED_C REV_L SEX_F /
                        selection=score outroc=roc_model rsq lackfit;
    output out=model_data pred=yhat_model;
    
    
    
*///MODELS-1 ////*;

proc logistic data=imp_ref_training DESCENDING PLOTS=EFFECT PLOTS=ROC;
    model TARGET_FLAG = CLM_FREQ IMP_INCOME MVR_PTS USE_P REV_L / rsq lackfit;
    output out=model_5_data pred=model_5_yhat;
    
    

proc logistic data=imp_ref_training DESCENDING PLOTS=EFFECT PLOTS=ROC;
    model TARGET_FLAG =CLM_FREQ IMP_INCOME MVR_PTS USE_P MARRIED_Y REV_L / rsq lackfit;
    output out=model_6_data pred=model_6_yhat;
    

proc logistic data=imp_ref_training DESCENDING PLOTS=EFFECT PLOTS=ROC;
    model TARGET_FLAG =CLM_FREQ HOMEKIDS IMP_INCOME MVR_PTS TYPE_MINI USE_P MARRIED_Y REV_L / rsq lackfit;
    output out=model_7_data pred=model_7_yhat;


proc logistic data=imp_ref_training DESCENDING PLOTS=EFFECT PLOTS=ROC;
    model TARGET_FLAG =CLM_FREQ HOMEKIDS IMP_INCOME MVR_PTS TYPE_MINI USE_P MARRIED_Y REV_L / rsq lackfit;
    output out=model_8_data pred=model_8_yhat;









/* Modeling TARGET_AMT */


proc reg data=imp_ref_training;
    model TARGET_AMT = BLUEBOOK IMP_HOME_VAL OLDCLAIM CAR_AGE CLM_FREQ HOMEKIDS IMP_INCOME MVR_PTS MARRIED_Y/
    selection=adjrsq aic bic cp best=5;

proc reg data=imp_ref_training;
    model TARGET_AMT = BLUEBOOK IMP_CAR_AGE CLM_FREQ OLDCLAIM IMP_INCOME;
  run;





*///DEPLOYMENT///*;

libname mydata "/sscc/home/n/ngg135/assignment2/" access=readonly;

proc datasets library=mydata; 
run;
 quit;
 

data testing;
set mydata.logit_insurance_test;
proc contents data=testing;
run;


data testing_new;
    set testing;
    IMP_CAR_AGE = CAR_AGE;
    I_IMP_CAR_AGE = 0;
    if missing(IMP_CAR_AGE) then do;
        IMP_CAR_AGE = 8.3283231;
        I_IMP_CAR_AGE = 1;
    end;
    
  
    IMP_INCOME = INCOME;
    I_IMP_INCOME = 0;
    if missing(IMP_INCOME) then do;
        IMP_INCOME = 61898.10;
        I_IMP_INCOME = 1;
    end;

    if CAR_USE in ('Commercial' 'Private') then do;
        USE_P = (car_use eq 'Private');
    end;
        if CAR_TYPE in ('Minivan' 'Panel Truck' 'Pickup' 'Sports Car' 'Van' 'z_SUV') then do;
        TYPE_MINI = (CAR_TYPE eq 'Minivan');
        TYPE_PICK = (CAR_TYPE eq 'Pickup');
        TYPE_SPOR = (CAR_TYPE eq 'Sports Car');
        TYPE_VAN = (CAR_TYPE eq 'Van');
        TYPE_SUV = (CAR_TYPE eq 'z_SUV');
    end;
    if MSTATUS in ('Yes' 'z_No') then do;
        MARRIED_Y = (MSTATUS eq 'Yes');
    end;
    if REVOKED in ('No' 'Yes') then do;
        REV_L = (REVOKED eq 'Yes');
    end;

data testing_score;
    set testing_new;

    Target_flag = -0.5089 + 0.2620 * CLM_FREQ + 0.1813 * HOMEKIDS - 0.00000764 * IMP_INCOME + 0.1395 * MVR_PTS - 0.6053 * TYPE_MINI- 0.5915 * USE_P - 0.6336 * MARRIED_Y + REV_L *0.8521 ;
/*  pi = exp(Target_flag) / (1+exp(Target_flag)); */
/*  P_TARGET_FLAG = (pi gt 0.50); */
    P_TARGET_FLAG = exp(Target_flag) / (1+exp(Target_flag));
    P_TARGET_AMT = 1483.329 + 0.01480 * BLUEBOOK – 37.24954 * IMP_CAR_AGE + 433.80 * CLM_FREQ - IMP_INCOME * 0.00457;
    keep index P_TARGET_FLAG P_TARGET_AMT;
    
    
 data testing_score;
    set testing_fixed;

    wat = 0.2631 * CLM_FREQ - 0.00000856 * IMP_INCOME + 0.1449 * MVR_PTS - 0.6733 * USE_P - 0.5995 * MARRIED_Y + 0.8764 * REV_L - 0.4333;
/*  pi = exp(wat) / (1+exp(wat)); */
/*  P_TARGET_FLAG = (pi gt 0.50); */
   P_TARGET_FLAG = exp(wat) / (1+exp(wat));
    P_TARGET_AMT = 1324.64489 + 216.66500 * HOMEKIDS - 41.75664 * IMP_CAR_AGE + 464.81029 * CLM_FREQ;
    keep index P_TARGET_FLAG P_TARGET_AMT;

proc print data=testing_score;

proc print data=testing_score  (obs=10);
run;

proc export data=testing_score
    outfile='/sscc/home/a/agd808/sasuser.v94/411/2/out.csv'
    dbms=csv
    replace;

run;


libname mydata "/sscc/home/n/ngg135/assignment2/" access=readonly;

proc datasets library=mydata; 
run;
 quit;



data random;
set mydata.logit_insurance_test_random;
proc contents data=random;
run;


proc print data=random (obs=10);
run;



data testing_score;
    set testing_new;
       P_TARGET_AMT = 1324.64489 + 216.66500 * HOMEKIDS - 41.75664 * IMP_CAR_AGE + 464.81029 * CLM_FREQ;
    keep index P_TARGET_FLAG P_TARGET_AMT;
    keep index P_TARGET_FLAG P_TARGET_AMT;


proc print data=testing_score;

proc print data=testing_new (obs=10);
run;




