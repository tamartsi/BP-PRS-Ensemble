#######################################################################################################
# Function to load the data
#######################################################################################################

def load_data(var_pheno, one_hot_encode = True, exclude_meds =False, pc_include = False,
             unrelated_pheno_file = '/2022_BP_ensemble/TOPMed_data/Phenotypes/2021-09-07_Clean_BP_pheno_unrels_only.csv',
             pheno_file = '/2022_BP_ensemble/TOPMed_data/Phenotypes/2021-09-26_Clean_pheno_for_mixed_model.csv'
    ):
    '''
    Function to input a phenotype and alpha level, return the genotype and phenotype data

    Parameters:
        var_pheno : phenotype name in phenotype file, e.g. "SBP_V1" for systolic blood pressure

    Returns:
        y_train : target phenotype data for train set
        y_test : target phenotype data for test set
        phenotype_data_train : other covariate data for the train set (e.g., Age, Race, Sex)
        phenotype_data_test : other covariate data for the train set (e.g., Age, Race, Sex)
    '''

     ####################
    ## Phenotype data ##
    ####################
    # Load the phenotype data - train
    unrelated_phenotype_data = pd.read_csv(unrelated_pheno_file, dtype = {'sample.id':'str'})
    phenotype_data = pd.read_csv(pheno_file, dtype = {'sample.id':'str'})

    if exclude_meds == True:
        phenotype_data = phenotype_data.loc[~phenotype_data.HTNMED_V1.isin([1,2])]

    #define HTN_V1 as either SBP_V1 or DBP_V1 is too high or HTNMED_V1 is True
    phenotype_data['HTN_V1']= np.where((phenotype_data['HTNMED_V1']==1) | (phenotype_data['SBP_V1']>=130) |\
                                       (phenotype_data['DBP_V1']>=80) | (phenotype_data['HTNMED_V1']==1), 1, 0)

    # Index phenotype data on observation ID
    phenotype_data.index = phenotype_data['sample.id']

    if one_hot_encode == False:
        phenotype_data = phenotype_data[['race_clean', 'study','PC_1', 'PC_2', 'PC_3', 'PC_4', 'PC_5',\
                                                'PC_6','PC_7','PC_8','PC_9','PC_10','PC_11',\
                                                'GENDER','AGE_V1','BMI_V1', 'SBP_V1', 'DBP_V1','HTN_V1']]
    else:
        phenotype_data = pd.concat([pd.get_dummies(phenotype_data[['race_clean', 'study']]),
                                    phenotype_data[['PC_1', 'PC_2', 'PC_3', 'PC_4', 'PC_5',\
                                                'PC_6','PC_7','PC_8','PC_9','PC_10','PC_11',\
                                                    'GENDER','AGE_V1','BMI_V1', var_pheno]]],axis=1)

    # Exclude any subjects with missing data (NA's)
    sna = phenotype_data.isna().any(axis = 1)
    #print("Subject with NA's - Train: {}".format(phenotype_data[sna == True].shape[0]))
    phenotype_data = phenotype_data[sna == False]

    #filter in phenotype data the sample id's that are in the unrelated phenotype data for the test set
    unrelated_phenotype_data_2 = phenotype_data.loc[
    phenotype_data.index.isin(unrelated_phenotype_data['sample.id'])]

    #select randomly 30% size of phenotype mixed model data for test dataset from unrelated phenotype data
    phenotype_data_test_full = unrelated_phenotype_data_2.sample(n=round(len(phenotype_data)*0.3),
                                                       random_state=1)

    y_test = phenotype_data_test_full[var_pheno]
    phenotype_data_test = phenotype_data_test_full.drop(var_pheno, axis=1)


    #the remaining data is the train dataset from the phenotype mixed model
    phenotype_data_train_full = phenotype_data.loc[~phenotype_data.index.isin(phenotype_data_test.index)]

    y_train = phenotype_data_train_full[var_pheno]
    phenotype_data_train = phenotype_data_train_full.drop(var_pheno, axis=1)


     # Exclude outliers using the 1st and 99th percentile
    phenotype_data_test = phenotype_data_test[(y_test >= y_train.quantile(0.01)) & (y_test <= y_train.quantile(0.99))]
    y_test = y_test[(y_test >= y_train.quantile(0.01)) & (y_test <= y_train.quantile(0.99))]

    phenotype_data_train = phenotype_data_train[(y_train >= y_train.quantile(0.01)) & (y_train <= y_train.quantile(0.99))]
    y_train = y_train[(y_train >= y_train.quantile(0.01)) & (y_train <= y_train.quantile(0.99))]
    return y_train, y_test, phenotype_data_train, phenotype_data_test
#######################################################################################################
