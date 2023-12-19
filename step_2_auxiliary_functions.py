#######################################################################################################
## Define function to Pull PRS Results when using PRSs in a moodel##
#######################################################################################################

def load_prsice(var, phenotype_data_train,phenotype_data_test, gwas, single_threshold = False,
                prs_dir = '/2022_BP_ensemble/TOPMed_data/',
                ukbb_flag = False, prs_csx_race = 'EUR'):

    # Load PRSice
    if gwas=="BBJ":
        fname = prs_dir + '/'+str(gwas)+"/PRS/"+"2020-06-05_"+str(var)+'_hg38_v2_MAF0.1_500kb.all_score'
    elif (gwas =='MVP') or (gwas == 'UKBB+ICBP'):
        fname = prs_dir + '/' + str(gwas)+"/PRS/"+str(gwas)+"_2020-06-05_"+str(var)+'_hg38_v2_MAF0.1_500kb.all_score'
    elif(gwas=="PRS-CsX"):
        fname = prs_dir + 'PRS-CSx/'+str(var)+'_'+prs_csx_race+'.all_score'
    else: #BP-ICE_EUR or BP-ICE_PA and var== SBP or DBP
        if var=="HTN":
            fname = prs_dir + 'rare_variants/PRS/'+str(gwas)+'_'+str(var)+"_15-04-2020_hg38_MAF0.1_500kb_v2.all_score"
        else:
            fname = prs_dir + 'rare_variants/PRS/'+str(gwas)+'_'+str(var)+"_transformed_15-04-2020_hg38_ICBPm_MAF0.1_500kb_v2.all_score"
    prs = pd.read_csv(fname, sep = '\s+')

    # Set index
    prs.index=prs.IID
    prs.drop(['FID','IID'], axis=1, inplace=True)

    if ukbb_flag == True: #Only include "Pt_0.01"
        prs = prs[["Pt_0.01"]]
        single_threshold = False

    #only include PRS with threshold of 10e-4 if True
    if single_threshold == True:
        prs = prs[["Pt_0.01"]]

    # Train/Test split
    prs_train = prs.reindex(phenotype_data_train.index)
    prs_test = prs.reindex(phenotype_data_test.index)
    prs_train = prs_train.add_prefix(str(var)+"_"+str(gwas)+"_")
    prs_test = prs_test.add_prefix(str(var)+"_"+str(gwas)+"_")

    return prs_train, prs_test


#define Optuna Objective function
def objective(trial, data, target):
    param = {
        "verbosity": 0,
        "nthread": -1,
        "objective": "reg:squarederror",
        "eval_metric": "rmse",
        "grow_policy":"depthwise",
        "normalize_type": "tree",
        "booster": "gbtree",
        "n_estimators" : trial.suggest_int('n_estimators', 0, 1000)
    }
    param["max_depth"] = trial.suggest_int("max_depth", 1,100)
    param["min_child_weight"] = trial.suggest_int("min_child_weight", 1,100)

    param["subsample"] = trial.suggest_float("subsample", 0.1, 1, step = 0.1)
    param["colsample_bytree"] = trial.suggest_float("colsample_bytree", 0.1, 1, step = 0.1)
    param["lambda"] = trial.suggest_int("lambda",0,50, step = 1)
    param["alpha"] = trial.suggest_int("alpha",0,50, step = 1)
    param["gamma"] = trial.suggest_int("gamma",0,50, step = 1)
    param["eta"] = trial.suggest_float("eta", 0.01,0.1,step=0.01)
    model = XGBRegressor(**param)

    return cross_val_score(model, data, target, cv=5, scoring ='explained_variance').mean()


#######################################################################################################
## Define function to run linear model on age and sex and return the adjusted phenotype ##
#######################################################################################################

def adj_pheno(pheno_train, pheno_test, y_train, y_test,xgb_params,xgb_n_estimators,
              model_type = "linear regression", prs_include = False,
              pc_include = False, ukbb_flag = False,
              gwas= 'UKBB+ICBP', var='SBP', output_results = False, race_results = True,
              save_model_weights = False, load_model_weights = False, model_weights_name = 'baseline_model_weights_xgb',
              topmed_data_export_path = "/2022_BP_ensemble/Data/TOPMed_phenotypes/baseline adjusted model/",
              train_export_data_name = 'training_dataset_20231012',
              test_export_data_name = 'testing_dataset_20231012'):

    model_weights_dir = '/2022_BP_ensemble/TOPMed_trained_genetic_model_weights/genetic model weights_v2/'

    if pc_include == False:
        if set(['PC_1', 'PC_2', 'PC_3', 'PC_4', 'PC_5','PC_6','PC_7','PC_8','PC_9','PC_10','PC_11']).issubset(pheno_train.columns):
            pheno_train = pheno_train.drop(['PC_1', 'PC_2', 'PC_3', 'PC_4', 'PC_5','PC_6','PC_7','PC_8','PC_9','PC_10','PC_11'], axis=1)
        if set(['PC_1', 'PC_2', 'PC_3', 'PC_4', 'PC_5','PC_6','PC_7','PC_8','PC_9','PC_10','PC_11']).issubset(pheno_test.columns):
            pheno_test = pheno_test.drop(['PC_1', 'PC_2', 'PC_3', 'PC_4', 'PC_5','PC_6','PC_7','PC_8','PC_9','PC_10','PC_11'], axis=1)

    if prs_include == True:
        #Add in Full PRS
        if(gwas != 'PRS-CsX'):
            full_prs_train_gwas, full_prs_test_gwas = load_prsice(var,pheno_train, pheno_test, gwas, \
                                                        single_threshold=False)
        else: #load all prs-csx for all race and concatenate together
            full_prs_train_gwas_eur, full_prs_test_gwas_eur = load_prsice(var,pheno_train, pheno_test, gwas, \
                                                        single_threshold=False, prs_csx_race = 'EUR')
            full_prs_train_gwas_eur = full_prs_train_gwas_eur.add_prefix('EUR_')
            full_prs_test_gwas_eur = full_prs_test_gwas_eur.add_prefix('EUR_')

            full_prs_train_gwas_afr, full_prs_test_gwas_afr = load_prsice(var,pheno_train, pheno_test, gwas, \
                                                        single_threshold=False, prs_csx_race = 'AFR')
            full_prs_train_gwas_afr = full_prs_train_gwas_afr.add_prefix('AFR_')
            full_prs_test_gwas_afr = full_prs_test_gwas_afr.add_prefix('AFR_')

            full_prs_train_gwas_eas, full_prs_test_gwas_eas = load_prsice(var,pheno_train, pheno_test, gwas, \
                                                        single_threshold=False, prs_csx_race = 'EAS')
            full_prs_train_gwas_eas = full_prs_train_gwas_eas.add_prefix('EAS_')
            full_prs_test_gwas_eas = full_prs_test_gwas_eas.add_prefix('EAS_')

            full_prs_train_gwas = pd.concat([full_prs_train_gwas_eur, full_prs_train_gwas_afr,\
                                                                full_prs_train_gwas_eas], axis = 1)
            full_prs_test_gwas = pd.concat([full_prs_test_gwas_eur, full_prs_test_gwas_afr,\
                                                                full_prs_test_gwas_eas], axis = 1)

        pheno_train = pd.concat([pheno_train, full_prs_train_gwas], axis = 1)
        sna_train = pheno_train.isna().any(axis = 1)
        pheno_train = pheno_train[sna_train == False]

        pheno_test = pd.concat([pheno_test, full_prs_test_gwas], axis = 1)
        sna_test = pheno_test.isna().any(axis = 1)
        pheno_test = pheno_test[sna_test == False]

        y_train = y_train[sna_train == False]
        y_test = y_test[sna_test == False]

        print('# rows train: '+str(len(y_train)))
        print('# rows test: '+str(len(y_test)))

    if ukbb_flag == True:
        #Add in Full PRS
        full_prs_train_gwas, full_prs_test_gwas = load_prsice("SBP",pheno_train, pheno_test, "UKBB+ICBP", \
                                                        single_threshold=False, ukbb_flag = True)
        pheno_train = pd.concat([pheno_train, full_prs_train_gwas], axis = 1)
        pheno_test = pd.concat([pheno_test, full_prs_test_gwas], axis = 1)

    # Run lm
    if model_type == "linear regression":
        if load_model_weights==True:
            lm = pickle.load(open(model_weights_dir+model_weights_name+'_lr_'+var+'.pkl', "rb"))
        # Train model
        else:
            lm = LinearRegression(fit_intercept=True, n_jobs=-1).fit(pheno_train, y_train)
        if save_model_weights==True:
            pickle.dump(lm, open(model_weights_dir+model_weights_name+'_lr_'+var+'.pkl', 'wb'))
        # Calculate genetic component
        y_train_pred = lm.predict(pheno_train)
        y_test_pred = lm.predict(pheno_test)

    elif model_type == "xgboost":
        D_train = xgb.DMatrix(pheno_train, label=list(y_train))
        D_test = xgb.DMatrix(pheno_test, label=list(y_test))

        if load_model_weights==True:
            xgb_model = pickle.load(open(model_weights_dir+model_weights_name+'_'+var+'.pkl', "rb"))
        # Train model
        else:
            xgb_model = xgb.train(xgb_params, D_train, num_boost_round = xgb_n_estimators)
        if save_model_weights==True:
            pickle.dump(xgb_model, open(model_weights_dir+model_weights_name+'_'+var+'.pkl', 'wb'))
        # Calculate genetic component
        y_train_pred = xgb_model.predict(D_train)
        y_test_pred = xgb_model.predict(D_test)

        y_train_pred_df = pd.DataFrame(y_train_pred, columns = [y_train.name+'_prediction'])
        y_train_pred_df.index = pheno_train.index
        y_train_resid_df = (y_train - y_train_pred).rename(y_train.name+'_residuals')

        y_test_pred_df = pd.DataFrame(y_test_pred, columns = [y_test.name+'_prediction'])
        y_test_pred_df.index = pheno_test.index
        y_test_resid_df = (y_test - y_test_pred).rename(y_test.name+'_residuals')

        #update index for y_train_pred and residuals
        train_dataset_export = pd.concat([pheno_train,y_train, y_train_pred_df, y_train_resid_df], axis = 1)
        test_dataset_export = pd.concat([pheno_test,y_test, y_test_pred_df, y_test_resid_df], axis = 1)
        train_dataset_export.to_csv(topmed_data_export_path+train_export_data_name+".csv")
        test_dataset_export.to_csv(topmed_data_export_path+test_export_data_name+".csv")

    else:
        print("model type input is wrong; input classification or regression")
        exit()

    # Print Metrics
    print("Train R^2 Covariates: {}".format(r2_score(y_train, y_train_pred)))
    print("Test R^2 Covariates: {}".format(r2_score(y_test, y_test_pred)))

    train_result = explained_variance_score(y_train, y_train_pred)
    test_result = explained_variance_score(y_test, y_test_pred)
    print("Train EVR Covariates: {}".format(train_result))
    print("Test EVR Covariates: {}".format(test_result))

    # Calculate residuals
    y_train_resid = y_train - y_train_pred
    y_test_resid = y_test - y_test_pred

    #results by race
    #AA
    if race_results == True:
        train_result_aa = explained_variance_score(y_train[pheno_train.race_clean_AA==1],
                                                   y_train_pred[pheno_train.race_clean_AA==1])
        test_result_aa = explained_variance_score(y_test[pheno_test.race_clean_AA==1],
                                                   y_test_pred[pheno_test.race_clean_AA==1])

        #AsA
        train_result_asa = explained_variance_score(y_train[pheno_train.race_clean_AsA==1],
                                                   y_train_pred[pheno_train.race_clean_AsA==1])
        test_result_asa = explained_variance_score(y_test[pheno_test.race_clean_AsA==1],
                                                   y_test_pred[pheno_test.race_clean_AsA==1])

        #EA
        train_result_ea = explained_variance_score(y_train[pheno_train.race_clean_EA==1],
                                                   y_train_pred[pheno_train.race_clean_EA==1])
        test_result_ea = explained_variance_score(y_test[pheno_test.race_clean_EA==1],
                                                   y_test_pred[pheno_test.race_clean_EA==1])

        #HA
        train_result_ha = explained_variance_score(y_train[pheno_train.race_clean_HA==1],
                                                   y_train_pred[pheno_train.race_clean_HA==1])
        test_result_ha = explained_variance_score(y_test[pheno_test.race_clean_HA==1],
                                                   y_test_pred[pheno_test.race_clean_HA==1])

        train_results_list = [train_result, train_result_aa, train_result_asa, \
                              train_result_ea, train_result_ha]

        test_results_list = [test_result, test_result_aa, test_result_asa, \
                             test_result_ea, test_result_ha]

        train_results_n = [len(y_train), len(y_train[pheno_train.race_clean_AA==1]),
                          len(y_train[pheno_train.race_clean_AsA==1]), len(y_train[pheno_train.race_clean_EA==1]),
                          len(y_train[pheno_train.race_clean_HA==1])]

        test_results_n = [len(y_test), len(y_test[pheno_test.race_clean_AA==1]),
                          len(y_test[pheno_test.race_clean_AsA==1]), len(y_test[pheno_test.race_clean_EA==1]),
                          len(y_test[pheno_test.race_clean_HA==1])]

        outcome = [var]*len(train_results_list)

        # Collect the results of the predictions
        metadata = {'phenotype': outcome,
                    'group': ['overall','AA','AsA','EA','HA'],
                    'model_type':[model_type]*len(outcome),
                    'n_train':train_results_n,
                    'n_test':test_results_n,
                    'train_result': train_results_list,
                    'test_result': test_results_list}

        metadata = pd.DataFrame.from_dict(metadata)

    if output_results == False:
        return y_train_resid, y_test_resid
    else:
        return y_train_resid, y_test_resid, metadata



# compute prediction intervals
def bootstrap_prediction_intervals(var, pheno_test, y_test, model_type, model_weights_name):
    model_weights_directory = '/2022_BP_ensemble/TOPMed_trained_genetic_model_weights/genetic model weights_v2/'
    fitted_model = pickle.load(open(model_weights_directory+model_weights_name, "rb"))

    evr_list = []
    evr_list_aa = []
    evr_list_asa = []
    evr_list_ea = []
    evr_list_ha = []

    for i in range(99):
        #sample with replacement from the test set with the same size
        baseline_pheno_data_sample = pheno_test.sample(n=len(pheno_test), replace=True)
        baseline_y_test_sample = y_test.reindex(baseline_pheno_data_sample.index)
        if model_type == 'xgboost':
        #use fitted model to predict from sampled test dataset, calculate PVE, then take results and append to evr list
            test_sample = xgb.DMatrix(baseline_pheno_data_sample, label=list(baseline_y_test_sample))
        else:
            test_sample = baseline_pheno_data_sample
        y_test_predict_sample = fitted_model.predict(test_sample)

        #Overall
        test_result_sample = explained_variance_score(baseline_y_test_sample, y_test_predict_sample)
        #AA
        test_result_aa = explained_variance_score(baseline_y_test_sample[baseline_pheno_data_sample.race_clean_AA==1],
                                                   y_test_predict_sample[baseline_pheno_data_sample.race_clean_AA==1])
        #AsA
        test_result_asa = explained_variance_score(baseline_y_test_sample[baseline_pheno_data_sample.race_clean_AsA==1],
                                                   y_test_predict_sample[baseline_pheno_data_sample.race_clean_AsA==1])
        #EA
        test_result_ea = explained_variance_score(baseline_y_test_sample[baseline_pheno_data_sample.race_clean_EA==1],
                                                   y_test_predict_sample[baseline_pheno_data_sample.race_clean_EA==1])
        #HA
        test_result_ha = explained_variance_score(baseline_y_test_sample[baseline_pheno_data_sample.race_clean_HA==1],
                                                   y_test_predict_sample[baseline_pheno_data_sample.race_clean_HA==1])

        evr_list.append(test_result_sample)
        evr_list_aa.append(test_result_aa)
        evr_list_asa.append(test_result_asa)
        evr_list_ea.append(test_result_ea)
        evr_list_ha.append(test_result_ha)

    group = ['overall','AA','AsA','EA','HA']
    lower_bound = [np.percentile(evr_list, 2.5), np.percentile(evr_list_aa, 2.5),np.percentile(evr_list_asa, 2.5),\
                  np.percentile(evr_list_ea, 2.5), np.percentile(evr_list_ha, 2.5)]

    upper_bound = [np.percentile(evr_list, 97.5), np.percentile(evr_list_aa, 97.5),np.percentile(evr_list_asa, 97.5),\
                  np.percentile(evr_list_ea, 97.5), np.percentile(evr_list_ha, 97.5)]

        # Collect the results of the predictions
    metadata = pd.DataFrame.from_dict({'phenotype': [var]*len(group),
                'group': group,
                'model_type':[model_type]*len(group),
                'lower_bound_2.5':lower_bound,
                'upper_bound_97.5': upper_bound})

    return(metadata)
