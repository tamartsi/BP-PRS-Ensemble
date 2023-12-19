def load_data_mgbb(phenotype_data_mgb, var_pheno):
# Index phenotype data on observation ID
    phenotype_data_mgb.index = phenotype_data_mgb['sample.id']
    #convert race columns
    phenotype_data_mgb.loc[~phenotype_data_mgb["race_clean"].isin(['HISPANIC OR LATINO','WHITE','ASIAN','BLACK','HISPANIC',\
                                                    'SPANISH', "BLACK OR AFRICAN AMERICAN"]), "race_clean"] = 'other'

    phenotype_data_mgb= phenotype_data_mgb.replace({'race_clean' : {'HISPANIC OR LATINO' : 'race_clean_HA', \
                                                                    'WHITE' : 'race_clean_EA', \
                                                                    'ASIAN' : 'race_clean_AsA',\
                                                                    'BLACK': "race_clean_AA",\
                                                                    'BLACK OR AFRICAN AMERICAN':"race_clean_AA",\
                                                                    'HISPANIC':"race_clean_HA",\
                                                                    "SPANISH":"race_clean_EA",\
                                                                   'other': 'race_clean_Other/Unknown'}})

    #concatenate columns
    phenotype_data = pd.concat([pd.get_dummies(phenotype_data_mgb['race_clean']),
                                phenotype_data_mgb[['GENDER','AGE_V1','BMI_V1', var_pheno]]],axis=1)
    # Exclude any subjects with NA's
    sna = phenotype_data.isna().any(axis = 1)
    print("Subject with NA's - Train: {}".format(phenotype_data[sna == True].shape[0]))

    phenotype_data = phenotype_data[sna == False]

    #   phenotype_data_mgb['race_clean_Other/Unknown'] = 0
    studies_list = ['study_ARIC','study_Amish','study_BioMe','study_CARDIA','study_CFS', 'study_CHS', 'study_COPDGene', 'study_FHS',
                'study_GENOA', 'study_GenSalt', 'study_HCHS_SOL', 'study_JHS', 'study_MESA', 'study_THRV', 'study_WHI']
    for i in studies_list:
        phenotype_data[i] = np.nan

    phenotype_data =  phenotype_data[['race_clean_AA', 'race_clean_AsA', 'race_clean_EA', 'race_clean_HA',
                       'race_clean_Other/Unknown', 'study_ARIC', 'study_Amish', 'study_BioMe',
                       'study_CARDIA', 'study_CFS', 'study_CHS', 'study_COPDGene', 'study_FHS',
                       'study_GENOA', 'study_GenSalt', 'study_HCHS_SOL', 'study_JHS', 'study_MESA',
                       'study_THRV', 'study_WHI', 'GENDER', 'AGE_V1', 'BMI_V1', var_pheno]]


    y = phenotype_data[var_pheno]
    phenotype_data = phenotype_data.drop(var_pheno, axis=1)
    return y, phenotype_data

def mgb_adj_pheno(x_data, y_data, model_type = "xgboost", optuna_flag = False, output_results = False, tune_num_boost = False,
                 params =  {'booster': 'gbtree', 'eta': 0.054, 'max_depth': 81, 'min_child_weight': 44,
                               'subsample': 0.2, 'colsample_bytree': 0.5,
                               'lambda': 138, 'alpha': 107, 'gamma': 188,
                               'nthread': -1}, optuna_num_boost_round = 0, save_mod = False, mod_name = 'mgb_baseline_model_weights',
                  load_baseline_topmed_model = False, load_model_name = 'TOPMed_baseline_xgb_model_weights'):
    model_weights_dir = '/2022_BP_ensemble/TOPMed_trained_genetic_model_weights/'
    model_weights_dir_2 = '/2022_BP_ensemble/TOPMed_trained_genetic_model_weights/genetic model weights_v2/'
    # Run lm
    if model_type == "linear regression":
        lm = LinearRegression(fit_intercept=True, n_jobs=-1).fit(x_data, y_data)
        # Calculate genetic component
        y_train_pred = lm.predict(x_data)

    elif model_type == "xgboost":
        D_train = xgb.DMatrix(x_data, label=list(y_data))
        if optuna_flag == False:
            if tune_num_boost == True:
                # Run XGBoost with CV for number of trees
                params['objective'] = 'reg:squarederror'
                cv_results = xgb.cv(params,
                                    D_train,
                                    num_boost_round=1000,
                                    early_stopping_rounds=10,
                                    nfold=5,
                                    as_pandas=True,
                                    metrics={'rmse'})

                # Train model with best B from CV
                best_B = cv_results['test-rmse-mean'].idxmin()
                mean_metric = cv_results['test-rmse-mean'].min()
                print("RMSE {} with Best B = {}".format(mean_metric, best_B))
                # Train model with best B from CV
                xgb_model = xgb.train(params, D_train, num_boost_round = best_B)

            #load baseline model weights from Topmed
            elif load_baseline_topmed_model == True:
                xgb_model = pickle.load(open(model_weights_dir_2+load_model_name+'.pkl', "rb"))
            else:
                xgb_model = xgb.train(params, D_train, num_boost_round = optuna_num_boost_round)
        else:
                study = optuna.create_study(direction="maximize")

                # Pass additional arguments inside another function
                baseline_func = lambda trial: objective(trial,x_data, y_data)
                study.optimize(baseline_func, n_trials=50)
                print(study.best_trial)
                print(study.best_trial.params)
                params = study.best_trial.params
                params['nthread'] = -1
                xgb_model = xgb.train(params, D_train, num_boost_round = params['n_estimator'])

        # Calculate genetic component
        y_train_pred = xgb_model.predict(D_train)

        if save_mod == True:
            pickle.dump(xgb_model, open(model_weights_dir+mod_name+'.pkl', 'wb'))
            mod_name = 'mgb_baseline_model_weights'
    else:
        print("model type input is wrong; input classification or regression")
        exit()

    # Print Metrics
    result = explained_variance_score(y_data, y_train_pred)
    print("Train R^2 Covariates: {}".format(r2_score(y_data, y_train_pred)))
    print("Train EVR Covariates: {}".format(result))
    # Calculate residuals
    y_train_resid = y_data - y_train_pred

    #results by race
    #AA
    result_aa = explained_variance_score(y_data[x_data.race_clean_AA==1],
                                               y_train_pred[x_data.race_clean_AA==1])

    #AsA
    result_asa = explained_variance_score(y_data[x_data.race_clean_AsA==1],
                                               y_train_pred[x_data.race_clean_AsA==1])

    #EA
    result_ea = explained_variance_score(y_data[x_data.race_clean_EA==1],
                                               y_train_pred[x_data.race_clean_EA==1])

    results_list = [result, result_aa, result_asa, \
                          result_ea]

    results_n = [len(y_data), len(y_data[x_data.race_clean_AA==1]),
                      len(y_data[x_data.race_clean_AsA==1]), len(y_data[x_data.race_clean_EA==1])
                ]

    # Collect the results of the predictions
    metadata = {'group': ['overall','AA','AsA','EA'],
                    'n_results':results_n,
                    'results': results_list}

    metadata = pd.DataFrame.from_dict(metadata)
    if output_results==False:
        return y_train_resid
    else:
        return y_train_resid, metadata

def preprocess_mgbb_has_genetic_data(mgbb_df,mgbb_df_y, fam_df):
    mgb_data_in_prs_fam = mgbb_df.index.isin(fam_df["Biobank Subject ID"])
    mgbb_df_y_fam = mgbb_df_y[mgb_data_in_prs_fam==True]
    phenotype_data_mgb_df = mgbb_df[mgb_data_in_prs_fam==True]
    phenotype_data_mgb_df['GENDER'] = np.where(phenotype_data_mgb_df['GENDER']=="F",1,0)
    return mgbb_df_y_fam, phenotype_data_mgb

def load_prsice_mgbb(var, phenotype_data, gwas, single_threshold = False,
                prs_dir ='/2022_BP_ensemble/MGB_PRS_files/MGB_Biobank/',
                ukbb_flag = False):
    # Load PRSice
    fname = prs_dir + "MGB_"+str(gwas)+"_"+str(var)+"_ls-all_R_0.1_500kb.all_score"
    prs = pd.read_csv(fname, sep = '\s+')

    prs['IID'] = prs['IID'].str.split('-').str[1]

    #remove duplicate IID by keeping the first value
    prs = prs.loc[prs.duplicated(subset=['IID'])==False]

    #remove leading 0s in IID
    prs['IID'] = [s.lstrip("0") for s in prs['IID']]

    # Set index
    prs.index=prs.IID
    prs.drop(['FID','IID'], axis=1, inplace=True)

    if ukbb_flag == True: #Only include "Pt_0.01"
        prs = prs[["Pt_0.01"]]
        single_threshold = False

    #only include PRS with threshold of Pt_0.01 if True
    if single_threshold == True:
        prs = prs[["Pt_0.01"]]

    prs = prs.reindex(phenotype_data.index)
    prs = prs.add_prefix(str(var)+"_"+str(gwas)+"_")
    return prs
