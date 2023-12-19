import pandas as pd
import numpy as np


#######################################################################################################
## Define function to run linear or logistic regression (to compare to running xgboost)              ##
#######################################################################################################

def run_regr(var, X, y, model_type,
             race_results=True, save_model = False,
             save_model_name = 'lg_model_fit', load_model_weights = False):
    model_weights_dir = '/2022_BP_ensemble/TOPMed_trained_genetic_model_weights/genetic model weights_v2/'

    if model_type == 'regression': # For SBP and DBP
            if load_model_weights ==True:
                lm = pickle.load(open(model_weights_dir+save_model_name+'_'+var+'.pkl', "rb"))
            else:
                lm = LinearRegression(fit_intercept=True, n_jobs=-1).fit(X, y)
            # Calculate genetic component

            genetic_component_train = lm.predict(X)

            train_result = explained_variance_score(y, genetic_component_train)

            if save_model==Tpreidue:
                pickle.dump(lm, open(model_weights_dir+save_model_name+'_'+var+'.sav', 'wb'))

    elif model_type == 'classification': #Accuracy for HTN
        # Run logistic regression
        log = LogisticRegression(fit_intercept=True).fit(X, y)
        # Calculate genetic component
        genetic_component_train = log.predict(X)
        genetic_component_train = [np.round(x) for x in genetic_component_train]

        train_result = accuracy_score(y, genetic_component_train)
        if save_model==True:
                pickle.dump(log, open(model_weights_dir+save_model_name+'_'+var+'.sav', 'wb'))

        print("Accuracy Linear Regression: {}".format(train_result))

    if model_type == 'regression': #SBP or DBP
        train_result = explained_variance_score(y, genetic_component_train)

        print("EVR Linear Regression: {}".format(train_result))

        race_list = ['aa','asa','ea']
        race_column_names_list = ['race_clean_AA','race_clean_AsA',\
                                      'race_clean_EA']

        if race_results == True:
            #results by race
            #AA
            train_result_aa = explained_variance_score(y[X.race_clean_AA==1],
                                                       genetic_component_train[X.race_clean_AA==1])

            #AsA
            train_result_asa = explained_variance_score(y[X.race_clean_AsA==1],
                                                       genetic_component_train[X.race_clean_AsA==1])

            #EA
            train_result_ea = explained_variance_score(y[X.race_clean_EA==1],
                                                       genetic_component_train[X.race_clean_EA==1])

            train_race_results = [train_result_aa, train_result_asa, \
                                  train_result_ea] #, train_result_ha
            train_result_list = [train_result]+train_race_results

        print("")
        return genetic_component_train.squeeze(), train_result_list #, genetic_component_test, test_result_list




#######################################################################################################
## Define function to run XGB, cross validate for the optimal number of trees, and report out metrics ##
#######################################################################################################

def run_prediction(var, X, y_train, model_type,
            params = {'alpha': 15, 'colsample_bytree': 0.8,
              'eta': 0.01, 'gamma': 0, 'max_depth': 6,
              'min_child_weight': 16, 'subsample': 0.5, 'nthread':-1}, set_num_boost_rounds = False, race_results = True,
            optuna_flag = False, save_model = False,
            model = 'xgboost',
            save_model_name = 'model_weights', n_jobs=1000, grid_search_global = False,
            print_features = False,
            load_model_weights = False,
            grid_search_indicator_lasso = False,
            optuna_csv_save = '/2022_BP_ensemble/TOPMed_trained_genetic_model_weights/genetic model weights_v2/optuna_genetic_tuning.csv',
           xgb_boost_rounds = 290, alpha = 0.0008251094706741554,lasso_alpha_list = np.linspace(start=np.log(1),stop=np.log(1.02), num=25),):

    model_weights_dir = '/2022_BP_ensemble/TOPMed_trained_genetic_model_weights/genetic model weights_v2/'
    if print_features == True:
        print(X.columns)

    if model == 'xgboost':
        # Convert to XGBoost D-Matrix
        D_train = xgb.DMatrix(X, label=y_train)
        if load_model_weights == True:
            prediction_model = pickle.load(open(model_weights_dir+save_model_name+'_'+ var +'.pkl', "rb"))
            mean_metric = 0
            feature_important = prediction_model.get_score(importance_type='gain')
            keys = list(feature_important.keys())
            values = list(feature_important.values())
            feature_important_df = pd.DataFrame(data=values, index=keys, columns=["score"]).sort_values(by = "score", ascending=False)
        else:
            if model_type == 'regression': # For SBP and DBP
                if optuna_flag == True:
                    study = optuna.create_study(direction='maximize')
                    study.optimize(lambda trial : objective(trial,X,y_train),n_trials= 50)
                    hist_study = study.trials_dataframe()
                    hist_study.to_csv(optuna_csv_save)
                    optuna_params = study.best_trial.params.copy()
                    del optuna_params['n_estimators']
                    params['nthread']=-1
                    xgb_boost_rounds = study.best_params['n_estimators']
                    print(study.best_params)
                    mean_metric = float("nan")
                    prediction_model = xgb.train(params, D_train, num_boost_round = xgb_boost_rounds)
                if set_num_boost_rounds == True:
                    # Run XGBoost with CV for number of trees
                    params['objective'] = 'reg:squarederror'
                    cv_results = xgb.cv(params,
                                        D_train,
                                        num_boost_round=1000,
                                        early_stopping_rounds=10,
                                        nfold=5,
                                        as_pandas=True,
                                        metrics={'rmse'})
                    mean_metric = cv_results['test-rmse-mean'].min()
                    best_B = cv_results['test-rmse-mean'].idxmin()
                    print("RMSE {} with Best B = {}".format(mean_metric, best_B))
                    # Train model with best B from CV
                    prediction_model = xgb.train(params, D_train, num_boost_round = best_B)
                else:
                    mean_metric = float("nan")
                    prediction_model = xgb.train(params, D_train, num_boost_round = xgb_boost_rounds)
                if save_model==True:
                    pickle.dump(prediction_model, open(model_weights_dir+save_model_name+'_'+var+'.pkl', 'wb'))

            ###################

            else: #cut and print error
                print("model type input is wrong; input classification or regression")
                exit()
            # Feature Importance
            feature_important = prediction_model.get_score(importance_type='gain')
            keys = list(feature_important.keys())
            values = list(feature_important.values())
            feature_important_df = pd.DataFrame(data=values, index=keys, columns=["score"]).sort_values(by = "score", ascending=False)
            print("Feature Importance (Gain):")
            print(feature_important_df)

            # Save model
            if save_model==True:
                pickle.dump(prediction_model, open(model_weights_dir+save_model_name+'_'+var+'.pkl', 'wb'))

        # Calculate genetic component
        genetic_component_train = prediction_model.predict(D_train)

    else: #lasso
        feature_importance = []

        #create training sub A and Test A
        sub_phenotype_data_train, sub_phenotype_data_test, sub_y_train, sub_y_test = train_test_split(X, y_train,
                                                                                                     test_size = 0.30,
                                                                                                     random_state=1)
        scaler = preprocessing.StandardScaler()
        scaler_train = scaler.fit(X)

        scale_train_df = scaler_train.transform(X)

        alphas = lasso_alpha_list
        lasso = Lasso(max_iter=10000)
        mean_metric = float("nan")

        if grid_search_indicator_lasso == True:
            for a in alphas:
                lasso.set_params(alpha=a)
                lasso.fit(sub_phenotype_data_train, sub_y_train)
                feature_importance.append(lasso.coef_)
            prediction_model = LassoCV(alphas = lasso_alpha_list).fit(sub_phenotype_data_train, sub_y_train)
            print("alpha used: {}".format(prediction_model.alpha_))
        else:
            if load_model_weights == True:
                prediction_model = pickle.load(open(model_weights_dir+save_model_name+'_'+var+'.pkl', "rb"))
            else:
                prediction_model = Lasso(alpha = alpha).fit(X, y_train) #alpha used: 0.0008251094706741554
            feature_importance.append(prediction_model.coef_)
            genetic_component_train = prediction_model.predict(X)

            #print out non-zero coefficients features selected
            print("count of features: {}".format(len(X.columns)))
            print("count of nonzero coefficient features: {}".format(len(X.columns[prediction_model.coef_>0])))
            feature_selection = X.columns[prediction_model.coef_>0]
            feature_important_df = pd.DataFrame(data=prediction_model.coef_[prediction_model.coef_>0], index=feature_selection, columns=["score"]).sort_values(by = "score", ascending=False)

            train_result = explained_variance_score(y_train, genetic_component_train)
            print("Lasso Regression results:")
            print("Train EVR Lasso Regression: {}".format(train_result))
        if save_model==True:
                    pickle.dump(prediction_model, open(model_weights_dir+save_model_name+'_'+var+'.pkl', 'wb'))

    train_result = explained_variance_score(y_train, genetic_component_train)

    print("EVR XGB: {}".format(train_result))

    if race_results == True:
        #results by race
        #AA
        train_result_aa = explained_variance_score(y_train[X.race_clean_AA==1],
                                                   genetic_component_train[X.race_clean_AA==1])

        #AsA
        train_result_asa = explained_variance_score(y_train[X.race_clean_AsA==1],
                                                   genetic_component_train[X.race_clean_AsA==1])

        #EA
        train_result_ea = explained_variance_score(y_train[X.race_clean_EA==1],
                                                   genetic_component_train[X.race_clean_EA==1])

        train_race_results = [train_result_aa, train_result_asa, \
                              train_result_ea]

        #aggregate the overall EVR with the Race EVR by training and testing
        train_result_list = [train_result]+train_race_results
        race_list = ['aa','asa','ea','other']

    return genetic_component_train, mean_metric, train_result_list, feature_important_df

#######################################################################################################
#######################################################################################################



#phenotype_list: list of phenotypes to make predictions for (ie. SBP_V1, DBP_V1, HTNMED_V1)
#adj_phenotype: If True, predict raw Phenotype and use residuals as new output; set default to xgboost
#prediction_model_type: which model to run the final prediction model with (either xgboost or linear regression)

def run_model(phenotype_list, var_list, gwas,
              prs_train,
              base_model = "Yes", pc_include = True,
              ukbb_flag = False,
              adj_phenotype = True,
              tune_xgb = False,
              remove_na = False,
              include_race_results = True,
              remove_csx = False,
              prediction_model_type= "xgboost",
              params = {'booster': 'gbtree', 'eta': 0.039,
                               'max_depth': 25, 'min_child_weight': 41,
                               'subsample': 0.4, 'colsample_bytree': 1.0,
                               'lambda': 257, 'alpha': 212, 'gamma': 114, 'nthread':-1},
             xgb_n_estimator = 0,
             # for lasso
             alpha = 0.0008251094706741554,
             lasso_alpha_list = np.linspace(start=np.log(1),stop=np.log(1.02), num=25),
             residuals_model = "linear regression",
             print_features_model = False,
             #add loaded data with adjustments outside this function
             y_train_loaded = pd.DataFrame(),
             adj_y_train_loaded=pd.DataFrame(),
             phenotype_data_train_loaded = pd.DataFrame(),
             save_model = False, model_name_saved = "genetic_model_weights",
             remove_study = False,
             exclude_meds_flag = False,
             model_weights_load = False,
             grid_search_indicator_lasso = False,
             optuna_tuning_save = 'optuna_genetic_tuning.csv'):
    mean_rmse_cv_xgbs = []
    feature_importance_list = []

    study_columns = ['study_WHI', 'study_CHS', 'study_Amish', 'study_CFS', 'study_BioMe', 'study_FHS', 'study_COPDGene', 'study_THRV', \
                     'study_ARIC', 'study_CARDIA', 'study_GenSalt', 'study_HCHS_SOL', 'study_MESA', \
                     'study_JHS', 'study_GENOA']
    optuna_dir = '/2022_BP_ensemble/TopMed_optuna_results/'
    #create models for each phenotype
    for i in range(len(phenotype_list)):
        phenotype = phenotype_list[i]
        var = var_list[i]
        print("PHENOTYPE:",phenotype)

        y_train = y_train_loaded
        y_train_adj = adj_y_train_loaded
        phenotype_data_train = phenotype_data_train_loaded

        if(base_model=="Yes"): #don't include PRS as features
            train_df = phenotype_data_train
        else:
            train_df = pd.concat([phenotype_data_train, prs_train], axis=1)
        if remove_na==True: #remove rows where there's blanks for prs_csx
            sna_train = train_df.isna().any(axis = 1)
            train_df = train_df[sna_train == False]

            y_train_adj = y_train_adj[sna_train == False]
            y_train = y_train[sna_train == False]

            print('# rows train: '+str(len(y_train_adj)))

            if remove_csx==True:
                train_df = train_df[train_df.columns.drop(list(train_df.filter(regex='PRS-CsX')))]

        if remove_study==True:
            train_df= train_df.drop(columns=study_columns)
            print(train_df.columns)

        if prediction_model_type=="xgboost":

            genetic_component_train, mean_rmse_cv_xgb, train_result_xgb, feature_importance = \
            run_prediction(phenotype, train_df, y_train_adj, model_type = "regression",
            race_results = include_race_results, print_features = print_features_model, params=params,save_model=save_model,
                   save_model_name=model_name_saved,
                   load_model_weights = model_weights_load, optuna_flag = tune_xgb,
                   optuna_csv_save = optuna_dir+optuna_tuning_save)

            print("")
            # Append info
            mean_rmse_cv_xgbs.append(mean_rmse_cv_xgb)
            train_results_list = train_result_xgb
            feature_importance_list.append(feature_importance.head(10))

        elif prediction_model_type == "lasso_regression":
            genetic_component_train, mean_rmse_cv_xgb, train_result_xgb, feature_importance = \
            run_prediction(phenotype, train_df, y_train_adj, model_type = "regression",
            race_results = include_race_results, print_features = print_features_model, params=params,save_model=save_model,
                   save_model_name=model_name_saved,
                           load_model_weights = model_weights_load, optuna_flag = tune_xgb,
                   optuna_csv_save = optuna_dir+optuna_tuning_save, model = prediction_model_type)

            print("")
            # Append info
            mean_rmse_cv_xgbs.append(mean_rmse_cv_xgb)
            train_results_list = train_result_xgb
            feature_importance_list.append(feature_importance.head(10))

        else: #linear or logistic regression
            genetic_component_train, train_result_regression = \
                run_regr(var = phenotype, X = train_df, y = y_train_adj,\
                         model_type = "regression", \
                         race_results = include_race_results)

            train_results_list = train_result_regression

        if phenotype !="HTN_V1":
            #y_train - y_train_adj = y1_train_hat
            genetic_component_train = pd.DataFrame({'genetic_component_train':genetic_component_train}).set_index(y_train.index)

            genetic_component_train = genetic_component_train.squeeze()
            y_train_adj = y_train_adj.squeeze()

            y_train_full_prediction = genetic_component_train+y_train-y_train_adj



            full_train_result = explained_variance_score(y_train, y_train_full_prediction)
            print("Train EVR from model1+model2 predictions: {}".format(full_train_result))
            race_list = ['race_clean_AA','race_clean_AsA','race_clean_EA']

            #create lists appending the results from the full prediction for race
            race_full_train_result_list = []
            n_full_train_list = []

            for r in race_list:
                full_train_result_prediction_race=genetic_component_train[train_df[r]==1]+y_train[train_df[r]==1]-y_train_adj[train_df[r]==1]
                race_full_train_result = explained_variance_score(y_train[train_df[r]==1], full_train_result_prediction_race)
                race_full_train_result_list.append(race_full_train_result)
                n_full_train_list.append(len(full_train_result_prediction_race))

    group_list = ["Overall","AA","AsA","EA"]
    full_train_result_list = [full_train_result]+race_full_train_result_list
    full_n_train = [len(y_train_full_prediction)]+n_full_train_list
    if prediction_model_type=="xgboost":
        # Collect the results of the predictions

        metadata = {'phenotype': [phenotype]*len(group_list),
                    'genetic_model_type':[prediction_model_type]*len(group_list),
                    'group': group_list,
                    'n_train':full_n_train,
                    'train_result_xgb': train_results_list,
                    'full_result_train': full_train_result_list}
    else:
        # Collect the results of the predictions
        metadata = {'phenotype': [phenotype]*len(group_list),
                    'genetic_model_type':[prediction_model_type]*len(group_list),
                    'group': group_list,
                    'n_train':full_n_train,
                    'train_result': train_results_list,
                    'full_result_train': full_train_result_list}

    metadata = pd.DataFrame.from_dict(metadata)
    print(metadata)
    return genetic_component_train, metadata, feature_importance_list



# var: SBP_V1, DBP_V1
def bootstrap_prediction_intervals(var, full_df):
    model_list = ['model_1', 'model_2','model_3']
    model_type_list = ['xgb','lr']

    #######################actual results###################################
    #get baseline model prediction evr
    baseline_test_result = explained_variance_score(full_df[var], full_df[var+'_prediction'])
    baseline_test_aa = explained_variance_score(full_df[var][full_df.race_clean_AA==1],
                                                   full_df[var+'_prediction'][full_df.race_clean_AA==1])
    baseline_test_asa = explained_variance_score(full_df[var][full_df.race_clean_AsA==1],
                                                   full_df[var+'_prediction'][full_df.race_clean_AsA==1])
    baseline_test_ea = explained_variance_score(full_df[var][full_df.race_clean_EA==1],
                                                   full_df[var+'_prediction'][full_df.race_clean_EA==1])

    genetic_results = []
    full_results = []
    for j in model_type_list:
        for i in model_list:
            genetic_test_result_sample = explained_variance_score(full_df[var+'_residuals'], full_df[i+'_'+var+'_'+j+'_predictions'])
            genetic_test_result_aa = explained_variance_score(full_df[var+'_residuals'][full_df.race_clean_AA==1],
                                                       full_df[i+'_'+var+'_'+j+'_predictions'][full_df.race_clean_AA==1])
            genetic_test_result_asa = explained_variance_score(full_df[var+'_residuals'][full_df.race_clean_AsA==1],
                                                       full_df[i+'_'+var+'_'+j+'_predictions'][full_df.race_clean_AsA==1])
            genetic_test_result_ea = explained_variance_score(full_df[var+'_residuals'][full_df.race_clean_EA==1],
                                                       full_df[i+'_'+var+'_'+j+'_predictions'][full_df.race_clean_EA==1])
            genetic_results = genetic_results+[genetic_test_result_sample, genetic_test_result_aa, genetic_test_result_asa,
                                               genetic_test_result_ea]
            full_test_result_sample = explained_variance_score(full_df[var], full_df[i+'_'+var+'_'+j+'_predictions']+full_df[var+'_prediction'])
            full_result_aa = explained_variance_score(full_df[var][full_df.race_clean_AA==1],
                                                       (full_df[i+'_'+var+'_'+j+'_predictions']+full_df[var+'_prediction'])[full_df.race_clean_AA==1])
            full_result_asa = explained_variance_score(full_df[var][full_df.race_clean_AsA==1],
                                                       (full_df[i+'_'+var+'_'+j+'_predictions']+full_df[var+'_prediction'])[full_df.race_clean_AsA==1])
            full_result_ea = explained_variance_score(full_df[var][full_df.race_clean_EA==1],
                                                       (full_df[i+'_'+var+'_'+j+'_predictions']+full_df[var+'_prediction'])[full_df.race_clean_EA==1])
            full_results = full_results+[full_test_result_sample, full_result_aa, full_result_asa,
                                         full_result_ea]

    #######################actual results###################################
    actual_results_list = pd.Series([baseline_test_result, baseline_test_aa, baseline_test_asa,
                           baseline_test_ea]+genetic_results+full_results, dtype = 'float64')

    baseline_evr_list = pd.Series([], dtype = 'float64')
    baseline_evr_list_aa = pd.Series([], dtype = 'float64')
    baseline_evr_list_asa = pd.Series([], dtype = 'float64')
    baseline_evr_list_ea = pd.Series([], dtype = 'float64')

    #xgb model 1 lists
    xgb_model_1_genetic_evr_list = pd.Series([], dtype = 'float64',)
    xgb_model_1_genetic_evr_list_aa = pd.Series([], dtype = 'float64')
    xgb_model_1_genetic_evr_list_asa = pd.Series([], dtype = 'float64')
    xgb_model_1_genetic_evr_list_ea = pd.Series([], dtype = 'float64')

    xgb_model_1_full_prediction_evr_list = pd.Series([], dtype = 'float64', name = 'xgb model 1 full evr')
    xgb_model_1_full_prediction_evr_list_aa = pd.Series([], dtype = 'float64', name = 'xgb model 1 full evr AA')
    xgb_model_1_full_prediction_evr_list_asa = pd.Series([], dtype = 'float64', name = 'xgb model 1 full evr Asa')
    xgb_model_1_full_prediction_evr_list_ea = pd.Series([], dtype = 'float64', name = 'xgb model 1 full evr EA')

    #xgb model 2 lists
    xgb_model_2_genetic_evr_list = pd.Series([], dtype = 'float64')
    xgb_model_2_genetic_evr_list_aa = pd.Series([], dtype = 'float64')
    xgb_model_2_genetic_evr_list_asa = pd.Series([], dtype = 'float64')
    xgb_model_2_genetic_evr_list_ea = pd.Series([], dtype = 'float64')

    xgb_model_2_full_prediction_evr_list = pd.Series([], dtype = 'float64')
    xgb_model_2_full_prediction_evr_list_aa = pd.Series([], dtype = 'float64')
    xgb_model_2_full_prediction_evr_list_asa = pd.Series([], dtype = 'float64')
    xgb_model_2_full_prediction_evr_list_ea = pd.Series([], dtype = 'float64')

    #xgb model 3 lists
    xgb_model_3_genetic_evr_list = pd.Series([], dtype = 'float64')
    xgb_model_3_genetic_evr_list_aa = pd.Series([], dtype = 'float64')
    xgb_model_3_genetic_evr_list_asa = pd.Series([], dtype = 'float64')
    xgb_model_3_genetic_evr_list_ea = pd.Series([], dtype = 'float64')

    xgb_model_3_full_prediction_evr_list = pd.Series([], dtype = 'float64')
    xgb_model_3_full_prediction_evr_list_aa = pd.Series([], dtype = 'float64')
    xgb_model_3_full_prediction_evr_list_asa = pd.Series([], dtype = 'float64')
    xgb_model_3_full_prediction_evr_list_ea = pd.Series([], dtype = 'float64')

    #LINEAR REGRESSION
    #linear regression model 1 lists
    lr_model_1_genetic_evr_list = pd.Series([], dtype = 'float64')
    lr_model_1_genetic_evr_list_aa = pd.Series([], dtype = 'float64')
    lr_model_1_genetic_evr_list_asa = pd.Series([], dtype = 'float64')
    lr_model_1_genetic_evr_list_ea = pd.Series([], dtype = 'float64')

    lr_model_1_full_prediction_evr_list = pd.Series([], dtype = 'float64')
    lr_model_1_full_prediction_evr_list_aa = pd.Series([], dtype = 'float64')
    lr_model_1_full_prediction_evr_list_asa = pd.Series([], dtype = 'float64')
    lr_model_1_full_prediction_evr_list_ea = pd.Series([], dtype = 'float64')

    #linear model 2 lists
    lr_model_2_genetic_evr_list = pd.Series([], dtype = 'float64')
    lr_model_2_genetic_evr_list_aa = pd.Series([], dtype = 'float64')
    lr_model_2_genetic_evr_list_asa = pd.Series([], dtype = 'float64')
    lr_model_2_genetic_evr_list_ea = pd.Series([], dtype = 'float64')

    lr_model_2_full_prediction_evr_list = pd.Series([], dtype = 'float64')
    lr_model_2_full_prediction_evr_list_aa = pd.Series([], dtype = 'float64')
    lr_model_2_full_prediction_evr_list_asa = pd.Series([], dtype = 'float64')
    lr_model_2_full_prediction_evr_list_ea = pd.Series([], dtype = 'float64')

    #linear model 3 lists
    lr_model_3_genetic_evr_list = pd.Series([], dtype = 'float64')
    lr_model_3_genetic_evr_list_aa = pd.Series([], dtype = 'float64')
    lr_model_3_genetic_evr_list_asa = pd.Series([], dtype = 'float64')
    lr_model_3_genetic_evr_list_ea = pd.Series([], dtype = 'float64')

    lr_model_3_full_prediction_evr_list = pd.Series([], dtype = 'float64')
    lr_model_3_full_prediction_evr_list_aa = pd.Series([], dtype = 'float64')
    lr_model_3_full_prediction_evr_list_asa = pd.Series([], dtype = 'float64')
    lr_model_3_full_prediction_evr_list_ea = pd.Series([], dtype = 'float64')

    for i in range(99):
        #sample with replacement from the test set with the same size
        full_df_sample = full_df.sample(n=len(full_df), replace=True)

        #get baseline model prediction evr
        baseline_test_result_sample = explained_variance_score(full_df_sample[var], full_df_sample[var+'_prediction'])
        baseline_test_result_aa = explained_variance_score(full_df_sample[var][full_df_sample.race_clean_AA==1],
                                                       full_df_sample[var+'_prediction'][full_df_sample.race_clean_AA==1])
        baseline_test_result_asa = explained_variance_score(full_df_sample[var][full_df_sample.race_clean_AsA==1],
                                                       full_df_sample[var+'_prediction'][full_df_sample.race_clean_AsA==1])
        baseline_test_result_ea = explained_variance_score(full_df_sample[var][full_df_sample.race_clean_EA==1],
                                                       full_df_sample[var+'_prediction'][full_df_sample.race_clean_EA==1])
        baseline_evr_list = baseline_evr_list.append(pd.Series([baseline_test_result_sample]))
        baseline_evr_list_aa = baseline_evr_list_aa.append(pd.Series([baseline_test_result_aa]))
        baseline_evr_list_asa = baseline_evr_list_asa.append(pd.Series([baseline_test_result_asa]))
        baseline_evr_list_ea = baseline_evr_list_ea.append(pd.Series([baseline_test_result_ea]))

        for j in model_type_list:
            for i in model_list:
                genetic_test_result_sample = explained_variance_score(full_df_sample[var+'_residuals'], full_df_sample[i+'_'+var+'_'+j+'_predictions'])
                full_test_result_sample = explained_variance_score(full_df_sample[var], full_df_sample[i+'_'+var+'_'+j+'_predictions']+full_df_sample[var+'_prediction'])

                #AA
                genetic_test_result_aa = explained_variance_score(full_df_sample[var+'_residuals'][full_df_sample.race_clean_AA==1],
                                                           full_df_sample[i+'_'+var+'_'+j+'_predictions'][full_df_sample.race_clean_AA==1])
                full_result_aa = explained_variance_score(full_df_sample[var][full_df_sample.race_clean_AA==1],
                                                           (full_df_sample[i+'_'+var+'_'+j+'_predictions']+full_df_sample[var+'_prediction'])[full_df_sample.race_clean_AA==1])

                #AsA
                genetic_test_result_asa = explained_variance_score(full_df_sample[var+'_residuals'][full_df_sample.race_clean_AsA==1],
                                                           full_df_sample[i+'_'+var+'_'+j+'_predictions'][full_df_sample.race_clean_AsA==1])
                full_result_asa = explained_variance_score(full_df_sample[var][full_df_sample.race_clean_AsA==1],
                                                           (full_df_sample[i+'_'+var+'_'+j+'_predictions']+full_df_sample[var+'_prediction'])[full_df_sample.race_clean_AsA==1])

                #EA
                genetic_test_result_ea = explained_variance_score(full_df_sample[var+'_residuals'][full_df_sample.race_clean_EA==1],
                                                           full_df_sample[i+'_'+var+'_'+j+'_predictions'][full_df_sample.race_clean_EA==1])
                full_result_ea = explained_variance_score(full_df_sample[var][full_df_sample.race_clean_EA==1],
                                                           (full_df_sample[i+'_'+var+'_'+j+'_predictions']+full_df_sample[var+'_prediction'])[full_df_sample.race_clean_EA==1])

                if j =='xgb':
                    if i == 'model_1':
                        xgb_model_1_genetic_evr_list= xgb_model_1_genetic_evr_list.append(pd.Series([genetic_test_result_sample]))
                        xgb_model_1_genetic_evr_list_aa = xgb_model_1_genetic_evr_list_aa.append(pd.Series([genetic_test_result_aa]))
                        xgb_model_1_genetic_evr_list_asa = xgb_model_1_genetic_evr_list_asa.append(pd.Series([genetic_test_result_asa]))
                        xgb_model_1_genetic_evr_list_ea = xgb_model_1_genetic_evr_list_ea.append(pd.Series([genetic_test_result_ea]))

                        xgb_model_1_full_prediction_evr_list = xgb_model_1_full_prediction_evr_list.append(pd.Series([full_test_result_sample]))
                        xgb_model_1_full_prediction_evr_list_aa = xgb_model_1_full_prediction_evr_list_aa.append(pd.Series([full_result_aa]))
                        xgb_model_1_full_prediction_evr_list_asa = xgb_model_1_full_prediction_evr_list_asa.append(pd.Series([full_result_asa]))
                        xgb_model_1_full_prediction_evr_list_ea = xgb_model_1_full_prediction_evr_list_ea.append(pd.Series([full_result_ea]))

                    elif i == 'model_2':
                        xgb_model_2_genetic_evr_list = xgb_model_2_genetic_evr_list.append(pd.Series([genetic_test_result_sample]))
                        xgb_model_2_genetic_evr_list_aa= xgb_model_2_genetic_evr_list_aa.append(pd.Series([genetic_test_result_aa]))
                        xgb_model_2_genetic_evr_list_asa= xgb_model_2_genetic_evr_list_asa.append(pd.Series([genetic_test_result_asa]))
                        xgb_model_2_genetic_evr_list_ea= xgb_model_2_genetic_evr_list_ea.append(pd.Series([genetic_test_result_ea]))

                        xgb_model_2_full_prediction_evr_list= xgb_model_2_full_prediction_evr_list.append(pd.Series([full_test_result_sample]))
                        xgb_model_2_full_prediction_evr_list_aa= xgb_model_2_full_prediction_evr_list_aa.append(pd.Series([full_result_aa]))
                        xgb_model_2_full_prediction_evr_list_asa = xgb_model_2_full_prediction_evr_list_asa.append(pd.Series([full_result_asa]))
                        xgb_model_2_full_prediction_evr_list_ea= xgb_model_2_full_prediction_evr_list_ea.append(pd.Series([full_result_ea]))

                    else: #model_3
                        xgb_model_3_genetic_evr_list = xgb_model_3_genetic_evr_list.append(pd.Series([genetic_test_result_sample]))
                        xgb_model_3_genetic_evr_list_aa =xgb_model_3_genetic_evr_list_aa.append(pd.Series([genetic_test_result_aa]))
                        xgb_model_3_genetic_evr_list_asa= xgb_model_3_genetic_evr_list_asa.append(pd.Series([genetic_test_result_asa]))
                        xgb_model_3_genetic_evr_list_ea =  xgb_model_3_genetic_evr_list_ea.append(pd.Series([genetic_test_result_ea]))

                        xgb_model_3_full_prediction_evr_list = xgb_model_3_full_prediction_evr_list.append(pd.Series([full_test_result_sample]))
                        xgb_model_3_full_prediction_evr_list_aa = xgb_model_3_full_prediction_evr_list_aa.append(pd.Series([full_result_aa]))
                        xgb_model_3_full_prediction_evr_list_asa = xgb_model_3_full_prediction_evr_list_asa.append(pd.Series([full_result_asa]))
                        xgb_model_3_full_prediction_evr_list_ea = xgb_model_3_full_prediction_evr_list_ea.append(pd.Series([full_result_ea]))
                else:
                    if i == 'model_1':
                        lr_model_1_genetic_evr_list= lr_model_1_genetic_evr_list.append(pd.Series([genetic_test_result_sample]))
                        lr_model_1_genetic_evr_list_aa= lr_model_1_genetic_evr_list_aa.append(pd.Series([genetic_test_result_aa]))
                        lr_model_1_genetic_evr_list_asa= lr_model_1_genetic_evr_list_asa.append(pd.Series([genetic_test_result_asa]))
                        lr_model_1_genetic_evr_list_ea = lr_model_1_genetic_evr_list_ea.append(pd.Series([genetic_test_result_ea]))

                        lr_model_1_full_prediction_evr_list = lr_model_1_full_prediction_evr_list.append(pd.Series([full_test_result_sample]))
                        lr_model_1_full_prediction_evr_list_aa= lr_model_1_full_prediction_evr_list_aa.append(pd.Series([full_result_aa]))
                        lr_model_1_full_prediction_evr_list_asa = lr_model_1_full_prediction_evr_list_asa.append(pd.Series([full_result_asa]))
                        lr_model_1_full_prediction_evr_list_ea= lr_model_1_full_prediction_evr_list_ea.append(pd.Series([full_result_ea]))

                    elif i == 'model_2':
                        lr_model_2_genetic_evr_list = lr_model_2_genetic_evr_list.append(pd.Series([genetic_test_result_sample]))
                        lr_model_2_genetic_evr_list_aa = lr_model_2_genetic_evr_list_aa.append(pd.Series([genetic_test_result_aa]))
                        lr_model_2_genetic_evr_list_asa = lr_model_2_genetic_evr_list_asa.append(pd.Series([genetic_test_result_asa]))
                        lr_model_2_genetic_evr_list_ea = lr_model_2_genetic_evr_list_ea.append(pd.Series([genetic_test_result_ea]))

                        lr_model_2_full_prediction_evr_list = lr_model_2_full_prediction_evr_list.append(pd.Series([full_test_result_sample]))
                        lr_model_2_full_prediction_evr_list_aa = lr_model_2_full_prediction_evr_list_aa.append(pd.Series([full_result_aa]))
                        lr_model_2_full_prediction_evr_list_asa = lr_model_2_full_prediction_evr_list_asa.append(pd.Series([full_result_asa]))
                        lr_model_2_full_prediction_evr_list_ea= lr_model_2_full_prediction_evr_list_ea.append(pd.Series([full_result_ea]))

                    else: #model_3
                        lr_model_3_genetic_evr_list= lr_model_3_genetic_evr_list.append(pd.Series([genetic_test_result_sample]))
                        lr_model_3_genetic_evr_list_aa = lr_model_3_genetic_evr_list_aa.append(pd.Series([genetic_test_result_aa]))
                        lr_model_3_genetic_evr_list_asa = lr_model_3_genetic_evr_list_asa.append(pd.Series([genetic_test_result_asa]))
                        lr_model_3_genetic_evr_list_ea = lr_model_3_genetic_evr_list_ea.append(pd.Series([genetic_test_result_ea]))

                        lr_model_3_full_prediction_evr_list= lr_model_3_full_prediction_evr_list.append(pd.Series([full_test_result_sample]))
                        lr_model_3_full_prediction_evr_list_aa = lr_model_3_full_prediction_evr_list_aa.append(pd.Series([full_result_aa]))
                        lr_model_3_full_prediction_evr_list_asa = lr_model_3_full_prediction_evr_list_asa.append(pd.Series([full_result_asa]))
                        lr_model_3_full_prediction_evr_list_ea= lr_model_3_full_prediction_evr_list_ea.append(pd.Series([full_result_ea]))

    #combine all evr_lists together, and get 2.5th and 97.5th percentile
    prediction_intervals = pd.concat(
        [
            baseline_evr_list, baseline_evr_list_aa, baseline_evr_list_asa, baseline_evr_list_ea,
            xgb_model_1_genetic_evr_list, xgb_model_1_genetic_evr_list_aa, xgb_model_1_genetic_evr_list_asa, xgb_model_1_genetic_evr_list_ea,
            xgb_model_2_genetic_evr_list, xgb_model_2_genetic_evr_list_aa, xgb_model_2_genetic_evr_list_asa, xgb_model_2_genetic_evr_list_ea,
            xgb_model_3_genetic_evr_list, xgb_model_3_genetic_evr_list_aa, xgb_model_3_genetic_evr_list_asa, xgb_model_3_genetic_evr_list_ea,
            lr_model_1_genetic_evr_list, lr_model_1_genetic_evr_list_aa, lr_model_1_genetic_evr_list_asa, lr_model_1_genetic_evr_list_ea,
            lr_model_2_genetic_evr_list, lr_model_2_genetic_evr_list_aa, lr_model_2_genetic_evr_list_asa, lr_model_2_genetic_evr_list_ea,
            lr_model_3_genetic_evr_list, lr_model_3_genetic_evr_list_aa, lr_model_3_genetic_evr_list_asa, lr_model_3_genetic_evr_list_ea,
            xgb_model_1_full_prediction_evr_list, xgb_model_1_full_prediction_evr_list_aa, xgb_model_1_full_prediction_evr_list_asa, xgb_model_1_full_prediction_evr_list_ea,
            xgb_model_2_full_prediction_evr_list, xgb_model_2_full_prediction_evr_list_aa, xgb_model_2_full_prediction_evr_list_asa, xgb_model_2_full_prediction_evr_list_ea,
            xgb_model_3_full_prediction_evr_list, xgb_model_3_full_prediction_evr_list_aa, xgb_model_3_full_prediction_evr_list_asa, xgb_model_3_full_prediction_evr_list_ea,
            lr_model_1_full_prediction_evr_list, lr_model_1_full_prediction_evr_list_aa, lr_model_1_full_prediction_evr_list_asa, lr_model_1_full_prediction_evr_list_ea,
            lr_model_2_full_prediction_evr_list, lr_model_2_full_prediction_evr_list_aa, lr_model_2_full_prediction_evr_list_asa, lr_model_2_full_prediction_evr_list_ea,
            lr_model_3_full_prediction_evr_list, lr_model_3_full_prediction_evr_list_aa, lr_model_3_full_prediction_evr_list_asa, lr_model_3_full_prediction_evr_list_ea
        ], axis = 1).quantile([0.025,0.975])
    prediction_intervals.columns = \
    [
        'baseline evr','baseline evr AA','baseline evr Asa','baseline evr EA',
        'xgb model 1 genetic evr','xgb model 1 genetic evr AA','xgb model 1 genetic evr Asa', 'xgb model 1 genetic evr EA',
        'xgb model 2 genetic evr','xgb model 2 genetic evr AA','xgb model 2 genetic evr Asa', 'xgb model 2 genetic evr EA',
        'xgb model 3 genetic evr','xgb model 3 genetic evr AA','xgb model 3 genetic evr Asa', 'xgb model 3 genetic evr EA',
        'lr model 1 genetic evr','lr model 1 genetic evr AA','lr model 1 genetic evr Asa', 'lr model 1 genetic evr EA',
        'lr model 2 genetic evr','lr model 2 genetic evr AA','lr model 2 genetic evr Asa', 'lr model 2 genetic evr EA',
        'lr model 3 genetic evr','lr model 3 genetic evr AA','lr model 3 genetic evr Asa', 'lr model 3 genetic evr EA',
        'xgb model 1 full prediction evr','xgb model 1 full prediction evr AA','xgb model 1 full prediction evr Asa', 'xgb model 1 full prediction evr EA',
        'xgb model 2 full prediction evr','xgb model 2 full prediction evr AA','xgb model 2 full prediction evr Asa', 'xgb model 2 full prediction evr EA',
        'xgb model 3 full prediction evr','xgb model 3 full prediction evr AA','xgb model 3 full prediction evr Asa', 'xgb model 3 full prediction evr EA',
        'lr model 1 full prediction evr','lr model 1 full prediction evr AA','lr model 1 full prediction evr Asa', 'lr model 1 full prediction evr EA',
        'lr model 2 full prediction evr','lr model 2 full prediction evr AA','lr model 2 full prediction evr Asa', 'lr model 2 full prediction evr EA',
        'lr model 3 full prediction evr','lr model 3 full prediction evr AA','lr model 3 full prediction evr Asa', 'lr model 3 full prediction evr EA'
    ]
    predictions_df = prediction_intervals.T
    predictions_df['EVR'] = actual_results_list.set_axis(predictions_df.index)
    return(predictions_df)
