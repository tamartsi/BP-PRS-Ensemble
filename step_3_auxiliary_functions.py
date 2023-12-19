#######################################################################################################
## Define function to Pull PRS Results ##
#######################################################################################################

def load_prsice(var, phenotype_data_train,phenotype_data_test, gwas, single_threshold = False,
                prs_dir = '/2022_BP_ensemble/TOPMed_data/',
                prs_export_dir = '/2022_BP_ensemble/TOPMed_data/Global PRS Prepared/',
                ukbb_flag = False):

    # Load PRSice
    if gwas=="BBJ":
        fname = prs_dir + '/'+str(gwas)+"/PRS/"+"2020-06-05_"+str(var)+'_hg38_v2_MAF0.1_500kb.all_score'
    elif (gwas =='MVP') or (gwas == 'UKBB+ICBP'):
        fname = prs_dir + '/' + str(gwas)+"/PRS/"+str(gwas)+"_2020-06-05_"+str(var)+'_hg38_v2_MAF0.1_500kb.all_score'
    elif(gwas=="PRS-CsX"):
        fname =  '/2022_BP_ensemble/Data/TOPMed_freeze8_BP_PRS_w_PRS_CSx/20230915_BP_full_data_with_PRS-CSx_PSs.txt'
    elif(gwas=='Local PRS UKBB'):
        fname = '/2022_BP_ensemble/Local_PRS_files/SPLIT_GWAS/'+var+'_full_sub_prs_df.csv'
    else: #BP-ICE_EUR or BP-ICE_PA and var== SBP or DBP
        if var=="HTN":
            fname = prs_dir + 'rare_variants/PRS/'+str(gwas)+'_'+str(var)+"_15-04-2020_hg38_MAF0.1_500kb_v2.all_score"
        else:
            fname = prs_dir + 'rare_variants/PRS/'+str(gwas)+'_'+str(var)+"_transformed_15-04-2020_hg38_ICBPm_MAF0.1_500kb_v2.all_score"

    if gwas == 'Local PRS UKBB':
        prs = pd.read_csv(fname)
    else:
        prs = pd.read_csv(fname, sep = '\s+')

    # Set index
    if gwas == 'Local PRS UKBB':
        prs.index=prs.IID
        prs.drop(['IID'], axis=1, inplace=True)
    elif gwas != "PRS-CsX":
        prs.index=prs.IID
        prs.drop(['FID','IID'], axis=1, inplace=True)
    else:
        prs.set_index("sample.id", inplace = True)
        prs = prs[['SBP_AFR_std','SBP_EAS_std', 'SBP_EUR_std', 'DBP_AFR_std', 'DBP_EAS_std',
                   'DBP_EUR_std', 'SBP_std_sum', 'DBP_std_sum', 'SBP_PS_std_sum_std','DBP_PS_std_sum_std']]

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

    prs_train.to_csv(prs_export_dir+'TOPMED_'+str(var)+"_"+str(gwas)+"_prs_train.csv")
    prs_test.to_csv(prs_export_dir+'TOPMED_'+str(var)+"_"+str(gwas)+"_prs_test.csv")

    return prs_train, prs_test
