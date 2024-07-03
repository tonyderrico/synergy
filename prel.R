str(syn_all_occ_hist)
names(syn_all_subjects)

syn_all_subjects$diagyr_dic <- ifelse(is.na(syn_all_subjects$diagyr_dic), 0, 1)
table(syn_all_subjects$diagyr_dic, syn_all_subjects$farmer)


