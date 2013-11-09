# runit test BHY
test.SR_adj_multests <- function() {
  sm_free = 3 # test data p_value_full.csv is annual
    #%%% 'sm_fre': Sampling frequency; [1,2,3,4,5] = [Daily, Weekly, Monthly, Quarterly, Annual];
  num_obs = 120
    #%%% 'num_obs': No. of observations in the frequency specified in the previous step;

  SR = 1
    #%%% 'SR': Sharpe ratio; either annualized or in the frequency specified in the previous step;
  ind_an = 1
    #%%% 'ind_an': Indicator; if annulized, 'ind_an' = 1; otherwise = 0; 
  ind_aut = 0
    #%%% 'ind_aut': Indicator; if adjusted for autocorrelations, 'ind_aut' = 1; otherwise = 0;
  
  rho = 0.1
    #%%% 'rho': Autocorrelation coefficient at the specified frequency ;
  
  num_test = 100
    #%%% 'num_test': Number of tests allowed, Harvey, Liu and Zhu (2013) find 314 factors
  
  res <- checkEquals(SR_adj_multests(sm_fre, num_obs, SR, ind_an, ind_aut, rho, num_test), NULL) # this will fail
}
