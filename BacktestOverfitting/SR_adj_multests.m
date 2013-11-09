%%% Sharpe ratio adjustment due to testing multiplicity ------ Harvey and Liu
%%% (2013): "Backtesting", Working Paper, Duke University 

function res = SR_adj_multests(sm_fre, num_obs, SR, ind_an, ind_aut, rho, num_test) 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Parameter inputs %%%%%%
 
%%% 'sm_fre': Sampling frequency; [1,2,3,4,5] = [Daily, Weekly, Monthly, Quarterly, Annual]; 
%%% 'num_obs': No. of observations in the frequency specified in the previous step; 
%%% 'SR': Sharpe ratio; either annualized or in the frequency specified in the previous step;
%%% 'ind_an': Indicator; if annulized, 'ind_an' = 1; otherwise = 0; 
%%% 'ind_aut': Indicator; if adjusted for autocorrelations, 'ind_aut' = 1; otherwise = 0;
%%% 'rho': Autocorrelation coefficient at the specified frequency ;
%%% 'num_test': Number of tests allowed, Harvey, Liu and Zhu (2013) find 314 factors.
 
%%% Calculating the equivalent annualized Sharpe ratio 'sr_annual', after 
%%% taking autocorrlation into account 

if ind_an == 1 && ind_aut == 1, 
    sr_annual = SR;
elseif ind_an ==1 && ind_aut == 0, 
    if sm_fre ==1, 
    sr_annual = SR*[1 + (2*rho/(1-rho))*(1- ((1-rho^(360))/(360*(1-rho))))]^(-0.5);
    elseif sm_fre ==2,
    sr_annual = SR*[1 + (2*rho/(1-rho))*(1- ((1-rho^(52))/(52*(1-rho))))]^(-0.5);
    elseif sm_fre ==3,
    sr_annual = SR*[1 + (2*rho/(1-rho))*(1- ((1-rho^(12))/(12*(1-rho))))]^(-0.5);
    elseif sm_fre ==4,
    sr_annual = SR*[1 + (2*rho/(1-rho))*(1- ((1-rho^(4))/(4*(1-rho))))]^(-0.5);
    elseif sm_fre ==5,
    sr_annual = SR; 
    end
elseif ind_an == 0 && ind_aut == 1,
     if sm_fre ==1, 
    sr_annual = SR*sqrt(360);
    elseif sm_fre ==2,
    sr_annual = SR*sqrt(52);
    elseif sm_fre ==3,
    sr_annual = SR*sqrt(12);
    elseif sm_fre ==4,
    sr_annual = SR*sqrt(4);
    elseif sm_fre ==5,
    sr_annual = SR; 
     end
 elseif ind_an == 0 && ind_aut == 0,
     if sm_fre ==1, 
    sr_annual = sqrt(360)*sr*[1 + (2*rho/(1-rho))*(1- ((1-rho^(360))/(360*(1-rho))))]^(-0.5);
    elseif sm_fre ==2,
    sr_annual = sqrt(52)*sr*[1 + (2*rho/(1-rho))*(1- ((1-rho^(52))/(52*(1-rho))))]^(-0.5);
    elseif sm_fre ==3,
    sr_annual = sqrt(12)*sr*[1 + (2*rho/(1-rho))*(1- ((1-rho^(12))/(12*(1-rho))))]^(-0.5);
    elseif sm_fre ==4,
    sr_annual = sqrt(4)*sr*[1 + (2*rho/(1-rho))*(1- ((1-rho^(4))/(4*(1-rho))))]^(-0.5);
    elseif sm_fre ==5,
    sr_annual = SR; 
     end 
end

%%% Number of monthly observations 'N' %%%

if sm_fre ==1, 
    N = floor(num_obs*12/360);
elseif sm_fre ==2,
    N = floor(num_obs*12/52);
elseif sm_fre == 3,
    N = floor(num_obs*12/12);
elseif sm_fre == 4,
    N = floor(num_obs*12/4);
elseif sm_fre == 5,
    N = floor(num_obs*12/1);
end

%%% Number of tests allowed %%%                              
M = num_test; 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% Intermediate outputs %%%%%%%%%%
fprintf('Intermediate Outputs:\n');
fprintf('Annualized Sharpe ratio = %.3f;\n', sr_annual);
fprintf('Based on %d monthly observations.\n\n', N);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Sharpe ratio adjustment %%%%%%%%%

m_vec = 1:(M+1);
c_const = sum(1./m_vec);

p_data = xlsread('p_value_full.xlsx', 'B2:B315');  % Data in Harvey, Liu and Zhu (2013)
p_large = p_data(p_data > 2.5); % Truncated data at 2.5
Lar_esti = length(p_large); 

% Parameter estimates in Harvey, Liu and Zhu (2013) for the log p-value
M_esti = 492;
mu = 0.90;
sigma = 0.66;

% Sharpe ratio, monthly 
sr = sr_annual/sqrt(12);
T = sr*sqrt(N);
p_val = 2*(1- tcdf(T, N-1));

% Drawing observations from the underlying p-value distribution; simulate a
% large number (WW) of p-value samples 
WW = 1000;
p_holm = ones(WW,1);
p_bhy = ones(WW,1);

for ww = 1: WW,
  % Drawing p-values proportionally from the p-value sample 
  KK_lar = floor(M*(Lar_esti/M_esti)) +1; 
  KK_small = M - KK_lar;
   
  xx = randsample(Lar_esti,KK_lar,'true');
  yy00 = normrnd(mu,sigma,[1000 1]);
  yy11 = exp(yy00);
  yy22 = yy11(yy11 < 2.5);
  yy =  [p_large(xx)', yy22(1:KK_small)']; 
  t_value = yy'; 
  
  p_val_sub= 2*(1- normcdf(t_value,0,1));
  
  %%% Holm %%%%
  p_val_all =  [p_val_sub', p_val];
  p_val_order = sort(p_val_all);
  p_holm_vec = [];
  for i = 1:(M+1),
      p_new = [];
      for j = 1:i,
          p_new = [p_new, (M+1-j+1)*p_val_order(j)];
      end
      p_holm_vec = [p_holm_vec, min(max(p_new),1)];
  end
  
  p_sub_holm = p_holm_vec(p_val_order == p_val);
  p_holm(ww) = p_sub_holm(1);
  
  %%%%% BHY %%%%%%%
  p_bhy_vec = [];

  for i = 1:(M+1),
      kk = (M+1) - (i-1);
      if kk == (M+1),
          p_new = p_val_order(end);
      else
          p_new = min( ((M+1)*c_const/kk)*p_val_order(kk), p_0);
      end    
      p_bhy_vec = [p_new, p_bhy_vec];
      p_0 = p_new;
  end
  
  p_sub_bhy = p_bhy_vec(p_val_order == p_val);
  p_bhy(ww) = p_sub_bhy(1);
  
end

%%% Bonferroni %%%
p_BON = min(M*p_val,1);
%%% Holm %%%
p_HOL = median(p_holm);
%%% BHY %%%
p_BHY = median(p_bhy);
%%% Average %%%
p_avg = (p_BON + p_HOL + p_BHY)/3;

% Invert to get z-score   
z_BON = tinv(1- p_BON/2, N-1);
z_HOL = tinv(1- p_HOL/2, N-1);
z_BHY = tinv(1- p_BHY/2, N-1);
z_avg = tinv(1- p_avg/2, N-1);

% Annualized Sharpe ratio
sr_BON = (z_BON/sqrt(N))*sqrt(12);
sr_HOL = (z_HOL/sqrt(N))*sqrt(12);
sr_BHY = (z_BHY/sqrt(N))*sqrt(12);
sr_avg = (z_avg/sqrt(N))*sqrt(12);

% Calculate haircut
hc_BON = (sr_annual - sr_BON)/sr_annual; 
hc_HOL = (sr_annual - sr_HOL)/sr_annual;
hc_BHY = (sr_annual - sr_BHY)/sr_annual;
hc_avg = (sr_annual - sr_avg)/sr_annual;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Final Output %%%%%%%%%%%
fprintf('Final Outputs:\n');
fprintf('Bonferroni adjustment:\n');
fprintf('Adjusted p-value = %.3f;\n', p_BON);
fprintf('Adjusted Sharpe ratio = %.3f;\n', sr_BON);
fprintf('Haircut = %.1f%%.\n\n', hc_BON*100);

fprintf('Holm adjustment:\n');
fprintf('Adjusted p-value = %.3f;\n', p_HOL);
fprintf('Adjusted Sharpe ratio = %.3f;\n', sr_HOL);
fprintf('Haircut = %.1f%%.\n\n', hc_HOL*100);

fprintf('BHY adjustment:\n');
fprintf('Adjusted p-value = %.3f;\n', p_BHY);
fprintf('Adjusted Sharpe ratio = %.3f;\n', sr_BHY);
fprintf('Haircut = %.1f%%.\n\n', hc_BHY*100);

fprintf('Average adjustment:\n');
fprintf('Adjusted p-value = %.3f;\n', p_avg);
fprintf('Adjusted Sharpe ratio = %.3f;\n', sr_avg);
fprintf('Haircut = %.1f%%.\n\n', hc_avg*100);

