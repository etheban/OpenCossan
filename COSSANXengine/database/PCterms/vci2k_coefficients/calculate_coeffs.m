%% Written by HMP 3-Oct-2009
%
% this is to create a database of vijk coefficients
%
% 
clear all

%% calculate & store the coefficients

% notation followed to store the coeffs is
%
% => vijk_coeffs_Nrvs_Norder

% define calculation range
Nrvs   = 10;
Norder = 5;

for i=1:Nrvs
    for j=5:Norder
        [Vci2k_i,Vci2k_k,Vci2k] = ci2knz(i,j); 
        eval(['save vci2k_coeffs_' num2str(i) '_' num2str(j) ' Vci2k_i Vci2k_k Vci2k' ]);
        disp(['Calculation for RV no ' num2str(i) ' - Order: ' num2str(j) ' completed ']);
    end
end
