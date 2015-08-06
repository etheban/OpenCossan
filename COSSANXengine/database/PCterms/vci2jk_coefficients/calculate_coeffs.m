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
        [Vci2jk_i,Vci2jk_j,Vci2jk_k,Vci2jk] = ci2jknz(i,j); 
        eval(['save vci2jk_coeffs_' num2str(i) '_' num2str(j) ' Vci2jk_i Vci2jk_j Vci2jk_k Vci2jk' ]);
        disp(['Calculation for RV no ' num2str(i) ' - Order: ' num2str(j) ' completed ']);
    end
end
