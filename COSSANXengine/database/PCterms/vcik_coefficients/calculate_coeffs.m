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
Nrvs   = 50;
Norder = 5;

for i=31:Nrvs
    for j=3:Norder
        [Vcik_i,Vcik_k,Vcik] = ciknz(i,j); 
        eval(['save vcik_coeffs_' num2str(i) '_' num2str(j) ' Vcik_i Vcik_k Vcik' ]);
        disp(['Calculation for RV no ' num2str(i) ' - Order: ' num2str(j) ' completed ']);
    end
end
