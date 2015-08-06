%% Written by HMP 3-Oct-2009
%
% this is to create a database of vijk coefficients
%
% 
clear variables;

%% calculate & store the coefficients

% notation followed to store the coeffs is
%
% => vijk_coeffs_Nrvs_Norder

% define calculation range
Nrvs   = 20;
Norder = 5;

for i=11:Nrvs
    for j=3:Norder
        [Vcijk_i,Vcijk_j,Vcijk_k,Vcijk] = cijknz(i,j); 
        eval(['save vcijk_coeffs_' num2str(i) '_' num2str(j) ' Vcijk_i Vcijk_j Vcijk_k Vcijk' ]);
        disp(['Calculation for RV no ' num2str(i) ' - Order: ' num2str(j) ' completed ']);
    end
end
