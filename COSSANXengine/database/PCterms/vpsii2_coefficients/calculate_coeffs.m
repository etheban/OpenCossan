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
Nrvs   = 15;
Norder = 5;

for i=1:Nrvs
    for j=3:Norder
        Vpsii2 = psi2eval(i,j);
        eval(['save vpsii2_coeffs_' num2str(i) '_' num2str(j) ' Vpsii2' ]);
        disp(['Calculation for RV no ' num2str(i) ' - Order: ' num2str(j) ' completed ']);
    end
end
