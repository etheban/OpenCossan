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
Ninputorder = 2;
Noutputorder = 2;

for i=1:Nrvs
    for j=1:Ninputorder
        for k=1:Noutputorder
            [Vi,Vk,Vdik] = LHS_nzdik(i,j,k); 
            eval(['save vdik_coeffs_' num2str(i) '_' num2str(j) '_' num2str(k) ' Vi Vk Vdik' ]);
            OpenCossan.cossanDisp(['Calculation for RV no ' num2str(i) ' - Input Order: ' num2str(j) ' - Output Order: ' num2str(k) ' completed ']);
        end
    end
end
