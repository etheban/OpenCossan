function [vi vj vk vnzdijk] = LHS_nzdijk(Nrvs,PC_order1,PC_order2)
%MCS_NZDIJK calculates the non-zero dijk coefficients which are needed if a
%           non-gaussian input is app. using P-C Exp and in addition to the P-C
%           Exp. of the response. The coefficients are calculated using MCS
%           with 1,000,000 samples (this no was enough to obtain the coefficients correctly)
%   
% Usage:  [vi vj vk vnzdijk] = MCS_nzdijk(3,2,4)  
%
%         3 = No of RV's in the problem (e.g. No of K-L terms)
%         2 = P-C Exp. order used for the input (e.g. Young's Modulus)
%         4 = P-C Exp. order used for the response (e.g. Displacement)

%% Fix the seed

randn('state', 0);

%% Enter no of samples & Generate the samples

Nsims = 200000;

for i=1:Nrvs
    xi(:,i)    = lhsnorm(0,1,Nsims);
end

%% Calculate the no of PC terms needed by both Expansions

Npcterms1 = pcnumber(Nrvs,PC_order1);
Npcterms2 = pcnumber(Nrvs,PC_order2);

%% Evaluate the Hermite Polynomials for the sampled germs

Hi = pceval(xi,PC_order1); 
Hj = pceval(xi,PC_order2);
Hk = pceval(xi,PC_order2);

%% Calculate dijk coefficients

vdijk = zeros(Npcterms1,Npcterms2,Npcterms2);
for k = 1:Npcterms2
        vdijk(:,1:k,k) = (Hi'* ( Hj(:,1:k) .* repmat(Hk(:,k),1,k) )  ) / size(Hi,1);
end

% round the coefficients which are close to zero to zero 
vdijk = round(vdijk);
% Get rid of the negative ones
vdijk = max(vdijk,0);

%%  Obtain indices of nonzero elements

[vi vj vnzdijk] = find(vdijk);

aux1    = floor(vj/Npcterms2);
aux2    = rem(vj,Npcterms2);
c1      = (aux1 == 0);
c2      = (aux1>0 & aux2==0);
c3      = ~c1 & ~c2;
vk(c1)  = 1;    vk(c2)  = aux1(c2);     vk(c3)  = aux1(c3) + 1;
vj(c2)  =  Npcterms2;   vj(c3) = aux2(c3);
vk      = vk';

return