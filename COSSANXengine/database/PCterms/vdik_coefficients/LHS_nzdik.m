function [vi vk vnzdik] = LHS_nzdik(Nrvs,PC_order1,PC_order2)
%MCS_NZDIK calculates the non-zero vcik coefficients which are needed to
%          assemble the RHS of PC equation in case of random force 
%          NOTE: they are calculated using DMCS

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
Hk = pceval(xi,PC_order2);

%% Calculate dijk coefficients

vdik = zeros(Npcterms1,Npcterms2);
for k = 1:Npcterms2
    vdik(:,k) = (Hi'*(Hk(:,k)))'/Nsims;
end

% round the coefficients which are close to zero to zero 
vdik = round(vdik);
% Get rid of the negative ones
neg_index = find(vdik<0);
vcik(neg_index) = 0;

%%  Obtain indices of nonzero elements

[vi vk vnzdik] = find(vdik);

return