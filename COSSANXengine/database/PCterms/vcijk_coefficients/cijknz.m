function [iv jv kv cijk] = cijknz(M,p)
% CIJKEVAL   generates the coefficients Cijk = <XI_I PSI_J PSI_K>
%
%   MANDATORY ARGUMENTS:
%
%
%   OPTIONAL ARGUMENTS:
%
%   OUTPUT:
%
%   EXAMPLE:
%
% =========================================================================
% COSSAN-X - The next generation of the computational stochastic analysis
% University of Innsbruck, Copyright 1993-2011 IfM
% =========================================================================

% Generate the index sequences for the multidimensional Hermite polynomials
[alpha nalpha] = getindex_incl(M,p);

% Evaluate cijk for i=1 (corresponds to psi_1 = 1, shorter evaluation)
[jv,kv,cijk] = find(diag(prod(factorial(alpha),2)));
iv           = ones(size(jv));

% Evaluate rest of the cijk's; symmetry is not yet exploited; doing so will also require to
% change the dimensioning of the array cijk!

for i=2:(M+1),
    %OpenCossan.cossanDisp(['i = ' num2str(i)]);
    % Find which xi_i is referred to by the psi_i
    ixi=find(alpha(i,:));
    aux_ind     = setdiff(1:size(alpha,2),ixi);
    
    for j=1:nalpha,
        p   = alpha(j,ixi);
        q   = alpha(1+j-1:nalpha,ixi);
        ortchk  = repmat(alpha(j,aux_ind),nalpha-j+1,1) - alpha(1+j-1:nalpha,aux_ind);
        ind     = ~any(ortchk,2);
        alpha2  = repmat(alpha(j,:),nalpha-j+1,1); 
        alpha2(:,ixi)   = 1;
        pos_alpha   = (1+j-1:nalpha)';
        kv_temp     = pos_alpha(ind);
        try
        cijk_temp   = prod(factorial(alpha2(ind,:)),2).*(factorial(p)* ((p+1)==(q(ind)+2)) ...
                              + factorial(q(ind)).* ((p+2)==(q(ind)+1)) );
        ind     = find(cijk_temp~=0);
        cijk    = [cijk;cijk_temp(ind)];
        kv      = [kv;kv_temp(ind)];
        catch ME
            continue
        end
        jv  = [jv;j*ones(length(kv)-length(jv),1)];
    end
    iv  = [iv;i*ones(length(jv)-length(iv),1)];
end


return