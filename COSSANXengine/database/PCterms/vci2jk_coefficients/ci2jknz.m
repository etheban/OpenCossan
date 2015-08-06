function [iv jv kv c2ijk] = c2ijkeval_modified(M,p)
% CIJKEVAL   generates the coefficients Cijk = <XI_I PSI_J PSI_K>
%    CIJK = CIJKEVAL(M,p) returns the 3-dimensional array Cijk. 
%    CIJK contains the coefficients Cijk corresponding to a P-C basis
%    with dimension M and order p.
%
%    Example: 
%    Invoking CIJK = CIJKEVAL(3,3) returns the 3-D array CIJK;
%    e.g. CIJK(2,3,5) = < XI_2 PSI_3 PSI_5 >
%
%    NOTE: The size of the array CIJK is [M+1,P,P], where M is the input
%    number of XI's, and P is the number of P-C basis vectors.
%    To inspect the symbolic expressions associated with each PSI_J (or PSI_K),
%    use PCSYM.
%
%    NOTE2: CIJKEVAL does not take advantage of the sparsity of CIJK, all
%    zeros are stored.
%
%    See also: CIJKPRINT
%
%    Manuel Pellissetti, October 18, 2002
%    Version 1.3
%

% Generate the index sequences for the multidimensional Hermite polynomials
[alpha nalpha] = getindex_incl(M,p);

% Evaluate rest of the cijk's; symmetry is not yet exploited; doing so will also require to
% change the dimensioning of the array cijk!
iv  = [];
jv  = [];
kv  = [];
c2ijk   = [];
for i=2:(M+1),
    %OpenCossan.cossanDisp(['i = ' num2str(i)]);
    % Find which xi_i is referred to by the psi_i
    ixi=find(alpha(i,:));
    aux_ind     = setdiff(1:size(alpha,2),ixi);
    
    for j=1:nalpha,
        ortchk  = repmat(alpha(j,aux_ind),nalpha-j+1,1) - alpha(1+j-1:nalpha,aux_ind);
        ind     = ~any(ortchk,2);
        if isscalar(ind) && ~ind,
            continue;
        end
        alpha2  = repmat(alpha(j,:),nalpha-j+1,1); 
        alpha2(:,ixi)   = 1;
        pos_alpha   = (1+j-1:nalpha)';
        
        p   = alpha(j,ixi);
        q   = alpha(1+j-1:nalpha,ixi);
        q   = q(ind,:);
        
        kv_temp     = pos_alpha(ind);
        aux1        = factorial(p) .* (p==q);
        if p>0,
            aux2    = p*(factorial(p-1) * ((p-2)==q) + factorial(q).* ((p-1)==(q-1)));
        else
            aux2    = 0;
        end
        indq        = q>0;
        aux3        = zeros(sum(ind),1);
        aux3(indq)  =  q(indq).*...
            (factorial(p) * ((p-1)==(q(indq)-1)) + factorial(q(indq)-1) .* (p==(q(indq)-2)));
        c2ijk_temp   = prod(factorial(alpha2(ind,:)),2).*...
            (aux1 + aux2 + aux3);
        ind         = find(c2ijk_temp~=0);
        c2ijk    = [c2ijk;...
            c2ijk_temp(ind)];
        kv      = [kv;kv_temp(ind)];
        jv  = [jv;j*ones(length(kv)-length(jv),1)];
    end
    iv  = [iv;(i-1)*ones(length(jv)-length(iv),1)];
end


return