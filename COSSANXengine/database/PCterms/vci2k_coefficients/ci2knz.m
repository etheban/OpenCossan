function [iv kv c2ik] = ci2knz(M,p)
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




% Evaluate cijk for i=1 (corresponds to psi_1 = 1, shorter evaluation)
iv  = [];
kv  = [];
c2ik = [];


for i=2:(M+1),
    % Find which xi_i is referred to by the psi_i
    ixi     = find(alpha(i,:));
    aux_ind = setdiff(1:size(alpha,2),ixi);
    for k=1:nalpha,
        % Check if integer sequences for psi_j and psi_k are the same, except for element ixi
        % (If they are not the same, then cijk=0)
        aux_alpha   = alpha(k,aux_ind);
        if all(~aux_alpha),
            if alpha(k,ixi)==0,
                kv      = [kv;k];
                c2ik    = [c2ik;1];
            elseif alpha(k,ixi)==2
                kv      = [kv;k];
                c2ik    = [c2ik;2];
            end
        end
        
    end
    iv  = [iv;(i-1)*ones(length(kv)-length(iv),1)];
end



return