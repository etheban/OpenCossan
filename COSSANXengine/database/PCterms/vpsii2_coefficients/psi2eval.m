function psi2 = psi2eval(M,p)
%PSI2EVAL   evaluates <Psi_i^2>
%   PSI2EVAL (M,p) returns the vector of coefficients <Psi_i^2>, for a PC basis
%   with dimension M and order p.
%
%   See also: CIJKEVAL
%
%   Version 1.1
%   Manuel Pellissetti, October 25, 2002

% Generate the index sequences for the multidimensional Hermite polynomials
[alpha nalpha] = getindex_incl(M,p);

% Evaluate cijk for i=1 (corresponds to psi_1 = 1, shorter evaluation)
psi2    = prod(factorial(alpha),2);
