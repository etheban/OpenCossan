function Vpdf_vX = evalpdf(Xr,Vx)
%EVALPDF Evaluates the pdf of a RandomVariable at the point Vx
%  evalpdf(rv1,vX)
%
% MANDATORY ARGUMENTS:
%   - Xr     :  contains the information about the random variable
%   - Vx     :  realization where the pdf will be evaluated
%
% OUTPUT ARGUMENTS
%   - Vpdf_vX: double array of pdf value at the points of interest
%
%  Usage: Vpdf_Vx = evalpdf(rv1,vX)
%
%  See also: RandomVariable
%
% =====================================================
% COSSAN - COmputational Stochastic Structural Analysis
% IfM, Chair of Engineering Mechanics, LFU Innsbruck, A
% Copyright 1993-2007 IfM
% =====================================================

%% 1.   Argument Verification
if not(nargin==2),
     error('openCOSSAN:RandomVariable:evalpdf','Incorrect number of arguments \n Usage: evalpdf(RVobject,Vector of values)');
end

%% 2.   PDF
switch upper(Xr.Sdistribution)
    case 'NORMAL'
        Vpdf_vX  = normpdf(Vx,Xr.mean,Xr.std);
    case 'LOGNORMAL'
        Vpdf_vX  = lognpdf(Vx-Xr.shift,Xr.Cpar{1,2},Xr.Cpar{2,2});
    case 'UNIFORM'
        Vpdf_vX  = unifpdf(Vx,Xr.lowerBound,Xr.upperBound);
    case 'EXPONENTIAL'
        Vpdf_vX     = exppdf(Vx-Xr.Cpar{2,2},Xr.Cpar{1,2});
    case 'RAYLEIGH'
        Vpdf_vX     = raylpdf(Vx-Xr.shift,Xr.Cpar{1,2});
    case 'STUDENT'
        Vpdf_vX     = tpdf(Vx-Xr.shift,Xr.Cpar{1,2});       
    case 'SMALL-I'
        small_alpha = pi/(sqrt(6)*Xr.std);
        small_u     = Xr.mean+0.5772156/small_alpha;
        Vpdf_vX     = small_alpha * exp(small_alpha*(Vx-Xr.shift-small_u)) .* exp( -exp(small_alpha*(Vx-Xr.shift-small_u)) );
    case 'LARGE-I'
            large_alpha = pi/(sqrt(6)*Xr.std);
            large_u     = Xr.mean-0.5772156/large_alpha;
            Vpdf_vX     = large_alpha * exp(-large_alpha*(Vx-Xr.shift-large_u)) .* exp( -exp(-large_alpha*(Vx-Xr.shift-large_u)) );
    case 'WEIBULL'
        Vpdf_vX     = wblpdf(Vx-Xr.shift,Xr.Cpar{1,2},Xr.Cpar{2,2});
    case 'GAMMA'
        Vpdf_vX     = gampdf(Vx-Xr.shift,Xr.Cpar{1,2},Xr.Cpar{2,2});
    case 'BETA'
        Vpdf_vX     = betapdf(Vx-Xr.shift,Xr.Cpar{1,2},Xr.Cpar{2,2});
    case 'LOGISTIC'
        Vpdf_vX     = exp(-(Vx-Xr.Cpar{1,2})/Xr.Cpar{2,2})./(Xr.Cpar{2,2}*(1+exp(-(Vx-Xr.Cpar{1,2})/Xr.Cpar{2,2})).^2);
   case {'GENERALIZEDPARETO'}
       Vpdf_vX     = gppdf(Vx-Xr.shift,Xr.Cpar{1,2},Xr.Cpar{2,2},Xr.Cpar{3,2});
    case 'F'
        Vpdf_vX     = pdf('f',Vx-Xr.shift,Xr.Cpar{1,2},Xr.Cpar{2,2});
    case 'CHI2'
        Vpdf_vX     = pdf('CHI2',Vx-Xr.shift,Xr.Cpar{1,2});
    case 'NORMT'
        m=Xr.Cpar{1,2};
        s=Xr.Cpar{2,2};
        a=Xr.Cpar{3,2};
        b=Xr.Cpar{4,2};
        aa = (a-m)/s;
        bb = (b-m)/s;
        Vpdf_vX = zeros(size(Vx));
        %analytical pdf of truncated gaussian
        for i=1:length(Vx)
            if Vx(i)>b || Vx(i)<a
                Vpdf_vX(i)=0;
            else
                Vpdf_vX(i) = normpdf((Vx(i)-m)/s,0,1)/(normcdf(bb,0,1) - normcdf(aa,0,1));
            end
        end
    case 'UNID'
        Vpdf_vX     = pdf('unid',Vx-Xr.lowerBound-Xr.shift,Xr.upperBound-Xr.lowerBound);
    case {'POISSON','GEOMETRIC'}
        Vpdf_vX     = pdf(Xr.Sdistribution,Vx-Xr.shift,Xr.Cpar{1,2});
    case {'BINOMIAL','NEGATIVE BINOMIAL'}
        Vpdf_vX     = pdf(Xr.Sdistribution,Vx-Xr.shift,Xr.Cpar{1,2},Xr.Cpar{2,2});
    case 'HYPERGEOMETRIC'
        Vpdf_vX     = pdf(Xr.Sdistribution,Vx-Xr.shift,Xr.Cpar{1,2},Xr.Cpar{2,2},Xr.Cpar{3,2}); 
    otherwise
        error('openCOSSAN:RandomVariable:evalpdf',['distribution ' Xr.Sdistribution ' not available']);
end
   
