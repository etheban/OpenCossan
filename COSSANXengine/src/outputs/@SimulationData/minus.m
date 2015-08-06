function XSimOut = minus(XSimOut1,XSimOut2)
%MINUS substracts XSimOut2 from XSimOut1
%
%
%  Usage: MINUS(XSimOut1,XSimOut2) substracts the Output object XSimOut2
%  from XSimOut1
%  Example:  minus(XSimOut1,XSimOut2)
%
% =====================================================
% COSSAN - COmputational Stochastic Structural Analysis
% IfM, Chair of Engineering Mechanics, LFU Innsbruck, A
% =====================================================


Vindex = isfield(XSimOut1.Tvalues,XSimOut2.Cnames);
if ~all(Vindex)==1
    error('openCOSSAN:SimulationData:minus',...
        'the two objects do not contain the same output variables');
end

if XSimOut1.Nsamples ~= XSimOut2.Nsamples
    error('openCOSSAN:SimulationData:minus',...
        'the two objects do not contain the same number of simulations');
end

% Initialize structure
Cdiff=struct2cell(XSimOut1.Tvalues);
C2=struct2cell(XSimOut2.Tvalues);

for i=1:length(Cdiff)
    Cdiff{i}=Cdiff{i}-C2{i};
end

Tvalues = cell2struct(Cdiff, XSimOut1.Cnames, 1);        
XSimOut = SimulationData('Tvalues',Tvalues);

end

