function XSimOut = plus(XSimOut1,XSimOut2)
%PLUS adds one SimulationData object to the other
%
%
%  Usage: PLUS(XSimOut1,XSimOut2) adds the values of the Output object XSimOut2
%  to the  valus of the Output object XSimOut1
%  Example:  plus(XSimOut1,XSimOut2)
%
% =====================================================
% COSSAN - COmputational Stochastic Structural Analysis
% IfM, Chair of Engineering Mechanics, LFU Innsbruck, A
% =====================================================
%
% History:
% BG, 2009-11-17
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


for i=1:length(XSimOut1.Cnames)
    for isim = 1:XSimOut1.Nsamples
     Tvalues(isim).(XSimOut1.Cnames{i}) = ...
        XSimOut1.Tvalues(isim).(XSimOut1.Cnames{i}) + XSimOut2.Tvalues(isim).(XSimOut1.Cnames{i});
    end
end
        
XSimOut = SimulationData('Tvalues',Tvalues);
end

