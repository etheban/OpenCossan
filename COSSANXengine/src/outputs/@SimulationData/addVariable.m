function Xobj = addVariable(Xobj,varargin)
%ADDVARIABLE adds output values to SimulationData object
%
%   MANDATORY ARGUMENTS
%
%   OUTPUT
%   - Xobj: object of class SimulationData
%
%   USAGE
%   Xobj = Xobj.add(PropertyName, PropertyValue, ...)
%
%
%   EXAMPLES
%   Xobj = Xobj.add('Sname','Pippo','values',values)
%
% =====================================================
% COSSAN - COmputational Stochastic Structural Analysis
% IfM, Chair of Engineering Mechanics, LFU Innsbruck, A
% Copyright 1993-2009 IfM
% =====================================================


%% Validate input arguments
OpenCossan.validateCossanInputs(varargin{:})

%% Add samples
Xsim2=SimulationData(varargin{:});

Xobj=Xobj.merge(Xsim2);

