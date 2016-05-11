function Xobj = setGridProperties(Xobj,varargin)
%setGridProperties This method is used to add Grid support to the model
%
% THIS FUNCTION SHOULD ONLY BE USED BY COSSAN-X
% The second setting for the grid will be ignored since they refer to the
% PerformanceFunction
%
% ==================================================================
% COSSAN-X - The next generation of the computational stochastic analysis
% University of Innsbruck, Copyright 1993-2011 IfM
% ==================================================================

% Argument Check
OpenCossan.validateCossanInputs(varargin{:})

% Set parameters defined by the user
for k=1:2:length(varargin),
    switch lower(varargin{k})
        case 'cxjobmanagerinterface'
            Xjmi = varargin{k+1}{1};
        case 'ccxjobmanagerinterface'
            Xjmi = varargin{k+1}{1}{1};
        case 'csevaluatornames'
            CevaluatorNames = varargin{k+1};
        case 'cshosts'
            CShosts = varargin{k+1};
        case 'csqueues'
            CSqueues = varargin{k+1};
        case 'csparallelenvironments'
            CSparallelEnvironments = varargin{k+1};
        case 'vslots'
            Vslots = varargin{k+1};
        case 'vconcurrent'
            Vconcurrent = varargin{k+1};
        case 'lremoteinjectextract'
            LremoteInjectExtract = varargin{k+1};
        otherwise
            error('openCOSSAN:Model:addGrifInfo',...
                'Field name %s not allowed\nAvailable field name are: %s %s %s %s %s %s %s %s',...
                varargin{k},'CXjobmanagerInterface', 'CCXjobmanagerinterface', 'CSevaluatorNames', ...
                'CShosts','CSqueues','Vconcurrent','CSparallelEnvironments',...
                'Vslots');
    end
end

Nevaluators=length(Xobj.Xmodel.Xevaluator.CXsolvers);

if exist('CevaluatorNames','var')
    for n=1:Nevaluators
        if ~strcmp(CevaluatorNames{n},Xobj.Xevaluator.CSnames{n})
            warning('openCOSSAN:Model:setGridProperties', ...
                ['Name of the evaluator required %s does not correspond to the name' ...
                ' present in the evaluator %s'],CevaluatorNames{n},Xobj.Xmodel.Xevaluator.CSnames{n})
        end
    end
end

% Set JobManagerInterface
if exist('Xjmi','var')
    Xobj.Xmodel.Xevaluator.XjobInterface=Xjmi;
end

% Set Hostnames
if exist('CShosts','var')
    assert(length(CShosts)==Nevaluators+1, ...
        'openCOSSAN:Model:setGridProperties', ...
        'Length of CShost is %i and must be %i',length(CShosts),Nevaluators+1)
    Xobj.Xmodel.Xevaluator.CShostnames=CShosts(1:end-1);
end

% Set Queues
if exist('CSqueues','var')
    assert(length(CSqueues)==Nevaluators+1, ...
        'openCOSSAN:Model:setGridProperties', ...
        'Length of CSqueues is %i and must be %i',length(CSqueues),Nevaluators+1)
    
    Xobj.Xmodel.Xevaluator.CSqueues=CSqueues(1:end-1);
end

% Set Paralel Environments
if exist('CSparallelEnvironments','var')
    assert(length(CSparallelEnvironments)==Nevaluators+1, ...
        'openCOSSAN:Model:setGridProperties', ...
        'Length of CSparallelEnvironments is %i and must be %i',length(CSparallelEnvironments),Nevaluators+1)
    
    Xobj.Xmodel.Xevaluator.CSparallelEnvironments=CSparallelEnvironments(1:end-1);
end

% Set number of slots
if exist('Vslots','var')
    assert(length(Vslots)==Nevaluators+1, ...
        'openCOSSAN:Model:setGridProperties', ...varargout=Reliability_anal.setGridProperties('Vconcurrent',[1 1])
        'Length of Vslots is %i and must be %i',length(Vslots),Nevaluators+1)
    
    Xobj.Xmodel.Xevaluator.Vslots=Vslots(1:end-1);
end

% Set Concurrent jobs
if exist('Vconcurrent','var')
    assert(length(Vconcurrent)==Nevaluators+1, ...
        'openCOSSAN:Model:setGridProperties', ...
        'Length of Vconcurrent is %i and must be %i',length(Vconcurrent),Nevaluators+1)
    
    Xobj.Xmodel.Xevaluator.Vconcurrent=Vconcurrent(1:end-1);
end

if exist('LremoteInjectExtract','var')
    Xobj.Xmodel.Xevaluator.LremoteInjectExtract = LremoteInjectExtract;
end

varargout=Reliability_anal.setGridProperties('Vconcurrent',[1 1])
end

