function Xobj = merge(Xobj,Xobj2)
%MERGE merge 2 SimulationData objects
%
%   MANDATORY ARGUMENTS
%   - Xobj2: SimulationData object
%
%   OUTPUT
%   - Xobj: object of class SimulationData
%
%   USAGE
%   Xobj = Xobj.merge(Xobj2)
%
% =====================================================
% COSSAN - COmputational Stochastic Structural Analysis
% IfM, Chair of Engineering Mechanics, LFU Innsbruck, A
% =====================================================


%% Check Objects
% Check if one empty Simulation Output is passed
if isempty(Xobj.Cnames)
    OpenCossan.cossanDisp('[OpenCossan:SimulationData:merge] DataMerge descriptions and return the second SimulationData object',4)
    % Merge description and return the second SimulationData object
    Sdescription=[Xobj.Sdescription Xobj2.Sdescription];
    Xobj=Xobj2;
    Xobj.Sdescription=Sdescription;
    return
elseif isempty(Xobj2.Cnames)
    OpenCossan.cossanDisp('[OpenCossan:SimulationData:merge] Merge descriptions and return the first SimulationData object',4)
    % Merge description and return the first SimulationData object
    Sdescription=[Xobj.Sdescription Xobj2.Sdescription];
    Xobj.Sdescription=Sdescription;
    return
end

% Start with collecting fieldnames,
Cfieldname=[Xobj.Cnames; Xobj2.Cnames ];

OpenCossan.cossanDisp(['[Simulation:merge] Collection of the fieldnames: ' sprintf('"%s" ',Cfieldname{:})],4)

if all(isfield(Xobj.Tvalues,Xobj2.Cnames)) && length(Xobj2.Cnames)==length(Xobj.Cnames)
    % Merge 2 Simulation Output with exactly the same variables
    OpenCossan.cossanDisp('Merging SimulationData with the same fieldnames',4)

    % The structure contains the same field names
    Xobj.Tvalues(end+1:end+Xobj2.Nsamples)=Xobj2.Tvalues;
    if ~isempty(Xobj.Mvalues) && ~isempty(Xobj2.Mvalues) 
        Xobj.Mvalues=[Xobj.Mvalues; Xobj2.Mvalues];
    else
        % The array of values can not be merged
        Xobj.Mvalues=[];
    end
else
    % Make sure the field names are unique.
    if length(Cfieldname) ~= length(unique(Cfieldname))
        error('openCOSSAN:SimulationData:merge',...
              strcat('The field names of the SimulationData objects must be unique', ...
              '\nField Names:\n',sprintf('%5s; ',Cfieldname{:})));
    end
     % Make sure the objects have the same number of samples
    if Xobj.Nsamples~=Xobj2.Nsamples
       error('openCOSSAN:SimulationData:merge',...
           ['I can not merge these 2 objects. Obj1 contains ' ...
           num2str(Xobj.Nsamples) ' samples Obj2 contains ' ...
           num2str(Xobj2.Nsamples) ' samples'])  
    end
    
    % Now concatenate the data from each struct.  
    c = [squeeze(struct2cell(Xobj.Tvalues)); squeeze(struct2cell(Xobj2.Tvalues))];
    
    Xobj.Tvalues = cell2struct(c, Cfieldname, 1);
    
    Xobj.LisDataseries = [Xobj.LisDataseries; Xobj2.LisDataseries];
    % Merge the Msamples 
    if ~isempty(Xobj.Mvalues) && ~isempty(Xobj2.Mvalues) 
        if size(Xobj.Mvalues,1) == size(Xobj2.Mvalues,1)
            Xobj.Mvalues=[Xobj.Mvalues Xobj2.Mvalues];   
        else
        % The array of values can not be merged
            Xobj.Mvalues=[];
        end
    else
        % The array of values can not be merged
        Xobj.Mvalues=[];
    end
end


