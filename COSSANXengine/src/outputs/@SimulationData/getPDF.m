function [Vedges,Vpdf] = getPDF(Xobj,varargin)
%GETPDF This method computes the empirical PDF of the samples stored in the SimulationData object.
%
%  USAGE: [Vsupport,Vpdf]=XsimulationData.getPDF(varargin)
%
%  The method returns the vector of the support points (Vsupport) and the vector
%  of values of the pdf (Vpdf).
%
%  Valid input arguments: Nbins, Sname, Cnames, Vsuppport
%
%  See Also: http://cossan.co.uk/wiki/index.php/getPDF@SimulationData
%
% $Copyright~1993-2016,~COSSAN~Working~Group,~University~of~Liverpool,~UK$
% $Author: Edoardo-Patelli$

% =====================================================================
% This file is part of openCOSSAN.  The open general purpose matlab
% toolbox for numerical analysis, risk and uncertainty quantification.
%
% openCOSSAN is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License.
%
% openCOSSAN is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with openCOSSAN.  If not, see <http://www.gnu.org/licenses/>.
% =====================================================================


%% Set default values
Nbins=[];
CrequestedVariables=Xobj.Cnames; %
Vedges=[];

% Process input arguments
OpenCossan.validateCossanInputs(varargin{:});

for k=1:2:length(varargin)
    switch lower(varargin{k})
        case 'nbins'
            Nbins=varargin{k+1};
        case 'sname'
            CrequestedVariables=varargin(k+1);
        case 'cnames'
            CrequestedVariables=varargin{k+1};
        case 'vedges'
            Vedges=varargin{k+1};
        otherwise
            error('openCOSSAN:SimulationData:getPDF',...
                'PropertyName %s is not a valid input argument',varargin{k});
    end
end

assert(all(ismember(CrequestedVariables,Xobj.Cnames)),...
        'openCOSSAN:outputs:SimulationData:getPDF:wrongInput', ...
        'Variable(s) not present in the SimulationData object!\n Required variables: %s\nAvailable variables: %s',...
        sprintf('"%s" ',CrequestedVariables{:}),sprintf('"%s" ',Xobj.Cnames{:}))


%% Get Values
Mvalues=Xobj.getValues('Cnames',CrequestedVariables);
Nvariables=length(CrequestedVariables);
%% Evaluate PDF
if ~isempty(Vedges)
    Vpdf=zeros(length(Vedges)-1,Nvariables);
    for j=1:Nvariables
        [Vpdf(:,j)] = histcounts(Mvalues(:,j),Vedges,'Normalization', 'probability');
    end
else
    assert(Nvariables==1,'openCOSSAN:outputs:SimulationData:getPDF:wrongInput',...
        ['Only the PDF of one variable can be computed.\nProvide define the bin edges with ',...
        'a vector Vsupport if you want to get the PDF of more variables'])
    if isempty(Nbins)
        [Vpdf,Vedges] = histcounts(Mvalues,'Normalization', 'probability');
    else
        [Vpdf,Vedges] = histcounts(Mvalues,Nbins,'Normalization', 'probability');
    end
end




