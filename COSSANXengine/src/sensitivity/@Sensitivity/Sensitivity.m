classdef Sensitivity
    %SENSITIVITY Sensitivity Toolbox for COSSAN-X
    % This is an abstract class for creating sensitivity objects
    % See also: https://cossan.co.uk/wiki/index.php/@Sensitivity
    %
    % Author: Edoardo Patelli
    % Institute for Risk and Uncertainty, University of Liverpool, UK
    % email address: openengine@cossan.co.uk
    % Website: http://www.cossan.co.uk
    
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
    
    properties (SetAccess=protected,GetAccess=public)
        Sdescription
        Xtarget
        LperformanceFunction
        Cinputnames
        Coutputnames
        Xinput
    end
    
    properties (SetAccess=protected,GetAccess=public)
        Sevaluatedobjectname='N/A'
        Xsamples0
        fx0
    end    
       
    methods (Access=public)
        display(Xobj)                   % Show summary of Sensitivity object
        varargout=apply(Xobj,Xtarget)   % Perform Sensitivity on the Target object
        varargout=evaluate(Xobj)        % Perform Sensitivity
    end
    
    methods (Access=protected)
        Xobj=addModel(Xobj,Xtarget) 
        Xobj=validateSettings(Xobj)
        setAnalysisName(Xobj)
    end
    
    methods (Static) % Define static methods An instance of the sensitivity
        % object is not required
        
        varargout=gradientFiniteDifferences(varargin) % Compute the gradient
        % using a Finite
        % Difference Method,
        
        varargout=gradientMonteCarlo(varargin)   % Compute the gradient
        % using Monte Carlo method
        
        varargout=localFiniteDifferences(varargin) % Compute the LocalSensitivityMeasures
        % using a Finite Difference Method,
        
        varargout=localMonteCarlo(varargin) % Compute the LocalSensitivityMeasures
        % using Monte Carlo method
        
        %Xobj=fast(varargin) % compute the Fourier amplitude sensitivity test
        
        Xobj=randomBalanceDesign(varargin) % compute the first order
        % sensitivity analysis using the
        % Fourier Amplitude Sensitivity
        % Test (Tarantola et al. 2006)
        
        Xobj=sobolIndices(varargin) % compute the Sobol' sensitivity indices via
        % Monte Carlo simulation
        
        Xobj=upperBounds(varargin) % compute the upper bounds of the total
        % sensitivity indicies based on MCMC and
        % gradiend MC (Patelli et all. 2010)
        
        % Xobj=localAnalysis(varargin) % compute local sensitivity analysis
        
        % Xobj=globalAnalysis(varargin) % compute global sensitivity analysis
        
        % TO BE REMOVED!!! - ONly for GUI compatibility
        [Xout, varargout]=coreFiniteDifferences(varargin)
        [Xout, varargout]=coreMonteCarlo(varargin)
        [Xinput,perturbation,Coutputname]=checkModel(Xtarget,perturbation,LperformanceFunction,Coutputname)
        
    end
    
    
end

