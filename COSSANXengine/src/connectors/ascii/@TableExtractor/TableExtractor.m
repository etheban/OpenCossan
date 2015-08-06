classdef TableExtractor < Extractor
    
    properties
       Sdelimiter           % delimiter between columns
       Nheaderlines=0       % number of lines to skip from beginning of file
       Ncolumns=0           % number of column to skip from beginning of row
       LextractColumns=true  % extract single columns from table
       CcolumnPosition       % identify every single column (in case LextractColumns=true)
       ClinePosition         % identify every single column (in case LextractColumns=false)
       Nbegin=1              % beginning of column/line
       Nend                  % ending of column/line
       LextractCoord=true    % extract coordinates from first column/line of table
       Luseload=false        % use load instead of importdata for faster (but less flexible) reading of ascii matrices
    end
    
    properties (Access=private, Dependent=true)
        
    end
    
    methods
        
        function Xobj  = TableExtractor(varargin)
            
            if isempty(varargin)
                %compatibility for empty constructor
                return
            end
            
            %% 1. Check Inputs
            OpenCossan.validateCossanInputs(varargin{:});
            
            %% Set options

            for iVopt=1:2:length(varargin)
                switch lower(varargin{iVopt})
                    case 'sdescription'
                        Xobj.Sdescription = varargin{iVopt + 1};
                    case 'srelativepath'
                        Xobj.Srelativepath = varargin{iVopt + 1};
                    case 'sworkingdirectory'
                        Xobj.Sworkingdirectory = varargin{iVopt + 1};
                    case 'sfile'
                        Xobj.Sfile = varargin{iVopt + 1};
                    case 'nheaderlines'
                        Xobj.Nheaderlines = varargin{iVopt + 1};
                    case 'sdelimiter'
                        Xobj.Sdelimiter = varargin{iVopt + 1};
                    case 'soutputname'
                        Xobj.Soutputname = varargin{iVopt + 1};
                    case 'xresponse'
                        Xobj.Xresponse = varargin{iVopt + 1};
                    case 'cxresponse'
                        for n=1:length(varargin{iVopt + 1})
                            if ~isa(varargin{iVopt + 1}{n},'Response')
                                error('openCOSSAN:Extractor','CXresponse must be a cell array contain Response objects only')
                            end
                            tmp(n) = varargin{iVopt + 1}{n};
                        end
                        Xobj.Xresponse=tmp;
                    case 'ccxresponse'
                        for n=1:length(varargin{iVopt + 1})
                            if (length(varargin{iVopt + 1}{n})~=1)
                                error('openCOSSAN:Extractor','CCXresponse must be a cell array contain 1x1 cellarrays, containing a Response objects only')
                            end
                            if ~isa(varargin{iVopt + 1}{n},'cell')
                                error('openCOSSAN:Extractor','CCXresponse must be a cell array contain 1x1 cellarrays, containing a Response objects only')
                            end
                            if ~isa(varargin{iVopt + 1}{n}{1},'Response')
                                error('openCOSSAN:Extractor','CCXresponse must be a cell array contain 1x1 cellarrays, containing a Response objects only')
                            end
                            tmp(n) = varargin{iVopt + 1}{n}{1};
                        end
                        Xobj.Xresponse=tmp;
                    case 'ncolumns'
                        Xobj.Ncolumns=varargin{iVopt+1};
                    case 'lextractcolumns'
                        Xobj.LextractColumns=varargin{iVopt + 1};
                    case 'ccolumnposition'
                        Xobj.CcolumnPosition = varargin{iVopt + 1};
                    case 'clineposition'
                        Xobj.Nlines = varargin{iVopt + 1};
                    case 'nbegin'
                        Xobj.Nbegin = varargin{iVopt + 1};
                    case 'nend'
                        Xobj.Nend = varargin{iVopt + 1};
                    case 'lextractcoord'
                        Xobj.LextractCoord=varargin{iVopt + 1};
                    case 'luseload'
                        Xobj.Luseload=varargin{iVopt + 1};
                    otherwise
                        error('openCOSSAN:TableExtractor',['Unknown property: ' varargin{iVopt}])
                end
            end % end of inputs definition
            
            if ~isempty(Xobj.Xresponse)
                if ~isempty(Xobj.Nheaderlines) || ~isempty(Xobj.Sdelimiter) || ~isempty(Xobj.Soutputname)
                   error('openCOSSAN:TableExtractor','EITHER a Response object OR a table format (Nheaderlines,Sdelimiter,Soutputname) must be passed')
                end
            else
                if (isempty(Xobj.Sdelimiter) || isempty(Xobj.Soutputname)) && ~Xobj.Luseload
                   error('openCOSSAN:TableExtractor','either a Response object or a table format (Nheaderlines,Sdelimiter,Soutputname) must be passed')
                end
            end
                        % Check if an output is present more than once in the Extractor
            Lerror = false;
            Serrorstring = '';
            for i=1:Xobj.Nresponse
                for j=i+1:Xobj.Nresponse
                    if strcmpi(Xobj.Xresponse(j).Sname,Xobj.Xresponse(i).Sname)
                        Serrorstring = [Serrorstring ' -  ' Xobj.Xresponse(i).Sname ...
                            ' is present in response ' num2str(i) ' and ' num2str(j) '\n'];
                        Lerror = true;
                    end
                end
            end
            if Lerror
                error('openCOSSAN:Extractor:Extractor',['Duplicated output names defined in Extractor:\n' Serrorstring])
            end
            
            if ~isempty(Xobj.Srelativepath) 
                if~(strcmp(Xobj.Srelativepath(end),filesep)) % add / or \ at the end of the path
                    Xobj.Srelativepath  = [Xobj.Srelativepath filesep];
                end
            end
            
            
            end %end constructor
            
            
            [Tout, LsuccessfullExtract] = extract(Xe,varargin)    
    end

        
    
end