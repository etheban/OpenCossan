function [Tout, LsuccessfullExtract] = extract(Xte,varargin)
%EXTRACTOR  extract values form the 3-rd party ASCII output file and create
%                       a structure Tout
%
%   Arguments:
%   ==========
%   Xte         TableExtractor object (mandatory)
%   Tout        Structure that contains the extracted values
%
%   Usage:  Tout  = extract(Xte)
%
%   see also: connector, extractor
%

%% 1. Processing Inputs

global OPENCOSSAN

OpenCossan.validateCossanInputs(varargin{:});
for iopt=1:2:length(varargin)
    switch lower(varargin{iopt})
        case {'nsimulation'}
            Nsimulation = varargin{iopt+1};
        otherwise
            error('openCOSSAN:Extractor:extract',...
                ['Optional parameter ' varargin{iopt} ' not allowed']);
            
    end
end

LsuccessfullExtract = true;

%% 3. Access to the output file

if ~isempty(Xte.Xresponse)
    
    [Nfid Serror] = fopen(fullfile(Xte.Sworkingdirectory,Xte.Srelativepath,Xte.Sfile),'r'); % open ASCII file
    OpenCossan.cossanDisp(['[COSSAN-X:Tablextractor:extract] Open file : ' fullfile(Xte.Sworkingdirectory,Xte.Srelativepath,Xte.Sfile)],4 )
    
    % Return NaN value if an error occurs in the extractor
    if ~isempty(Serror)
        if exist('Nsimulation','var')
            warning('openCOSSAN:TableExtractor:extract',...
                ['The results file ' Xte.Sworkingdirectory Xte.Srelativepath Xte.Sfile ' of simulation #' num2str(Nsimulation) ' does not exist'])
        else
            warning('openCOSSAN:TableExtractor:extract',...
                ['The results file ' Xte.Sworkingdirectory Xte.Srelativepath Xte.Sfile ' does not exist'])
        end
        for iresponse=1:Xte.Nresponse
            Tout.(Xte.Xresponse(iresponse).Sname)=NaN;
            LsuccessfullExtract = false;
        end
        return;
    else
        OpenCossan.cossanDisp('[COSSAN-X.TableExtractor.extract] File Open correctly',4 )
    end
    
    
    %% Extract the values from file
    
    for iresponse=1:Xte.Nresponse
        
        % already_reset is a flag that check whether the end of file have been
        % already reached while looking for the iresponse-th response
        already_reset = false;
        
        if ~isempty(Xte.Xresponse(iresponse).Sregexpression)
            
            while 1
                tline = fgetl(Nfid);
                if ~ischar(tline),
                    if already_reset
                        warning('openCOSSAN:TableExtractor:extract','End of file reached');
                        break
                    else
                        already_reset = true; % set the flag to true
                        warning('openCOSSAN:TableExtractor:extract',...
                            ['File position reset while looking for the '...
                            num2str(iresponse) '-th response.\n'...
                            'Be sure to define reponses in the order they appear in the output file to improve performance'])
                        fseek(Nfid, 0, 'bof'); % reset file position
                        tline = fgetl(Nfid);
                    end
                end
                Nfound=regexp(tline, Xte.Xresponse(iresponse).Sregexpression,'end');
                if ~isempty(Nfound),
                    break,
                end
            end
            
        elseif ~isempty(Xte.Xresponse(iresponse).Clookoutfor)
            % Search the Clookoutfor strings
            for ilook=1:length(Xte.Xresponse(iresponse).Clookoutfor)
                Nfound=[];
                while isempty(Nfound)
                    tline = fgetl(Nfid);
                    if ~ischar(tline),
                        if already_reset
                            warning('openCOSSAN:TableExtractor:extract','End of file reached');
                            break
                        else
                            already_reset = true; % set the flag to true
                            warning('openCOSSAN:TableExtractor:extract',...
                                ['File position reset while looking for the '...
                                num2str(iresponse) '-th response.\n'...
                                'Be sure to define reponses in the order they appear in the output file to improve performance'])
                            fseek(Nfid, 0, 'bof'); % reset file position
                            tline = fgetl(Nfid);
                        end
                    end
                    Nfound = regexp(tline, Xte.Xresponse(iresponse).Clookoutfor{ilook}, 'end');
                end
            end
        elseif ~isempty(Xte.Xresponse(iresponse).Svarname)
            try
                positioncurrent = Tpos.(Xte.Xresponse(iresponse).Svarname);
            catch ME
                warning('openCOSSAN:TableExtractor:extract',...
                    ['Position of the response ' Xte.Xresponse(iresponse).Svarname ' not found \n ' ME.message])
                OpenCossan.cossanDisp('Continue with the next response')
                Tout.(Xte.Xresponse(iresponse).Sname)=NaN;
                LsuccessfullExtract = false;
                continue
            end
            Serror= fseek(Nfid, positioncurrent, 'bof');
            if Serror~=0
                Tout.(Xte.Xresponse(iresponse).Sname)=NaN;
                LsuccessfullExtract = false;
                continue
            end
        end
        
        % Now skip Nrow from the current position
        if isa(Xte.Xresponse(iresponse).Nrownum,'char')
            Xte.Xresponse(iresponse).Nrownum=str2num(Xte.Xresponse(iresponse).Nrownum); %#ok<ST2NM>
        end
        
        for i=1:Xte.Xresponse(iresponse).Nrownum-1
            tline=fgetl(Nfid);
            if tline==-1, break, end % exit from the loop if the end of file is found
        end
        
        if tline==-1
            if exist('Nsimulation','var')
                warning('openCOSSAN:Extractor:extract',...
                    ['Problems extracting value(s) for ' Xte.Xresponse(iresponse).Sname ' of simulation #' num2str(Nsimulation)]);
            else
                warning('openCOSSAN:Extractor:extract',...
                    ['Problems extracting value(s) for ' Xte.Xresponse(iresponse).Sname]);
                
            end
            Tout.(Xte.Xresponse(iresponse).Sname)=NaN;
            Tpos.(Xte.Xresponse(iresponse).Sname)=NaN;
            LsuccessfullExtract = false;
            continue
        end
        
        ncols = length(regexp(Xte.Xresponse(iresponse).Sfieldformat,'%'))-length(regexp(Xte.Xresponse(iresponse).Sfieldformat,'*'));
        output = (fscanf(Nfid, Xte.Xresponse(iresponse).Sfieldformat,[ncols,Xte.Xresponse(iresponse).Nrepeat]))';
        output = Dataseries('Mdata',output(:,2:end)','Mcoord',output(:,1)');
        
        % Associate read value with the output parameter
        try
            OpenCossan.cossanDisp(['[COSSAN-X:TableExtractor.extract] Response #' num2str(iresponse) ': ' ],4 )
            OpenCossan.cossanDisp('[COSSAN-X:TableExtractor.extract] Value(s) extracted: ' ,4 )
            if OPENCOSSAN.NverboseLevel>3
                output %#ok<NOPRT>
            end
            Tout.(Xte.Xresponse(iresponse).Sname)=output;
        catch ME
            OpenCossan.cossanDisp(['[COSSAN-X.TableExtractor.extract] Failed to associate extracted value to the output ( ' ME.message ')' ],4 )
            fclose(Nfid);
            return
        end
        % save the position of the last extracted value
        Tpos.(Xte.Xresponse(iresponse).Sname)=ftell(Nfid);
    end
    
    % close the file
    status=fclose(Nfid);
    OpenCossan.cossanDisp(['[COSSAN-X.TableExtractor.extract] Closing output files (status: ' status ')' ],4 )
    
else
    
    Sfile = fullfile(Xte.Sworkingdirectory, Xte.Srelativepath, Xte.Sfile);
    
    if ~exist(Sfile,'file')
        LsuccessfullExtract = false;
        for ioutput=1:length(Xte.Coutputnames)
            Tout.(Xte.Coutputnames{ioutput})=NaN;
        end
        return,
    end
    
    OpenCossan.cossanDisp(['[COSSAN-X.TableExtractor.extract] Reading file : ' Xte.Sworkingdirectory Xte.Srelativepath Xte.Sfile],4 )
    if ~Xte.Luseload
        Mextract = importdata(Sfile,Xte.Sdelimiter,Xte.Nheaderlines);
        if isstruct(Mextract)
            Mextract = Mextract.data;
        end
    else
        assert(isempty(Xte.Sdelimiter) && Xte.Nheaderlines==0,...
            'openCOSSAN:Extractor:extract',...
            ['Cannot import a table using load command if the data is not '...
             'purely in an ascii matrix format!']);
        try
            Mextract = load(Sfile);
        catch
            error('openCOSSAN:Extractor:extract',...
                ['Cannot import a table using load command if the data is not '...
                 'purely in an ascii matrix format!']);
        end
    end
%     output = Mextract(1,Xte.Ncolumns);
    %output = Dataseries('Mdata',Cextract.data(:,2:end)','Mcoord',Cextract.data(:,1)');
        
    if Xte.LextractColumns
        Ncolumns=length(Xte.CcolumnPosition);
        Mdata=zeros(size(Mextract,1),Ncolumns);
        for icol=1:Ncolumns
            Mdata(:,icol)=Mextract(:,Xte.CcolumnPosition{icol});
        end
    else
        Nlines=length(Xte.ClinePosition);
        Mdata=zeros(size(Mextract,2),Nlines);
        for irow=1:length(Xte.ClinePosition)
            Mdata(:,irow)=Mextract(:,Xte.ClinePosition{irow});
        end
    end
    
    if Xte.LextractCoord
        output = Dataseries('Vdata',Mdata','Mcoord',Mextract(:,1)');
    else
        output = Dataseries('Mmatrix',Mdata');
    end
    
    try
        OpenCossan.cossanDisp(['[COSSAN-X:TableExtractor.extract] Response ' Xte.Soutputname ': ' ],4 )
        OpenCossan.cossanDisp('[COSSAN-X:TableExtractor.extract] Table extracted: ' ,4 )
        if OPENCOSSAN.NverboseLevel>3
            output %#ok<NOPRT>
        end
        Tout.(Xte.Soutputname)=output;
    catch ME
        OpenCossan.cossanDisp(['[COSSAN-X.TableExtractor.extract] Failed to associate extracted value to the output ( ' ME.message ')' ],4 )
        return
    end
end