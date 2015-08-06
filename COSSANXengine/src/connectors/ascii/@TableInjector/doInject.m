function doInject(Xobj,Tinput)
%REPLACE_VALUES replace values in the open file
%
%
%       '-mat'                        Binary MAT-file format (default).
%       '-ascii'                      8-digit ASCII format.
%       '-ascii', '-tabs'             Tab-delimited 8-digit ASCII format.
%       '-ascii', '-double'           16-digit ASCII format.
%       '-ascii', '-double', '-tabs'  Tab-delimited 16-digit ASCII format.
%%

SfullName=fullfile(Xobj.Sworkingdirectory,Xobj.Srelativepath,Xobj.Sfile);

OpenCossan.cossanDisp(['[COSSAN-X.TableIdentifier.doInject] Filename:' SfullName],4);

CavailableInputs=fieldnames(Tinput);
assert(all(ismember(Xobj.Cinputnames,CavailableInputs)),...
    'openCOSSAN:TableIdentifier.doInject:checkInput', ...
    'Variable(s) not present in the input structure!\n Required variables: %s\nAvailable variables: %s',...
    sprintf('"%s" ',Xobj.Cinputnames{:}),sprintf('"%s" ',CavailableInputs{:}))


Mcoord = [];
Mdata=[];
for n=1:Xobj.Nvariable
    % get the value to be injected
    switch class(Tinput.(Xobj.Cinputnames{n}))
        case 'Dataseries'
            assert(Xobj.Nvariable==1,'openCOSSAN:TableIdentifier.doInject:DataseriesInjection',...
                'Only a single Dataseries can be injected in a TableInjector!')
            % if an index is specified, assign to value the content of
            % the property Mdata of the Dataseries object at the
            % specified index
            Mcoord = Tinput.(Xobj.Cinputnames{n}).Mcoord;
            Vdata = Tinput.(Xobj.Cinputnames{n}).Vdata;
            if ~isempty(Xobj.Vindices)
                Mdata = [Mcoord(:,Xobj.Vindices); Vdata(Xobj.Vindices)];
            else
                Mdata = [Mcoord; Vdata];
            end
        case {'double','single','logical'}
            Mdata = [Mdata Tinput.(Xobj.Cinputnames{n})];
        otherwise
            error('openCOSSAN:TableIdentifier:doInject:wrongClass',['It is not possible '...
                'to inject values from object of class %s'], ...
                class(Tinput.(Xobj.Cinputnames{n})))
    end
end


% Vfield=ismember(Xobj.Cinputnames,CavailableInputs);
% %% Convert structure to cell
% Cout=struct2cell(Tvalues);
% % removed unrequested values
% Cout(~Vfield,:)=[];
% % removed unrequested names
% Mout=cell2mat(Cout)'; %#ok<NASGU>

switch lower(Xobj.Stype)
    case {'matlab8'}
        save(SfullName,'Mdata', '-ascii', '-tabs');
    case {'matlab16'}
        save(SfullName,'Mdata', '-ascii','-double','-tabs');
    case {'nastran16_table'}
        if ~isempty(Mcoord)
            % you are injecting a Dataseries
            assert(size(Mcoord,1)==1,...
                'openCOSSAN:TableIdentifier:doInject:wrongDataSeriesFormat',...
                'Cannot inject a Dataseries with coordinates dimensions greater than 1.') 
        end
        
        Vdatatime = Mdata(:);
        ncols = 4;
        nrows = floor(length(Vdatatime)/ncols);
        nremaining = rem(length(Vdatatime),ncols);
        Mvalueinject = reshape(Vdatatime(1:end-nremaining),ncols,nrows);
        
        Nfid = fopen(SfullName,'w');
        fprintf(Nfid, '*      %16.7e%16.7e%16.7e%16.7e\n', Mvalueinject);
        if nremaining ~= 0
            fprintf(Nfid, '*      %16.7e%16.7e   ENDT\n', Vdatatime(end-1:end));
        else
            fprintf(Nfid, '*         ENDT\n');
        end
        fclose(Nfid);
        
    case {'abaqus_table'}
        if ~isempty(Mcoord)
            % you are injecting a Dataseries
            assert(size(Mcoord,1)==1,...
                'openCOSSAN:TableIdentifier:doInject:wrongDataSeriesFormat',...
                'Cannot inject a Dataseries with coordinates dimensions greater than 1.') 
        end
        
        Vdatatime = Mdata(:);
        ncols = 8;
        nrows = floor(length(Vdatatime)/ncols);
        nremaining = rem(length(Vdatatime),ncols);
        Mvalueinject = reshape(Vdatatime(1:end-nremaining),ncols,nrows);
        
        Nfid = fopen(SfullName,'w');
        fprintf(Nfid, '%16.7e,%16.7e,%16.7e,%16.7e,%16.7e,%16.7e,%16.7e,%16.7e,\n', Mvalueinject);
        if nremaining == 2
            fprintf(Nfid, '%16.7e,%16.7e,', Vdatatime(end-1:end));
        end
        if nremaining == 4
            fprintf(Nfid, '%16.7e,%16.7e,%16.7e,%16.7e,', Vdatatime(end-3:end));
        end
        if nremaining == 6
            fprintf(Nfid, '%16.7e,%16.7e,%16.7e,%16.7e,%16.7e,%16.7e,', Vdatatime(end-5:end));
        end
        fclose(Nfid);
        
    case {'userdefined'}
        error('TO BE IMPLEMENTED')
    otherwise
        error('openCOSSAN:TableInjector:doInject',...
            'Injector Type (%s) is not a valid format', Xobj.Stype)
end


