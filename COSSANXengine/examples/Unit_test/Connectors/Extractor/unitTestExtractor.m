function varargout = unitTestExtractor

% Test constructor
Ntest=11;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

% set the dir with the file to be extracted
SworkingDirectory = fullfile(OpenCossan.getCossanRoot,'/examples/Unit_test/Connectors/Extractor/');

Xresp1 = Response('Sname', 'OUT1', ...
             'Sfieldformat', '%11e', ...
             'Clookoutfor',{'E L E M E N T   O U T P U T'}, ...
             'Ncolnum',24, ...
             'Nrownum',11, ...
             'Nrepeat',2); 
         
Xresp2 = Response('Sname', 'OUT1', ...
             'Sfieldformat', '%11e', ...
             'Clookoutfor',{'E L E M E N T   O U T P U T'}, ...
             'Ncolnum',24, ...
             'Nrownum',11, ...
             'Nrepeat',1); 
         
Xresp3 = Response('Sname', 'OUT2', ...
             'Sfieldformat', '%11e', ...
             'Svarname','OUT1', ...
             'Ncolnum',24, ...
             'Nrownum',1, ... % position relative to the END of values associated to OUT1
             'Nrepeat',1);          

Xresp4 = Response('Sname', 'OUT3', ...
             'Sfieldformat', '%11e', ...
             'Sregexpression',' E L E M E N T   O U T P U T', ...
             'Ncolnum',24, ...
             'Nrownum',11, ... % position relative to the END of values associated to OUT1
             'Nrepeat',1);          
         
%% 1
% Construct extractor by passing 1 response object

itest = 1;
try
    Xe=Extractor('Sdescription','Extractor for Unit Test', ...
             'SworkingDirectory',SworkingDirectory, ...
             'Srelativepath','./', ...
             'Sfile','2D_Truss.dat',...
             'Xresponse',Xresp1);  
    display(Xe)
    Vtest(itest)=true;
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2
% check the display method

itest = itest + 1;
try
    Xe.display  
    Vtest(itest)=true;
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
% Define Extractor by passing 2 Response objects (this test shall fail
% since both Response object use the same name for the output) 

itest = itest + 1;
try
    Xe=Extractor('Sdescription','Extractor for Unit Test', ...
             'SworkingDirectory',SworkingDirectory, ...
             'Srelativepath','./', ...
             'Sfile','2D_Truss.dat',...
             'Xresponse',[Xresp1 Xresp2]);  
    
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
% Test the method add by adding 1 Response object

itest = itest + 1;
try
    Xe = Xe.add('Xresponse',Xresp3) 
    if Xe.Nresponse == 2
        Vtest(itest)=true;
    end
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
% Test the method remove

itest = itest + 1;
try
    Xe = Xe.remove('OUT2') 
    Vtest(itest)=true;
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
% test the method extract

itest = itest + 1;
try
    Tout = Xe.extract; 
    if Tout.OUT1.Vdata(1) == -1.5175E8 && Tout.OUT1.Vdata(2) == -1.5175E8
        Vtest(itest)=true;
    end
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7
% test extract with response position relative to another output
itest = itest + 1;
try
    Xe=Extractor('Sdescription','Extractor for Unit Test', ...
             'SworkingDirectory',SworkingDirectory, ...
             'Srelativepath','./', ...
             'Sfile','2D_Truss.dat',...
             'Xresponse',[Xresp1 Xresp3]);  
    Tout = Xe.extract; 
    if Tout.OUT1.Vdata(1) == -1.5175E8 && Tout.OUT1.Vdata(2) == -1.5175E8 && Tout.OUT2 ==  3.0349E+08
        Vtest(itest)=true;
    end
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8
% test extract with response position relative to a regular expression
itest = itest + 1;
try
    Xe=Extractor('Sdescription','Extractor for Unit Test', ...
             'SworkingDirectory',SworkingDirectory, ...
             'Srelativepath','./', ...
             'Sfile','2D_Truss.dat',...
             'Xresponse',[Xresp1 Xresp4]);  
    Tout = Xe.extract; 
    if Tout.OUT1.Vdata(1) == -1.5175E8 && Tout.OUT1.Vdata(2) == -1.5175E8 && Tout.OUT3 == -1.5175E8
        Vtest(itest)=true;
    end
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 test construct with CXresponse
itest = itest + 1;
try
    CXresp = {Xresp1, Xresp3, Xresp4};
    Xe=Extractor('Sdescription','Extractor for Unit Test', ...
             'SworkingDirectory',SworkingDirectory, ...
             'Srelativepath','./', ...
             'Sfile','2D_Truss.dat',...
             'CXresponse',CXresp);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 test construct with CCXresponse #1
itest = itest + 1;
try
    % this test should fail
    CCXresp = {Xresp1, {Xresp3}, {Xresp4}};
    Xe=Extractor('Sdescription','Extractor for Unit Test', ...
             'SworkingDirectory',SworkingDirectory, ...
             'Srelativepath','./', ...
             'Sfile','2D_Truss.dat',...
             'CCXresponse',CCXresp);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11 test construct with CCXresponse #2
itest = itest + 1;
try
    CCXresp = {{Xresp1}, {Xresp3}, {Xresp4}};
    Xe=Extractor('Sdescription','Extractor for Unit Test', ...
             'SworkingDirectory',SworkingDirectory, ...
             'Srelativepath','./', ...
             'Sfile','2D_Truss.dat',...
             'CCXresponse',CCXresp);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Finalize the test
if nargout>0
    % Export name of the UnitTest
    varargout{1}='Extractor';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
    
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of Extractor (' datestr(now) ')'])
    disp('--------------------------------------------------------------------')
    for i=1:length(Vtest)
        if Vtest(i)
            disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
        else
            disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
        end
    end
end

end
