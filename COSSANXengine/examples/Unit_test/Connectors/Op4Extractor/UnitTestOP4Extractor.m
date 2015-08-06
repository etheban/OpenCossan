%% Unit test for the OP4Extractor

function varargout=UnitTestOP4Extractor

Ntest=4;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
Sdirectory = fullfile(OpenCossan.getCossanRoot,'/examples/Unit_test/Connectors/Op4Extractor/');

%% 1 Method display
itest=1;
try
    Xop4 = Op4Extractor('Sdescription', 'Op4Extractor for Unit test',...
                'Sworkingdirectory',Sdirectory,...
                'Sfile','BEAM1_K.OP4', ....
                'Soutputname','stiffness');
    display(Xop4);
    Vtest(itest)=true;         
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Check whether Sdescription is correct
itest=itest+1;
try
    Xop4 = Op4Extractor('Sdescription', 'Op4Extractor for Unit test',...
                 'Sworkingdirectory',Sdirectory,...
                'Sfile','BEAM1_K.OP4', ....
                'Soutputname','stiffness');
    if strcmp(Xop4.Sdescription,'Op4Extractor for Unit test')
    Vtest(itest)=true;         
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Check whether method extract is correct
itest=itest+1;
try
    Xop4 = Op4Extractor('Sdescription', 'Op4Extractor for Unit test',...
                'Sworkingdirectory',Sdirectory,...
                'Sfile','BEAM1_K.OP4', ....
                'Soutputname','stiffness'); 
    Tout = extract(Xop4);
    Vtest(itest)=true;         
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Check whether relativepath is used correctly
itest=itest+1;
try
   Xop4 = Op4Extractor('Sfile','BEAM1_K.OP4', ....
                 'Sworkingdirectory',Sdirectory,...
                'Srelativepath','UseRelativePath',...
                'Soutputname','stiffness');   
    Tout = extract(Xop4);
    Vtest(itest)=true;         
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of OP4Extractor (' datestr(now) ')'])
disp([' Successful: ' num2str(100*(sum(Vtest)/length(Vtest))) '%'])
disp('--------------------------------------------------------------------')
for i=1:length(Vtest)
    if Vtest(i)
        disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);  
    else
        disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
    end
end

if nargout>0
% Export name of the UnitTest
varargout{1}='OP4Extractor';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
