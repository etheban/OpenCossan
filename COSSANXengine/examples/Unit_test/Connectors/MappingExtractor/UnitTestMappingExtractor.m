%% Unit Test
% HMP 

function varargout=UnitTestMappingExtractor

Ntest=4;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
Sdirectory = [OpenCossan.getCossanRoot '/examples/Unit_test/Connectors/MappingExtractor/'];

%% 1 Method display
itest=1;
try
   Xobj = MappingExtractor('Sdescription', 'MappingExtractor for Unit test',...
                'Sworkingdirectory',Sdirectory,...
                'Sfile','Ansys_K_NOMINAL.mapping',....
                'Soutputname','DOFs');   
    display(Xobj);
    Vtest(itest)=true; 
    Cmess{itest}='Creating MappingExtractor object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Check whether method extract is correct
itest=itest+1;
try
    Tout  = extract(Xobj);
    MDOFs = Tout.DOFs; %#ok<*NASGU>
    Vtest(itest)=true;   
    Cmess{itest}='Testing extract method';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
   Xobj = MappingExtractor('Sdescription', 'MappingExtractor for Unit test',...
                'Sworkingdirectory',Sdirectory,...
                'Soutputname','DOFs');   
catch ME
    Vtest(itest)=true; 
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
itest=itest+1;
try
   Xobj = MappingExtractor('Sdescription', 'MappingExtractor for Unit test',...
                'Sworkingdirectory',Sdirectory,...
                'Sfile','Ansys_K_NOMINAL.mapping');   
catch ME
    Vtest(itest)=true; 
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of MappingExtractor (' datestr(now) ')'])
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
varargout{1}='MappingExtractor';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end







