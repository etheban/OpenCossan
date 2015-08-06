%% Unit Test
% HMP 

function varargout=UnitTestPunchExtractor

Ntest=4;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
Sdirectory = [OpenCossan.getCossanRoot '/examples/Unit_test/Connectors/PunchExtractor/'];

%% 1 Method display
itest=1;
try
    Xpch = PunchExtractor('Sdescription', 'PunchExtractor for Unit test',...
                'Sworkingdirectory',Sdirectory,...
                'Sfile','BEAM1_DOFS.PCH', ....
                'Soutputname','dofs');
    display(Xpch);
    Vtest(itest)=true;         
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Check whether Sdescription is correct
itest=itest+1;
try
    Xpch = PunchExtractor('Sdescription', 'PunchExtractor for Unit test',...
                'Sworkingdirectory',Sdirectory,...
                'Sfile','BEAM1_DOFS.PCH', ....
                'Soutputname','dofs');
    if strcmp(Xpch.Sdescription,'PunchExtractor for Unit test')
    Vtest(itest)=true;         
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Check whether method extract is correct
itest=itest+1;
try
    Xpch = PunchExtractor('Sdescription', 'PunchExtractor for Unit test',...
                'Sfile','BEAM1_DOFS.PCH','Sworkingdirectory',Sdirectory,....
                'Soutputname','dofs');
 
    load nodesdofs Mnodesdofs     
    Tout = extract(Xpch);
    if all(Tout.dofs-Mnodesdofs<eps)
    Vtest(itest)=true;         
    end

catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 Check whether relativepath is used correctly
itest=itest+1;
try
    Xpch = PunchExtractor('Sdescription', 'PunchExtractor for Unit test',...
                'Sfile','BEAM1_DOFS1.PCH','Sworkingdirectory',Sdirectory, ....
                'Srelativepath','UseRelativePath',...
                'Soutputname','dofs');

    load nodesdofs Mnodesdofs          
    Tout = extract(Xpch);
    if all(Tout.dofs-Mnodesdofs<eps)
        Vtest(itest)=true;         
    end

catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of PunchExtractor (' datestr(now) ')'])
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
varargout{1}='PunchExtractor';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
