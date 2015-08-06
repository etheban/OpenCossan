%% Unit Test
% HMP 

function varargout=UnitTestHBExtractor

Ntest=5;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
Sdirectory = [OpenCossan.getCossanRoot '/examples/Unit_test/Connectors/HBExtractor/'];

%% 1 Method display
itest=1;
try
   Xhb = HBExtractor('Sdescription', 'HBExtractor for Unit test',...
                 'Sworkingdirectory',Sdirectory,...
                'Sfile','Building_K_NOMINAL', ....
                'Soutputname','stiffness');   
    display(Xhb);
    Vtest(itest)=true; 
    Cmess{itest}='Creating HBExtractor object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Check whether method extract is correct
itest=itest+1;
try
    Tout = extract(Xhb);
    MK = Tout.stiffness; %#ok<*NASGU>
    Vtest(itest)=true;   
    Cmess{itest}='Testing extract method';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
   Xhb = HBExtractor('Sworkingdirectory',Sdirectory,...
                'Sfile','dummy', ....
                'Soutputname','stiffness');   
catch ME
    Vtest(itest)=true; 
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 4
itest=itest+1;
try
   Xhb = HBExtractor('Sworkingdirectory',Sdirectory,...
                'Sfile','Building_K_NOMINAL');   
catch ME
    Vtest(itest)=true; 
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 5
itest=itest+1;
try
   Xhb = HBExtractor('Sworkingdirectory',Sdirectory,...
                'Soutputname','stiffness');   
catch ME
    Vtest(itest)=true; 
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of HBExtractor (' datestr(now) ')'])
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
varargout{1}='HBExtractor';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end







