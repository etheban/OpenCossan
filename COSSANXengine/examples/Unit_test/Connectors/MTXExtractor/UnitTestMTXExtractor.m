%% Unit Test
% HMP 

function varargout=UnitTestMTXExtractor

Ntest=4;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 Method display
itest=1;
try
    Xmtx = MTXExtractor('Sdescription', 'MTXExtractor for Unit test',...
                'Sfile','2D_Truss_MINP_STIF2.mtx', ....
                'Soutputname','stiffness');
    display(Xmtx);
    Vtest(itest)=true;         
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Check whether Sdescription is correct
itest=itest+1;
try
    Xmtx = MTXExtractor('Sdescription', 'MTXExtractor for Unit test',...
                'Sfile','2D_Truss_MINP_STIF2.mtx', ....
                'Soutputname','stiffness');
    Vtest(itest)=true;         
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Check whether method extract is correct
itest=itest+1;
try
    Xmtx = MTXExtractor('Sdescription', 'MTXExtractor for Unit test',...
                'Sfile','2D_Truss_MINP_STIF2.mtx', ....
                'Soutputname','stiffness');     
    Tout = extract(Xmtx);
    Vtest(itest)=true;         
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 Check whether relativepath is used correctly
itest=itest+1;
try
    Xmtx = MTXExtractor('Sdescription', 'MTXExtractor for Unit test',...
                'Sworkingdirectory',[OpenCossan.ScossanRoot '/examples/Unit_test/Connectors/MTXExtractor/'], ...
                'Sfile','2D_Truss_MINP_STIF.mtx', ....
                'Srelativepath','UseRelativePath',...
                'Soutputname','stiffness');
    Tout = extract(Xmtx);
    Vtest(itest)=true;         
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of MTXExtractor (' datestr(now) ')'])
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
varargout{1}='MTXExtractor';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
