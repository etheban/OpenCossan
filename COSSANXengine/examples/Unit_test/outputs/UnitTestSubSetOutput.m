function varargout = UnitTestSubSetOutput
%UNITTESTEVALUATOR Summary of this function goes here
%   Detailed explanation goes here

% Test SubSetOutput
% Perallocate memory

Ntest=6;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

%% 1 Create an empty object
itest = itest+1;
try
    Xobj1 = SubSetOutput;
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create an empty object
itest = itest+1;
try
    Xobj1 = SubSetOutput('Sfleubleubleu','gropkürgwokjkgr');
    display(Xobj)
    Cmess{itest}='This sould fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 3 constructor
itest = itest+1;
try
    Xobj1 = SubsetOutput('Sperformancefunctionname','Vg','Vpfl',[1 2 3],'Vcovpfl',[1 2 3],'Vrejectionrates',[1 2 3]);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
itest = itest+1;
try
    Xobj2 = SubsetOutput('Sperformancefunctionname','Vg','Vpfl',[1 2 3  4 5 3],'Vcovpfl',[1 2 3],'vrejectionrates',[1 2 3]);
catch ME
    Vtest(itest)=true;
        Cmess{itest}=[ME.identifier ' -- ' ME.message]; 
end



%% 5 invalid merge
itest = itest+1;
try
    Xobj2 = merge(Xobj2,Xobj1);
catch ME
    Vtest(itest)=true;
        Cmess{itest}=[ME.identifier ' -- ' ME.message]; 
end


%% 6 invalid merge
itest = itest+1;
try
    A=rand(5,3);
    Tstruct=cell2struct(num2cell(A),{'RV1', 'RV2','Xrv3'},2); 
    Xsd =  SimulationData('Cnames',{'RV1'; 'RV2';'Xrv3'},...
        'Tvalues',Tstruct,'Mvalues',A);
    Xobj2 = merge(Xobj2,Xobj1);
catch ME
    Vtest(itest)=true;
        Cmess{itest}=[ME.identifier ' -- ' ME.message]; 
end




if nargout>0
    % Export name of the UnitTest
    varargout{1}='SubSetOutput';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
    
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of the SubSetOutput (' datestr(now) ')'])
    disp([' Successful: ' num2str(100*(sum(Vtest)/length(Vtest))) '%'])
    disp('--------------------------------------------------------------------')
    for i=1:length(Vtest)
        if Vtest(i)
            disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
        else
            disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
        end
    end
    
end



