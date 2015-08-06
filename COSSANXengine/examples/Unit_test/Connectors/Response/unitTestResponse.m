function varargout = unitTestResponse
%% Unit Test object Response
% EP
Ntest=8;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% Test Constructors
%% 1
itest=1;
try
    Xresp = Response('Sname', 'OUT3', ...
        'Sfieldformat', '%10e', ...
        'Clookoutfor',{'N O D E   O U T P U T'}, ...
        'Ncolnum',30, ...
        'Nrownum',11);
    display(Xresp)
    % This constructor should succed
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Test 2
itest=itest+1;
try
    % minimal information
    Xresp = Response('Sname', 'OUT3');
    display(Xresp)
    Vtest(itest)=true;
    if ~strcmp(Xresp.Sfieldformat,'%10e')
        Vtest(itest)=false;
        Cmess{itest}='Wrong default value of the Sfieldformat';
    end
    
    if Xresp.Ncolnum~=1
        Vtest(itest)=false;
        Cmess{itest}='Wrong default value of the Ncolnum';
    end
    
    if Xresp.Nrownum~=1
        Vtest(itest)=false;
        Cmess{itest}='Wrong default value of the Nrow';
    end
    
    if Xresp.Nrepeat~=1
        Vtest(itest)=false;
        Cmess{itest}='Wrong default value of the Nrepeat';
    end
    % This constructor should succed
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% Test 3
itest=itest+1;
try
    Xresp = Response('Sname', 'OUT3', ...
        'Sfieldformat', '%10e', ...
        'Clookoutfor',{'string1' 'string2' 'string3'}, ...
        'Ncolnum',30, ...
        'Nrownum',11);
    display(Xresp)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



%% Test 4
itest=itest+1;
try
    Xresp = Response(...
        'Sfieldformat', '%10e', ...
        'Clookoutfor',{'N O D E   O U T P U T'}, ...
        'Ncolnum',30, ...
        'Nrownum',11);
    display(Xresp)
    Cmess{itest}='No name assigned to Sname';
catch ME
    % This constructor should fail
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Test 5
itest=itest+1;
try
    Xresp = Response('Sfieldformat', '%10e', ...
        'Sname','pippo',...
        'Svarname','PinkCow', ...
        'Ncolnum',30, ...
        'Nrownum',11);
    display(Xresp)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Test 6
itest=itest+1;
try
    Xresp = Response('RedCow',...
        'Sregexpression','<?*PinkCow*>');
    Cmess{itest}='This should fail, wrong number of input arguments';
catch ME
    % This should fail
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Test 7
itest=itest+1;
try
    Xresp = Response('Sname','RedCow',...
        'Sregexpression','<?*PinkCow*>');
    Vtest(itest)=true;
    if ~strcmp(Xresp.Sregexpression,'<?*PinkCow*>')
        Vtest(itest)=false;
        Cmess{itest}='Sregexpression not assigned correctly';
    end
    display(Xresp)
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Test 8
itest=itest+1;
try
    Xresp = Response('Sname','RedCow',...
        'Sfieldformat', '%10e', ...
        'Svarname','PinkCow', ...
        'Ncolnum',30, ...
        'Nrownum',11);
    display(Xresp)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% Finalize the test

if nargout>0
    % Export name of the UnitTest
    varargout{1}='Response';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    %% Show summary of the test
    OpenCossan.cossanDisp('--------------------------------------------------------------------')
    OpenCossan.cossanDisp([' Unit Test of Extractor (' datestr(now) ')'])
    OpenCossan.cossanDisp('--------------------------------------------------------------------')
    for i=1:length(Vtest)
        if Vtest(i)
            OpenCossan.cossanDisp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
        else
            OpenCossan.cossanDisp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
        end
    end
end
