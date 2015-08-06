function varargout = UnitTestMarkovChain
%UnitTestMarkovChain

% Perallocate memory

Ntest=13;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

%% 1 Create Preliminary objects
itest = itest+1;
try
    XrvN=RandomVariable('Sdistribution','normal','mean',0,'std',1);
    Xrvsbase = RandomVariableSet('Xrv',XrvN,'Nrviid',3);
    XrvU=RandomVariable('Sdistribution','uniform','lowerbound',-0.5,'upperbound',0.5);
    Xrvsoff = RandomVariableSet('Xrv',XrvU,'Nrviid',3);
    Xin=Input('Xrvset',Xrvsbase);
    Xin=sample(Xin,'Nsamples',10);
    Xs=Xin.Xsamples;
    Vtest(itest)=true;
    Cmess{itest}='Prepre preliminary objects';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create an empty object
itest = itest+1;
try
    Xobj = MarkovChain;
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 3 constructor
itest = itest+1;
try
    
    Xmkv1=MarkovChain('XtargetDistribution',Xrvsbase, ...
        'Xoffsprings',Xrvsoff, ...
        'Xsamples',Xs,'Npoints',5);
    display(Xmkv1)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 4 constructor, offsprings
itest = itest+1;
try
    Xmkv2=MarkovChain('Xinput',Xin,'Xoffsprings',Xrvsoff,'Npoints',8);
    display(Xmkv2)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 5 constructor, thin
itest = itest+1;
try
    Xmkv2=MarkovChain('Xinput',Xin,'Xoffsprings',Xrvsoff,'Npoints',8,'thin',3);
    display(Xmkv2)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 6 constructor, burnin
itest = itest+1;
try
    Xmkv2=MarkovChain('Xinput',Xin,'Xoffsprings',Xrvsoff,'Npoints',8,'burnin',3);
    display(Xmkv2)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 should fail
itest = itest+1;
try
    Xmkv2=MarkovChain('Stuutsk','dkuudkz');
    disply(Xmkv2)
catch ME
    Vtest(itest)=true;
    Cmess{itest}='This should fail';
end



%% 8 buildChain
itest = itest+1;
try
    Xmkv1=MarkovChain('XtargetDistribution',Xrvsbase, ...
        'Xoffsprings',Xrvsoff, ...
        'Xsamples',Xs,'Npoints',5);
    Xmkv1=Xmkv1.buildChain(5);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



%% 9 buildChain, no input
itest = itest+1;
try
    Xmkv1=MarkovChain('XtargetDistribution',Xrvsbase, ...
        'Xoffsprings',Xrvsoff, ...
        'Xsamples',Xs,'Npoints',5);
    Xmkv1=Xmkv1.buildChain();
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end




%% 10 add
itest = itest+1;
try
    
    Xmkv2=Xmkv2.add('Npoints',2);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



%% 11 add, no input
itest = itest+1;
try
    
    Xmkv2=Xmkv2.add();
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 12 remove
itest = itest+1;
try
    
    Xmkv2=Xmkv2.remove('Vchain',[1 2]);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 13 remove, no input
itest = itest+1;
try
    Xmkv2=Xmkv2.remove();
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%%
if nargout>0
    % Export name of the UnitTest
    varargout{1}='MarkovChain';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
    
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of the MarkovChain (' datestr(now) ')'])
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



