%% UNIT TEST for DOE

function varargout=UnitTestDesignOfExperiments

%% create the model
Xin   = Input('Sdescription','Input Object of our model');
RV1   = RandomVariable('Sdistribution','normal', 'mean',5, 'std',1); %#ok<*NASGU,SNASGU>
RV2   = RandomVariable('Sdistribution','normal', 'mean',15,'std',2); 
DV1   = DesignVariable('value',30,'minvalue',10,'maxvalue',50);
Xrvs1 = RandomVariableSet('Cmembers',{'RV1', 'RV2'},'CXrandomvariables',{RV1 RV2});
Xin   = add(Xin,Xrvs1);
Xin   = add(Xin,DV1);
Xm    = Mio('Sdescription', 'This is our Model', ...
    'Sscript','for j=1:length(Tinput),Toutput(j).out=2*Tinput(j).RV1-Tinput(j).RV2+3*Tinput(j).DV1; end', ...
    'Liostructure',true,...
    'Coutputnames',{'out'},...
    'Cinputnames',{'RV1','RV2','DV1'},...
    'Lfunction',false); 
Xeval = Evaluator('Xmio',Xm,'Sdescription','Evaluator for the DOE tutorial');
Xmdl  = Model('Xevaluator',Xeval,'Xinput',Xin);

%% start with the test

Ntest=17;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 Empty object
itest = 1;
try
    Xdoe=DesignOfExperiments;
    display(Xdoe)
    Vtest(itest) = true;
    Cmess{itest}='Create empty DOE Object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 test the constructor
itest =itest+ 1;
try
    Xdoe=DesignOfExperiments('Sdescription', 'DOE unit test #1',...
                             'SdesignType','FullFactorial','Vlevelvalues',[3],'Clevelnames',{'DV1'});
    display(Xdoe)
    Vtest(itest) = true;
    Cmess{itest}='Create DOE Object with Designtype';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 test the method sample
itest = itest + 1;
try
    Xsample = Xdoe.sample('Xinput',Xin);
    Vtest(itest) = true;
    Cmess{itest}='Testing sample method';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 test the method apply
itest = itest + 1;
try
    Xout = Xdoe.apply(Xmdl);
    display(Xout)
    Vtest(itest) = true;
    Cmess{itest}='Testing apply method - Sdesigntype: 2-level Full factorial';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 test the method apply
itest = itest + 1;
try
    Xdoe=DesignOfExperiments('SdesignType','BoxBehnken');
    Xout = Xdoe.apply(Xmdl);
    display(Xout)
    Vtest(itest) = true;
    Cmess{itest}='Testing apply method - Sdesigntype: BoxBehnken';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 test the method apply
itest = itest + 1;
try
    Xdoe=DesignOfExperiments('SdesignType','CentralComposite');
    Xout = Xdoe.apply(Xmdl);
    display(Xout)
    Vtest(itest) = true;
    Cmess{itest}='Testing apply method - Sdesigntype: CentralComposite (faced)';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 test the method apply
itest = itest + 1;
try
    Xdoe=DesignOfExperiments('SdesignType','CentralComposite','ScentralCompositeType','inscribed');
    Xout = Xdoe.apply(Xmdl);
    display(Xout)
    Vtest(itest) = true;
    Cmess{itest}='Testing apply method - Sdesigntype: CentralComposite (inscribed)';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 test the method apply
itest = itest + 1;
try
    Mdummy = ones(10,3);
    Xdoe=DesignOfExperiments('SdesignType','UserDefined','MdoeFactors',Mdummy);
    Xout = Xdoe.apply(Xmdl);
    display(Xout)
    Vtest(itest) = true;
    Cmess{itest}='Testing apply method - Sdesigntype: Userdefined';
    rmdir('2010*','s');
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 Wrong Sdesigntype - should fail
itest = itest + 1;
try
    Xdoe=DesignOfExperiments('SdesignType','dummy');
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 Wrong ScentralCompositeType - should fail
itest = itest + 1;
try
    Xdoe=DesignOfExperiments('SdesignType','CentralComposite','ScentralCompositeType','dummy');
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11 No Xinput provided for sample - should fail
itest = itest + 1;
try
    Xdoe=DesignOfExperiments('SdesignType','CentralComposite');
    Xsample = Xdoe.sample;
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12 No MdoeFactors provided for User Defined - should fail
itest = itest + 1;
try
    Xdoe=DesignOfExperiments('SdesignType','UserDefined');
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13 MdoeFactors with wrong dimension - should fail
itest = itest + 1;
try
    Mdummy = ones(2,2);
    Xdoe=DesignOfExperiments('SdesignType','UserDefined','MdoeFactors',Mdummy);
    Xout = Xdoe.apply(Xmdl);
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 14 No RVs or DVs defined in Xinput - should fail
itest = itest + 1;
try
    Xdoe=DesignOfExperiments('SdesignType','BoxBehnken');
    Xin2 = Input('Sdescription','Input Object of our model');
    Xsample = Xdoe.sample('Xinput',Xin2);
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 15 Perturbance parameter should be positive - should fail
itest = itest + 1;
try
    Xdoe=DesignOfExperiments('SdesignType','BoxBehnken','perturbanceParameter',-2);
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 16  RVs and DVs defined in Xinput 
itest = itest + 1;
try
    Xdoe=DesignOfExperiments('SdesignType','UserDefined', ...
        'MdoeFactors',[5 2 1; 1 2 3],'ClevelNames',{'RV2' 'DV1' 'RV1'});
    display(Xdoe)
    Xout=Xdoe.apply(Xmdl);
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 17  RVs and DVs defined in Xinput wrong name
itest = itest + 1;
try
    Xdoe=DesignOfExperiments('SdesignType','UserDefined', ...
        'MdoeFactors',[5 2 1; 1 2 3],'ClevelNames',{'RV2' 'DV2' 'RV1'});
    display(Xdoe)
    Xout=Xdoe.apply(Xmdl);
    
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



%% Finish the test
if nargout>0
    % Export name of the UnitTest
    varargout{1}='DesignOfExperiments';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of DesignOfExperiments (' datestr(now) ')'])
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
