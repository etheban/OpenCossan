%% UNIT TEST FOR @INPUT
% The class tested here can be found at: http://cossan.co.uk/wiki/index.php/@Input
%%  
% Author: Umar Razzaq
%%
% Individual tests have been split into their own sections to make it easy
% to find out what each test does.


% Note setup and teardown fixtures weren't used as testCase.Xpar1 syntax seems to
% cause an error with COSSAN.


classdef UnitTestInput < matlab.unittest.TestCase

    
    
    % The Input object is uses to generate samples of random variables, collection
    % parameters, function and design variables
    % todo: check to see if I can access private properties (are they in
    % the wiki)
    
    
    % TODO: rewrite this to create a setup so I don't have to rewrite all
    % this stuff every bloody time
    
    properties
    end
    
    methods (Test)
        
        %% just checks output (should pass)
        function inputEmptyObject(testCase)
            Xin1 = Input;
            testCase.assertClass(Xin1, 'Input' );
        end
        
        %% checks constructor (should pass)
        function nsamplesEmptyObject(testCase)
            Xin1 = Input;
            testCase.assertEqual(Xin1.Nsamples, 0);
        end
        
        %% using add should return input object (should pass)
        function addParameterreturnInput (testCase)
            Xmat1 = Parameter('Sdescription', 'Material 1', 'Value', 7E+5);
            Xin = Input('Sdescription', 'Input Description');
            Xin = Xin.add(Xmat1);
            testCase.assertClass(Xin, 'Input')
        end
        
        %% initialize empty object
        function inputDescription(testCase)
            Xin = Input('Sdescription', 'Input Description');
            testCase.assertEqual(Xin.Sdescription, 'Input Description');
        end
        
        %% pass three parameters to it and check how many inputs it counts (Ninputs)
        function checkNinputProperty(testCase)
            Xmat1   = Parameter('Sdescription','material 1','value',5E+3);
            Xmat2   = Parameter('Sdescription','material 2','value',4E+4);
            Xmat3   = Parameter('Sdescription','material 3','value',3E+5);
            Xin = Input('Sdescription', 'Input Description');
            Xin = Xin.add(Xmat1); 
            Xin = Xin.add(Xmat2);
            Xin = Xin.add(Xmat3);
            testCase.assertEqual(Xin.Ninputs, 3);
        end
        
        %% same as previous test but checks that CnamesParameter works
        function checkCnamesParameter(testCase)
            Xmat1   = Parameter('Sdescription','material 1','value',5E+3);
            Xmat2   = Parameter('Sdescription','material 2','value',4E+4);
            Xmat3   = Parameter('Sdescription','material 3','value',3E+5);
            Xin = Input('Sdescription', 'Input Description');
            Xin = Xin.add(Xmat1); 
            Xin = Xin.add(Xmat2);
            Xin = Xin.add(Xmat3);
            % the output should be 'Xmat1', 'Xmat2', 'Xmat3'
            compare = strcmp(Xin.CnamesParameter,  {'Xmat1', 'Xmat2', 'Xmat3'});
            testCase.assertEqual(isequal(compare, [1 1 1]), true);
        end
        
        
        %% add same object twice, should give a warning that it is already in there
        % Warning: The object Xmat2 of type Parameter is already present in
        % (should pass)
        function addXmatTwice(testCase)    
            try
                Xmat1 = Parameter('Sdescription', 'Material 1', 'value', 7E+5);
                Xin = Input('Sdescription', 'Input Description');
                Xin = Xin.add(Xmat1);
                Xin = Xin.add(Xmat1);
            catch err
                testCase.assertEqual(error(lastwarn), 'The object Xmat1 of type Parameter is already present in the Input object');
            end
        end
        
        %%
        function addingRandomVariableObject(testCase) % should fail
            try
                x1      = RandomVariable('Sdistribution','normal','mean',2.763,'std',0.4);
                x2      = RandomVariable('Sdistribution','normal','mean',1.25,'std',0.4);
                Xin     = Input('Sdescription', 'Input Description');
                Xin     = Xin.add(x1);
                Xin     = Xin.add(x2);
            catch err
                testCase.assertEqual(err.identifier, 'openCOSSAN:inputs:Inputs:add')
            end
        end
        
        
        %% checking remove works, should just return an output object
        function removeworks(testCase)
            Xmat1       = Parameter('Sdescription','material 1','value',5E+3);
            Xin         = Input('Sdescription', 'Input Description');
            Xin         = Xin.add(Xmat1); 
            Xin         = Xin.remove(Xmat1);
            testCase.assertEqual(Xin.Xparameters, struct()); % empty struct
        end
        
        %% CHECK EACH PROPERTY ONE AT A TIME
       
        %% total number of random variables
        function testNrandomVariables(testCase) % should fail
            x1          = RandomVariable('Sdistribution','normal','mean',2.763,'std',0.4);
            x2          = RandomVariable('Sdistribution','normal','mean',1.25,'std',0.4);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin         = Input('Sdescription', 'Input Description');
            Xin         = Xin.add(Xrvs1);
            testCase.assertEqual(Xin.NrandomVariables, 2);
        end
        
        %% test total number of design variables
        function testDesignVariables(testCase)
            Xdv1        = DesignVariable('Sdescription','dummy','value',5);
            Xin         = Input('Sdescription', 'Input Description');
            Xin         = Xin.add(Xdv1);
            testCase.assertEqual(Xin.NdesignVariables, 1);
        end
        
        %% test total number of samples (TODO: CHANGE THIS, I don't know how much of this I need to create samples)
        function testNsamples(testCase)
            % create random variables
            Xrv1        = RandomVariable('Sdistribution','exponential','par1',1);
            Xrv2        = RandomVariable('Sdistribution','normal','mean',3,'std',1);
            Xrv3        = RandomVariable('Sdistribution','lognormal','mean',3,'std',1);

            % create random variables set:
            Mcorr       = [1.0,0.5,0.2;...
                           0.5,1.0,0.1;...
                           0.2,0.1,1.0];
                   
            Xrvs1       = RandomVariableSet('Cmembers',{'Xrv1','Xrv2','Xrv3'},...
                       'CXrv',{Xrv1,Xrv2,Xrv3},'Mcorrelation',Mcorr);
                       
            % create input objects:
            Xin         = Input;
            Xin         = add(Xin,Xrvs1);
            
            % Used to reproduce the results
            OpenCossan.resetRandomNumberGenerator(400)
            
            % generate samples:
            Xin         = Xin.sample('Nsamples',1e3);
            MX=Xin.Xsamples.MsamplesPhysicalSpace;            

            % actual test is here:
            testCase.assertEqual(Xin.Nsamples, 1000);
            testCase.assertLessThan(sum(MX(1,:)-[1.445321314998831e+00     4.114443094251939e+00     3.386157085587457e+00]),1e-6);
        end
        
        %% test Cnames (output: randomvars, functions, parameters, stochastic, design)
        
        function testCnames(testCase)
            
            % parameters
            Xin         = Input;
            Xmat1       = Parameter('Sdescription','material 1 E','value',7E+7);
            Xin         = Xin.add(Xmat1);
            
            % random variables
            x1          = RandomVariable('Sdistribution','uniform','lowerbound',0,'upperbound',10);
            x2          = RandomVariable('Sdistribution','uniform','mean',5,'std',1);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin         = Xin.add(Xrvs1);
            
            % functions
            Xfun1       = Function('Sdescription','function #1', ...
                                   'Sexpression','<&x1&>+<&x2&>');
            Xin         = Xin.add(Xfun1);
            
            % design variables
            Xdv1        = DesignVariable('Sdescription','dummy','value',5);
            Xin         = Xin.add(Xdv1);
   
            % We are simply using Stochastic Process not test it
            SP1 = StochasticProcess('Sdescription', 'Description', 'Mcoord',linspace(0,50,51), 'Vmean', 20,'Mcovariance', eye(51));
            SP1 = KL_terms(SP1, 'NKL_terms', 2);  

             Xin        = Xin.add(SP1);
             testCase.assertEqual(Xin.Cnames ,{ 'x1' 'x2' 'Xfun1' 'Xmat1' 'SP1' 'Xdv1'});
             testCase.assertEqual(Xin.CnamesStochasticProcess,{'SP1'});
             testCase.assertEqual(Xin.CnamesDesignVariable,{'Xdv1'});
             testCase.assertEqual(Xin.CnamesFunction,{'Xfun1'});
        end
        
        %% test CnamesRandomVariable
        function testCnamesRandomVariable(testCase) 
            Xin         = Input;
            % random variables
            x1          = RandomVariable('Sdistribution','uniform','lowerbound',0,'upperbound',10);
            x2          = RandomVariable('Sdistribution','uniform','mean',5,'std',1);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin         = Xin.add(Xrvs1);
            
            testCase.assertEqual(Xin.CnamesRandomVariable, {'x1', 'x2'})
        end

        %% test CnamesRandomVariableSet
        function testCnamesRandomVariableSet(testCase)
            Xin         = Input;
            % random variables
            x1          = RandomVariable('Sdistribution','uniform','lowerbound',0,'upperbound',10);
            x2          = RandomVariable('Sdistribution','uniform','mean',5,'std',1);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin         = Xin.add(Xrvs1);
            
            testCase.assertEqual(Xin.CnamesRandomVariableSet, {'Xrvs1'})
        end
        
        %% test CnamesDesignVariable
        
        function testDesignVariable(testCase)
            Xdv1        = DesignVariable('Sdescription','dummy','value',5);
            Xin         = Input('Sdescription', 'Input Description');
            Xin         = Xin.add(Xdv1);
            testCase.assertEqual(Xin.CnamesDesignVariable, {'Xdv1'});    
        end
        
        %% test CnamesFunction
        
        function testCnamesFunction (testCase)
        
            Xin         = Input;
            x1          = RandomVariable('Sdistribution','uniform','lowerbound',0,'upperbound',10);
            x2          = RandomVariable('Sdistribution','uniform','mean',5,'std',1);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin         = Xin.add(Xrvs1);
            
            Xfun1       = Function('Sdescription','function #1', ...
                                'Sexpression','<&x1&>+<&x2&>');
            Xin         = Xin.add(Xfun1);
            
            testCase.assertEqual(Xin.CnamesFunction, {'Xfun1'});  
        end
        
        %% test CnamesParameter
        
        function testCnamesParameter (testCase)
            Xin         = Input;
            Xmat1       = Parameter('Sdescription','material 1 E','value',7E+7);
            Xin         = Xin.add(Xmat1);

            testCase.assertEqual(Xin.CnamesParameter, {'Xmat1'})
        end
        
        %% test CnamesStochasticProcess
        
        
        %% test CnamesSet
        
        function testCnamesSet (testCase)
            
        end
        
        
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Checking Methods
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% GET
        %% checking get function with UNIMPLEMENTED randomvariableset,
        % SHOULD NOT PASS if feature gets implemented, but fails in
        % behaviour... if the verifyError was not there it should fail and
        % should fail. THIS SHOULD NOT FAIL
        function testGetFunction_xrvset(testCase)
            Xin         = Input;
            x1          = RandomVariable('Sdistribution','uniform','lowerbound',0,'upperbound',10);
            x2          = RandomVariable('Sdistribution','uniform','mean',5,'std',1);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin         = Xin.add(Xrvs1);
           
            % testCase.verifyClass(@()Xin.get('xrvset'), 'RandomVariable')
            
            testCase.verifyError(@()Xin.get('randomvariableset'), 'openCOSSAN:Input:get')   
        end
        
        %% checking get default values, should return mean values of random variables
        function testGetFunction_defaultValues(testCase)
            Xin         = Input;
            x1          = RandomVariable('Sdistribution','uniform','lowerbound',0,'upperbound',10);
            x2          = RandomVariable('Sdistribution','uniform','mean',5,'std',1);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin         = Xin.add(Xrvs1);   
            
            testCase.verifyEqual(Xin.get('defaultvalues'), struct('x1',5,'x2',5))
        end
        
        %% SET setting the mean
        % TODO: Test other set
        function testSetFunction_changeMean (testCase)
            Xin         = Input;
            x1          = RandomVariable('Sdistribution','uniform','lowerbound',0,'upperbound',10);
            x2          = RandomVariable('Sdistribution','uniform','mean',5,'std',1);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin         = Xin.add(Xrvs1);   
            
            Xin = Xin.set('SobjectName','x2','SpropertyName','mean','value',2);
            mean = Xin.get('Xrv','x2').mean;
            
            testCase.assumeEqual(mean,2)
        end
        
        %% setting the std: FAILS (should pass)
        function testSetFunction_changeStd (testCase)
            Xin         = Input;
            x1          = RandomVariable('Sdistribution','uniform','lowerbound',0,'upperbound',10);
            x2          = RandomVariable('Sdistribution','uniform','mean',5,'std',1);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin         = Xin.add(Xrvs1);   
            Xin = Xin.set('SobjectName','x2','SpropertyName','std','value',2);
            Xin = Xin.set('SobjectName','x2','SpropertyName','variance','value',4);
            Xin = Xin.set('SobjectName','x2','SpropertyName','mean','value',3);
            Xin = Xin.set('SobjectName','x1','SpropertyName','parameter1','value',5);
               
            testCase.assumeEqual(Xin.get('Xrv','x1').lowerBound, 5)
            testCase.assumeEqual(Xin.get('Xrv','x2').std, 2)
            testCase.assumeEqual(Xin.get('Xrv','x2').mean,3)
        end  
        
        %% change distribution to normal using set
        function testSetFunction_changeSdistribution (testCase)
            Xin         = Input;
            x1          = RandomVariable('Sdistribution','uniform','lowerbound',0,'upperbound',10);
            x2          = RandomVariable('Sdistribution','uniform','mean',5,'std',1);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin         = Xin.add(Xrvs1);  

            Xin = Xin.set('SobjectName','x2','SpropertyName','Sdistribution','Svalue','normal');
            testCase.assumeEqual(Xin.Xrvset.Xrvs1.Xrv{2}.Sdistribution,'NORMAL')
        end  
        
        %% change first parameter using set (ERRORS)
        
        function testSetParamter1 (testCase)
            Xin         = Input;
            x1          = RandomVariable('Sdistribution','uniform','lowerbound',0,'upperbound',10);
            x2          = RandomVariable('Sdistribution','uniform','mean',5,'std',1);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin         = Xin.add(Xrvs1);      
            Xin=Xin.set('SobjectName','x1','SpropertyName','parameter1','value',2); % should change lowerbound
            % Retrieve mean of X1
            [mean, ~]=Xin.getMoments('SobjectName','x1');            
            testCase.assertEqual(mean, 6)
            
        end
        
        %% testing ADD (should pass) and return class
        function testAddParameter(testCase)
            Xmat1   = Parameter('Sdescription','material 1 E','value',7E+7);
            Xmat2   = Parameter('Sdescription','material 2 E','value',2E+7);  
            Xin = Input;
            Xin = Xin.add(Xmat1);
            Xin = Xin.add(Xmat2);
            testCase.verifyClass(Xin, 'Input')
        end
        
        
        %% testing add by adding an unexpected object (should NOT fail as it is try catch)
        function testAddUnexpected(testCase)
           try
              s = struct('field1', 'value1', 'field2', 'value2');
              Xin = Input;
              Xin = Xin.add(s);
           catch err
               testCase.assertEqual(err.identifier, 'openCOSSAN:inputs:Inputs:add')
           end               
        end
        
        %% testing MERGE
        
        %% merging two empty input objects (doesnt do anything?), should check if Xin1 is then destroyed?
        function testMergeTwoEmpty (testCase)
            Xin1 = Input;
            Xin2= Input;
            Xin2 = Xin2.merge(Xin1);
            
            % TODO: add some checks 
            % If you merge 2 empty objects you obtain an empty object. 
            % Does not make much sense. Try to merge 2 Input (not empty) objects.
        end
        
        %% merging two objects with different parameters, Xin2 should now have 2 Parameter objects
        function testMergeTwoParameters(testCase)
            Xmat1   = Parameter('Sdescription','material 1 E','value',7E+7);
            Xmat2   = Parameter('Sdescription','material 2 E','value',2E+7);  
            Xin1 = Input; 
            Xin2 = Input;
            Xin1 = Xin1.add(Xmat1);
            Xin2 = Xin2.add(Xmat2);
            
            Xin2 = Xin2.merge(Xin1);
            
            testCase.assumeEqual(Xin2.CnamesParameter, {'Xmat2', 'Xmat1'})
        end
        
        %% merging two inputs with same random variable sets, nothing written about this in the wiki
        
        function testMergeTwoRandomSets (testCase)
            Xin1         = Input;
            x1          = RandomVariable('Sdistribution','uniform','lowerbound',0,'upperbound',10);
            x2          = RandomVariable('Sdistribution','uniform','mean',5,'std',1);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin1         = Xin1.add(Xrvs1); 

            Xin2         = Input;
            Xin2         = Xin2.add(Xrvs1); 


            testCase.verifyWarning(@()Xin2.merge(Xin1) ,'openCOSSAN:Inputs:merge')
        end
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% testing EvalulateFunction %%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        %% evaluate a simple function
        % TODO: This should be a unit test of Function 
        function testEvaluateFunction(testCase)
            Xmat1   = Parameter('Sdescription','material 1 E','value',7E+7);
            Xmat2   = Parameter('Sdescription','material 2 E','value',2E+7);  
            Xin = Input; 
            Xin = Xin.add(Xmat1)
            Xin = Xin.add(Xmat2)
            
            x1          = RandomVariable('Sdistribution','uniform','lowerbound',0,'upperbound',10);
            x2          = RandomVariable('Sdistribution','uniform','mean',5,'std',1);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin         = Xin.add(Xrvs1);   
            
            Xfun1       = Function('Sdescription','function #1', ...
                                   'Sexpression','<&x1&>+<&x2&>');
            Xin         = Xin.add(Xfun1);
            
            
%             Xmat1   = Parameter('Sdescription','material 1 E','value',7E+7);
% Xmat2   = Parameter('Sdescription','material 2 E','value',2E+7);
% Xmat3   = Parameter('Sdescription','material 3 E','value',1E+4);
% Xconfiguration  = Parameter('Sdescription','material configuration','Mvalue',unidrnd(3,16,16));
% 
% % Now we create RandomVariable and RandomVariableSet
% x1  = RandomVariable('Sdistribution','normal','mean',2.763,'std',0.4);
% x2  = RandomVariable('Sdistribution','normal','mean',1.25,'std',0.4);
% Xrvs1 = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
%             
%             Xfun2   = Function('Sdescription','function #2', ...
%                                 'Sexpression','<&Xmat3&>./<&x1&>');
%             Xfun3   = Function('Sdescription','function #2', ...
%                                  'Sexpression','<&Xmat3&>+1');

        end

        %% test getMoments:  retrieves the first two moments of the random variables present in the Input object. 
        function testGetMoments(testCase) % should get the two mean values
            Xin = Input;
            x1  = RandomVariable('Sdistribution','normal','mean',2.763,'std',0.4);
            x2  = RandomVariable('Sdistribution','normal','mean',1.25,'std',0.4);
            Xrvs1       = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            Xin         = Xin.add(Xrvs1);  
            moments = Xin.getMoments;
            testCase.verifyEqual(moments, [2.763 1.25])    
        end

       %% check remove works
       % should throw an error here
       function testRemovefromEmpty(testCase)
            x1 = 'string';
            Xin = Input;
            testCase.assumeError(@()Xin.remove(x1), 'openCOSSAN:Input:remove')  
       end
       
       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       %% checking input arguments
       %% 

       %% check Sdescription, should throw an error if not a string
       function testInputSdescriptionNonString(testCase)
            testCase.verifyError( @()Input('Sdescription', 5646547),  'openCOSSAN:validateCOSSANInputs')
       end
       
       
       %% Using CSmembers and CXmembers
       
       function testCSandCXmembersTogether(testCase)
            x1      = RandomVariable('Sdistribution','normal','mean',2.763,'std',0.4);
            x2      = RandomVariable('Sdistribution','normal','mean',1.25,'std',0.4);
            Xrvs1   = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            
            x3      = RandomVariable('Sdistribution','normal','mean',22.273,'std',0.9);
            x4      = RandomVariable('Sdistribution','normal','mean',2.65,'std',0.7);
            Xrvs2   = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x3 x4]);           
            
            Xin = Input('Sdescription', 'Object2');
            
            
            Xin = Input('CSmembers', {'XrvsN1' 'XrvsN2'}, 'CXmembers', {Xrvs1 Xrvs2});

            testCase.assumeEqual(Xin.CnamesRandomVariableSet, {'XrvsN1' 'XrvsN2'})
       end
       
       %% Using CSmembers and CXmembers not together
       
       function testCSandCXmembersNotTogether(testCase) 
            x1      = RandomVariable('Sdistribution','normal','mean',2.763,'std',0.4);
            x2      = RandomVariable('Sdistribution','normal','mean',1.25,'std',0.4);
            Xrvs1   = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
            
            x3      = RandomVariable('Sdistribution','normal','mean',22.273,'std',0.9);
            x4      = RandomVariable('Sdistribution','normal','mean',2.65,'std',0.7);
            Xrvs2   = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x3 x4]);           
            
            Xin = Input('Sdescription', 'Object2');

            testCase.assumeError( @()Input('CSmembers', {}, 'CXmembers', {Xrvs1 Xrvs2}), 'openCOSSAN:Input:WrongInputLength' )            
       end
       
       %% test Xfunction
       
       function testXparameter(testCase)
            Xmat1   = Parameter('Sdescription','material 1 E','value',7E+7);            
            Xin = Input('Xparameter', Xmat1);
            testCase.assumeEqual(Xin.CnamesParameter, {'Xmat1'});
       end
       
       %% test XrandomVariableSet
       
       function testXrandomVariableSet(testCase)
            x1      = RandomVariable('Sdistribution','normal','mean',2.763,'std',0.4);
            x2      = RandomVariable('Sdistribution','normal','mean',1.25,'std',0.4);
            Xrvs1   = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);
          
            Xin = Input('Xrandomvariableset', Xrvs1);
            
            testCase.assumeEqual(Xin.CnamesRandomVariableSet, {'Xrvs1'})            
       end
       
       %% test Xdesignvariable

       function testDesignVariableIO(testCase)
            Xdv1  = DesignVariable('Sdescription','dummy','value',5);
            Xin = Input('Xdesignvariable', Xdv1);
            testCase.assumeEqual(Xin.CnamesDesignVariable, {'Xdv1'})
       end
       
       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       %%
       % Everything above is related to the wiki and what it says the code
       % should do
       
       
       
    end
    
end

