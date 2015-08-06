 function AddPathAndCreateInputs(testCase) % the inputs must be created to be able to test the MIO
%             testCase.addTeardown(@path, addpath(fullfile(OpenCossan.getCossanRoot,'UnitTest','MIO','FunctionForMio'))) % creates the path and enables the removal of the path before and after each unit test
%             StestPath = fullfile('UnitTest', 'MIO'); %sets the test path for the 
            Xrv1 = RandomVariable('Sdistribution', 'uniform', 'par1', 9,'par2', 11) %define the first random variable
            Xrv2 = RandomVariable('Sdistribution', 'uniform', 'par1', 14,'par2', 16) %defines the second random variable
            Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrandomvariables',{Xrv1,Xrv2}) % defines the random variable set
            Xin = Input %creates and input object
            Xin = add(Xin,Xrvs) %adds a random variable set to the Input objects
        end
