%% UNIT TEST FOR @FUNCTION
% The class tested here can be found at: http://cossan.co.uk/wiki/index.php/@Function
%%  
% Author: Umar Razzaq
%%
% Individual tests have been split into their own sections to make it easy
% to find out what each test does.


% Note setup and teardown fixtures weren't used as testCase.Xpar1 syntax seems to
% cause an error with COSSAN.
%% xUnit sub-class defintion
% This sub-class inherits from matlab.unittest.TestCase 
classdef UnitTestFunction < matlab.unittest.TestCase
%%


   % Methods block: Place individual tests in this block
   methods (Test) % Turn Test attribute on
       
       %% test 1: empy @Function class
       % Should return empty @Function class
       function testEmptyFunction(testCase)
       % TESTEMPTYFUNCTION tests an empty @Function object
            testCase.assertClass(Function(), 'Function'); % qualification method
       end
       %%
       % Expected Answer = 'Function'
       %
       % Status: Should Pass
       
       
       %% test 2: empty Function output properties
       % Should return properties of Function object
       function testEmptyPropertiesFunction(testCase)
            expProp = {'Sdescription'; 'Sfield'; 'Sexpression'; 'Ctoken'; 'Cinputnames'};
            testCase.assertEqual(properties(Function()), expProp);
       end
       %%
       % Expected Answer = {'Sdescription'; 'Sfield'; 'Sexpression'; 'Ctoken'; 'Cinputnames'}
       %
       % Status: Should Pass
       
       
       %% test 3: add input 
       % Should throw an expection
       function testWrongInputFuction(testCase)
           testCase.assertError(@()Parameter('kDescription', 'some description'), 'openCOSSAN:validateCOSSANInputs');
       end
       %%
       %
       % Status: Should Pass       
       %% test 4: add two parameters
       % Should return sum of two Parameters
       function testSumXparFunction(testCase)
           Xin = Input();
           
           Xpar1 = Parameter('Sdescription', 'first' ,'value', 1);
           Xpar2 = Parameter('Sdescription', 'second' ,'value', 2);
            
           Xfun = Function('Sexpression', '<&Xpar1&>+<&Xpar2&>');
            
           Xin = add(Xin, Xpar1);
           Xin = add(Xin, Xpar2);
           
           Xin = add(Xin, Xfun);
           
           actual = Xfun.evaluate(Xin);
           testCase.assertEqual(actual, 3);
       end
       %%
       %
       % Status: Should Pass       
       
       %% test 5: multiply two parameters
       % should return mulitplication of two parameters
       function testMultiplyXparFunction(testCase)
           Xin = Input();
           
           Xpar1 = Parameter('Sdescription', 'first' ,'value', 1);
           Xpar2 = Parameter('Sdescription', 'second' ,'value', 2);
            
           Xfun = Function('Sexpression', '<&Xpar1&>*<&Xpar2&>');
            
           Xin = add(Xin, Xpar1);
           Xin = add(Xin, Xpar2);
           
           Xin = add(Xin, Xfun);
           
           actual = Xfun.evaluate(Xin);
           testCase.assertEqual(actual, 2);
       end       
       %%
       %
       % Status: Should Pass       
       
       %% test 6: add two imaginary values
       % should return 2i
       function testSumImaginaryXparFunction(testCase)
            Xin = Input();
           
           Xpar1 = Parameter('Sdescription', 'first' ,'value', 1i);
           Xpar2 = Parameter('Sdescription', 'second' ,'value', 1i);
            
           Xfun = Function('Sexpression', '<&Xpar1&>+<&Xpar2&>');
            
           Xin = add(Xin, Xpar1);
           Xin = add(Xin, Xpar2);
           
           Xin = add(Xin, Xfun);
           
           actual = Xfun.evaluate(Xin);   
           testCase.assertEqual(actual, 0.0000 + 2.0000i)
       end
       %%
       %
       % Status: Should Pass
       
       %% test 7: addition for array values
       % Simply adds each ith value for one to another
       function testAddArrayXparFunction(testCase)
            Xin = Input();
           
           Xpar1 = Parameter('Sdescription', 'first' ,'Vvalue', [1 2 3 4]);
           Xpar2 = Parameter('Sdescription', 'second' ,'Vvalue', [5 6 7 8]);
            
           Xfun = Function('Sexpression', '<&Xpar1&>+<&Xpar2&>');
            
           Xin = add(Xin, Xpar1);
           Xin = add(Xin, Xpar2);
           
           Xin = add(Xin, Xfun);
           
           actual = Xfun.evaluate(Xin);   
           testCase.assertEqual(actual, [6 8 10 12]);
       end       
       %%
       %
       % Status: Should pass
       
       %% test 8: random variable set
       % add a random variable to a parameter
       function testAddRvsXparFunction(testCase)
           Xin = Input();
           
           Xpar1 = Parameter('Sdescription', 'first' ,'value', 1);
           Xrv1 = RandomVariable('Sdistribution', 'normal', 'mean', 2, 'std', 3);   %#ok<*NASGU>
           Xrvs = RandomVariableSet('Cmembers', {'Xrv1'},'CXrv',{Xrv1}); 
           Xfun = Function('Sexpression', '<&Xpar1&>+<&Xrv1&>');
            
           Xin = add(Xin, Xpar1);
           Xin = add(Xin, Xrvs);
           
           Xin = add(Xin, Xfun);
           
           Xin = sample(Xin, 'Nsamples', 1);
           
           actual = Xfun.evaluate(Xin);   
           exp = Xin.getValues('Sname', 'Xrv1');
           testCase.assertEqual(actual, exp+1);
       end        
       %%
       %
       % Status: errors with following details:
       % Error using RandomVariableSet (line 336)
       % the RandomVariableSet can not be build because not all the random variable are present in the workspace
       % Will pass if Xrv1 is present in the workspace.
       
       %% test 9: test complex numbers
       function testComplexNumbersFunction(testCase)
            Xin = Input();
           
           Xpar1 = Parameter('Sdescription', 'first' ,'Vvalue', [1 2 3 4]);
           Xpar2 = Parameter('Sdescription', 'second' ,'Vvalue', [5 6 7 8]);
            
           Xfun = Function('Sexpression', 'complex(<&Xpar1&>,<&Xpar2&>)');
            
           Xin = add(Xin, Xpar1);
           Xin = add(Xin, Xpar2);
           
           Xin = add(Xin, Xfun);
           
           actual = Xfun.evaluate(Xin);   
           testCase.assertEqual(actual, [1 + 5i, 2 + 6i, 3 + 7i, 4 + 8i]);
       end              
       %%
       %
       % Status: Should pass
       
       
       %% test 10: invalid syntax
       % should throw an exception if invalid syntax is used in function
       function testInvalidSyntaxFunction(testCase)
           try
                Xin = Input();
           
                Xpar1 = Parameter('Sdescription', 'first' ,'Vvalue', [1 2 3 4]);
                Xpar2 = Parameter('Sdescription', 'second' ,'Vvalue', [5 6 7 8]);
            
                Xfun = Function('Sexpression', 'pi.^<&Xpar1&>p<&Xpar2&>');
            
                Xin = add(Xin, Xpar1);
                Xin = add(Xin, Xpar2);
           
                Xin = add(Xin, Xfun);
           
                actual = Xfun.evaluate(Xin);   
           catch err 
              testCase.assertEqual(err.identifier, 'openCOSSAN:Function:evaluatefunction'); 
           end
               
       end
       %%
       %
       % Status : Should Pass
       
       %% test 11: function that references another function
       % Perform calculation using another function, should calculate                     
       % ((2*2) + 6) * 2) = 20
       function testMultiFuncFunction(testCase)
            Xin = Input();
           
           Xpar1 = Parameter('Sdescription', 'first' ,'Vvalue', [1 2 3 4]);
           Xpar2 = Parameter('Sdescription', 'second' ,'Vvalue', [5 6 7 8]);
            
           Xfunc1 = Function('Sexpression', '2*<&Xpar1&>+<&Xpar2&>');
           Xfunc2 = Function('Sexpression', '2*<&Xfunc1&>(2)');
           
           Xin = add(Xin, Xpar1);
           Xin = add(Xin, Xpar2);
           
           Xin = add(Xin, Xfunc1);
           Xin = add(Xin, Xfunc2);
           
           actual1 = Xfunc1.evaluate(Xin);   
           actual2 = Xfunc2.evaluate(Xin);
           testCase.assertEqual(actual2, 20);
       end     
       %%
       % 
       % Should Pass
       
       %% test 12: adding two random variables
       %
       function testAddTwoXrvFunction(testCase)
           Xin = Input();
           
           Xrv1 = RandomVariable('Sdistribution', 'normal', 'mean', 2, 'std', 3);  
           Xrv2 = RandomVariable('Sdistribution', 'normal', 'mean', 3, 'std', 4); 
           Xrvs = RandomVariableSet('Cmembers', {'Xrv1', 'Xrv2'},'CXrv',{Xrv1, Xrv2}); 

           Xin = add(Xin, Xrvs);

           Xfun = Function('Sexpression', '<&Xrv1&>+<&Xrv2&>');
           Xin = add(Xin, Xfun);
           
           Xin = sample(Xin, 'Nsamples', 1);
           
           actual = Xfun.evaluate(Xin);   
           exp1 = Xin.getValues('Sname', 'Xrv1');
           exp2 = Xin.getValues('Sname', 'Xrv2');
           testCase.assertEqual(actual, exp1+exp2);           
       end
       %%
       %
       % Status: Errors due to following:
       %  Error using RandomVariableSet (line 336)
       % the RandomVariableSet can not be build because not all the random variable are present in the workspace
       
       
       %% test 13: testing method getMembers
       % should return set of members as Xpar1, Xpar2 and type as Parameter
       function testgetMembersMethodFunction (testCase)
            Xin = Input();
           
           Xpar1 = Parameter('Sdescription', 'first' ,'Vvalue', [1 2 3 4]);
           Xpar2 = Parameter('Sdescription', 'second' ,'Vvalue', [5 6 7 8]);
            
           Xfun = Function('Sexpression', 'complex(<&Xpar1&>,<&Xpar2&>)');
            
           Xin = add(Xin, Xpar1);
           Xin = add(Xin, Xpar2);
           
           Xin = add(Xin, Xfun);
           
           Xfun.evaluate(Xin); 
           
           [m, t] = getMembers(Xfun);
           testCase.assertEqual(m, {'Xpar1';'Xpar2'});
           testCase.assertEqual(t, {'Parameter'; 'Parameter'})
       end
       %%
       %
       % Status: Should Pass
       %
       
   end % end methods block
   
   
    
end % end class definition
