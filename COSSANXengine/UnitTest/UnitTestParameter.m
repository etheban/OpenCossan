%% UNIT TEST FOR @FUNCTION
% The class tested here can be found at: http://cossan.co.uk/wiki/index.php/@Parameter
%%  
% Author: Umar Razzaq
%%
% Individual tests have been split into their own sections to make it easy
% to find out what each test does.

%% xUnit sub-class defintion
% This sub-class inherits from matlab.unittest.TestCase 
classdef UnitTestParameter < matlab.unittest.TestCase
    
    % Unit Test for Parameter
    % Testing each option for @Parameter
    

   % Methods block: Place individual tests in this block
    methods (Test) % Turn Test attribute on

        %% test 1: verify Parameter returns correct output class
        % tests output of the object
        function testOutputParameter(testCase)
           Xpar = Parameter;
           testCase.verifyClass(Xpar, 'Parameter')
        end
       %%
       %
       % Status: Should Pass  
       
       
        %% test 2: verify number of elements
        %  Checks that the length of Parameter object is 1
        function testNumElementsParameter(testCase)
           Xpar = Parameter;
           testCase.verifyNumElements(Xpar, 1)
        end
       %%
       %
       % Status: Should Pass  
       
       
        %% test 3: check original value
        % Verify that original value is actually 0, i.ie Parameter.value is
        % actually empty
        function testEmptyParameter(testCase)
            Xpar = Parameter;
            testCase.verifyLength(Xpar.value, 0)
        end
        
        % test 4: verify field values cannot be empty? 
        % When new Parameter object is created, a value should also be
        % provided, if not an error should be throw
        % *** Nothing written in Wiki
        function testSdescriptionParameter(testCase)
            testCase.verifyError(@()Parameter('Sdescription','Parameters'), 'openCOSSAN:Parameter')
        end
       %%
       %
       % Status: Should Pass  as error gets thrown
       
        %% test 5: test value works
        % When inputting value of 5, it should return value of 5
        function testValueWorksParameter(testCase)
            Xpar = Parameter('value', 5);
            testCase.verifyEqual(Xpar.value, 5)
        end
       %%
       %
       % Status: Should Pass
       
       
        %% test 6: value tolerance when using floating point number
        % This checks how it stores floating point numbers to a low
        % tolerance
        function testValueFloatingParameter(testCase)
            Xpar = Parameter('value', 5.000000001);
            testCase.verifyEqual(Xpar.value, 5.000000001, 'RelTol', .000000001)
        end
       %%
       %
       % Status: Should Pass
       
       
       %% test 7: check incorrect input
       % a char is passed instead of a numeric value
       function testValueStringParameter(testCase)
            testCase.verifyError(@()Parameter('value', '5.000000001'), 'openCOSSAN:validateCOSSANInputs')
       end
       %%
       %
       % Status: Should Pass as error gets thrown       
        
        % test 9: negative numbers
        % Checks that negative numbers are accepted
        function testNegativeValueParameter(testCase)
            Xpar = Parameter('value', -9);
            testCase.verifyEqual(Xpar.value, -9);
        end
       %%
       %
       % Status: Should Pass
       
       %% test 10: handle
       % Uses verify fail to check that Parameter class does not inherit
       % from handle
        function testNotHandleParameter(testCase)
            Xpar = Parameter;
            if ishandle(Xpar)
                testCase.verifyFail('Should not be a handle class')
            end 
        end
       %%
       %
       % Status: Should Pass as the error never gets thrown
       
        %% test 11: check empty object Nelements
        % checks that Nlelements is originally zero
        function testEmptyNelementsParameter(testCase)
            Xpar = Parameter;
            testCase.verifyEqual(Xpar.Nelements, 0)
        end
       %%
       %
       % Status: Should Pass
       
        %% test 12: check 4 Vvector
        % checks Nelements counts vector of elements
        function test4NelementsParameter(testCase)
            Xpar  = Parameter('Vvalue',[1 2 3 4]);
            testCase.verifyEqual(Xpar.Nelements, 4);
        end
       %%
       %
       % Status: Should Pass
       
       %% test 13: test Mvalue, Vvalue and value
       % Passes both Mvalue, Vvalue at same time. This should fail
        function testNelementsx2Parameter(testCase) % only counts the value, not Mvalues and Vvalue
           testCase.verifyError(@()Parameter('Vvalue',[1 2 3 4], 'Mvalues', [58 54]),'openCOSSAN:Parameter:MultipleValueDefinition')
        end
       %%
       %
       % Status: Fails, as it only counts Nelements as 1
       
               
        %% test 14: mis-spell Sdescription
        % Pass incorrect input which should thrown an error
        function testWrongSdescriptionParameter(testCase)
            testCase.verifyError(@()Parameter('Nvalue', 8), 'openCOSSAN:Parameter:wrongArgument')
        end
       %%
       %
       % Status: Should Pass
       
        %% test 15:  mis-spell Vvalue
        % This test uses input key of Nvalue and passes a string.
        function testWrongSdescriptionx2Parameter(testCase)
            testCase.verifyError(@()Parameter('Nvalue', 'some string'), 'openCOSSAN:validateCOSSANInputs')
        end
       %%
       %
       % Status: Fails, as the error it displays is that 'The input value after the PropertyName Nvalue
       % must be an integer.' However it was Nvalue that was the incorrect input, hence it throws the wrong error     
        
        %% test 16: get maximum value
        % Puts in several values ad checks if the correct maximum value is
        % returned
        function testMaximumValueParameter(testCase)
            Xpar = Parameter('Mvalues', [4 5 6]);
            testCase.verifyEqual(max(Xpar.value), 6) 
        end
       %%
       %
       % Status: Should Pass
       
        %% test 17: Pass Mvalues non matrix
        % Passes a non-matrix for Mvalus, which should show an invalid
        % input error
        function testNonVectorMvaluesParameter(testCase)
            testCase.verifyError(@()Parameter('Mvalues', '45'), 'openCOSSAN:validateCOSSANInputs')
        end
        %%
       %
       % Status: Should Pass
       
       
        %% test 18: check display works
        % display method should be empty with verbosity level 0
        function testDisplayMethodParameter(testCase)
            Xpar1 = Parameter('Mvalues', [4 5 6]);          
            VlevelOld=OpenCossan.getVerbosityLevel;
            OpenCossan.setVerbosityLevel(0);
            testCase.verifyLength(@()evalc('Xpar1.display'),1)
            OpenCossan.setVerbosityLevel(VlevelOld);
        end
        %%
       %
       % Status: Should Pass
       
       
        %% test 19: outputs a class of Parameter:
        function testOutParameter(testCase)
            Xpar = Parameter();
            testCase.verifyClass(Xpar, 'Parameter');
        end
        %%
       %
       % Status: Should Pass        
        
        %% test 20: Vvalue takes input of doubles only
        % Pass an integer_64 as Vvalue input which should thrown an input
        % error
        function testVvalueDoubleParameter(testCase)
            try
                Parameter('Vvalue', int64(4) )
            catch err
                testCase.assertEqual(err.identifier, 'openCOSSAN:validateCOSSANInputs')
                err.identifier
            end
        end
        %%
       %
       % Status: Should Pass   
       
       
        %% test 21: expected output properties
        % this compares output properties for empty parameter objects
        function testExpPropertiesParameter(testCase)
            expProp = {'Sdescription'; 'value'; 'Nelements'};
            testCase.assertEqual(properties(Parameter()), expProp);
        end
        %%
       %
       % Status: Should Pass           
        
        %% test 22: test .set method
        % this checks that set has input checking, does not throw an error when set
        % is used
        function testSetMethodParameter(testCase)
            Xpar = Parameter();
            testCase.verifyWarning(@()Xpar.set('Svalue', 8), 'OpenCossan:Parameter:set:obsolete');
        end
        %%
       %
       % Status: Should Fail, although a message is show that 'The field 'Svalue' does not exist in Xpar'  
       % it does not show an error
       
        %% test 23: check display works
        % This works by saving the output of the Command Window to a file
        % which is then read. The string within that file is compared with
        % what a line of Output should contain.
        function checkDisplayWorksParameter(testCase)
            Xpar = Parameter();
            diary('outputWorks');
            Xpar.display()
            diary off 
            content = fileread('outputWorks');
            testCase.assertTrue(all(strfind(content, '* Number of elements: 0')) );
        end
        %%
       %
       % Status: Should Pass  
       
       
        %% test 24: large numbers
        % Does it take any number even before it runs out of memory?
        % This works by passing a very large matrix to Mvalues. This test
        % can cause crashes if the size of the matrix is increased any
        % larger. COSSAN should check the environment and have an upper
        % limit?
        function testLargeValuesParameter(testCase)
            try
                Parameter('Mvalues', 1:9999999) % dangerous, don't increase this number and there won't be an error
            catch err
                testCase.assertError(err.identifier, 'MATLAB:nomem');
            end
        end
        %%
       %
       % Status: Should Pass 
       
       %% test 25: testing for cvalue
       %  
       % Does it count the correct number Cvalues elements
	    function testCvalueParamter(testCase)
	    Xpar = Parameter('Cvalues', {4, 5, 6});
	    testCase.assertEqual(Xpar.Nelements, 3);
        end 
        %%
       %
       % Status: Fails as it only counts 1 element   

        
        
        
    end %  end test block
end % end class def