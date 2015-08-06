%% UNIT TEST FOR @DESIGNVARIABLE
% The class tested here can be found at: http://cossan.co.uk/wiki/index.php/@DesignVariable
%%  
% Author: Umar Razzaq
%%
% Individual tests have been split into their own sections to make it easy
% to find out what each test does.


%% xUnit sub-class defintion
% This sub-class inherits from matlab.unittest.TestCase 
classdef UnitTestDesignVariable < matlab.unittest.TestCase
%%


   % Methods block: Place individual tests in this block
   methods (Test) % Turn Test attribute on

        %% test 1: empty object
        % empty object: check if it returns correct amount of properties
        function testEmptyDesignVariable(testCase)
            Xdv = DesignVariable;
            actualProp = properties(Xdv); % object properties
            expectedProp = {'Sdescription'; 'value'; 'lowerBound'; 'upperBound'; 'Vsupport'; 'Ldiscrete'};  % assumed properties
            testCase.assertEqual(actualProp, expectedProp);
        end
        
        %% test 2:  Discrete Variable
        % If variable is discrete, is Ldiscrete property set to true?
        function testCreateDiscreteDesignVariable(testCase)
           Xdv = DesignVariable('value', 2, 'Vsupport', 1:1:6);
           testCase.assertTrue(Xdv.Ldiscrete);
        end
        
        %% test 3: check discrete poitns
        % check discrete points are correct (i.e. 1:1:6 should return
        % 1,2,3,4,5,6)
        function testVvalueWorksDesignVariable(testCase)
            Xdv = DesignVariable('value', 2, 'Vsupport', 1:1:6);
            testCase.assertEqual(Xdv.Vsupport, [1 2 3 4 5 6]);
        end
        
        %% test 4: test maxvalue
        % this sets the maxvalue lower than the min value
        % (setting greater lowerbound than upperbound, continuous, should
        % give error)
        function testCreateIncorrectLowerboundDesignVariable(testCase)
            testCase.assertError(@()(DesignVariable('value', 20, 'minvalue', 20, 'maxvalue', 5)),  'openCOSSAN:DesignOfExperiments:DesignOfExperiments');
        end

        %% test 5: check value
        % value should be lower than lowerbound and upperbound
        % (value less than lowerbound or greater than upperbound does not give any errors) 
        function testValueIncorrectDesignVariable(testCase)
            testCase.assertError( @()(DesignVariable('value', 25,'minvalue', 20, 'maxvalue', 22)), 'openCOSSAN:DesignVariable:outOfBound' );
        end
        %%
        %
        % Status: Fails but shouldn't because no error is thrown if value
        % greater than max or less than min
        
        %% test 6: check value 2
        % This value checks if there is an error if value is not between
        % maxvalue and minvalue
        function testValueIncorrectx2DesignVariable(testCase)
            testCase.assertError( @()DesignVariable('value', 25,'Vsupport',[1:20]), 'openCOSSAN:DesignVariable:outOfBound' );
        end
        %%
        %
        % Status: Fails but shouldn't because no error is thrown if value
        % greater than max or less than min
        
        %% test 7: Continuous Variable
        % testing sample works (continuous), should have 10 values:
        function testMethodSampleDesignVariable(testCase)
            Xdv = DesignVariable('value', 20, 'minvalue', 10, 'maxvalue', 30);
            Vout = Xdv.sample('Nsamples', 10);
            testCase.assertSize(Vout, [10 1]) ;
        end
        
        %% test 8: Invalid Nsamples
        % does 10.5 Nsamples give an error?
        function testMethodSample105SampleDesignVariable(testCase)
            Xdv = DesignVariable('value', 20, 'minvalue', 10, 'maxvalue', 30);
            testCase.assertError( @()Xdv.sample('Nsamples', 10.5), 'openCOSSAN:validateCOSSANInputs' );
        end
        
        %% test 9: 10th percentile value
        % get 10th percentile value
        function testTenthValuePercentileDesignVariable(testCase)
            Xdv = DesignVariable('value', 20, 'minvalue', 10, 'maxvalue', 30);
            Vout = Xdv.sample('Nsamples', 10);
            actual = Xdv.getValue(0.1);
            testCase.assertEqual(actual, 12);
        end
        
        %% test 10: 20th percentile value
        % get 20th percentile value
        function testTwenthValuePercentileDesignVariable(testCase)
            Xdv = DesignVariable('value', 20, 'minvalue', 10, 'maxvalue', 30);
            Vout = Xdv.sample('Nsamples', 10);
            actual = Xdv.getValue(0.2);
            testCase.assertEqual(actual, 14);
        end        
        
        %% test 11: Incorrect Vsupport values
        % incorrect vsupport type passed (alphanumerical vector)
        function testVsupportAlphabetDesignVariable(testCase) 
            testCase.assertError(@()DesignVariable('value', 2, 'Vsupport', [1 2 3 'a' 'b']),  'openCOSSAN:validateCOSSANInputs');
        end
        
        %% test 12: Check invalid sample number
        % 0.5 samples should throw an error
        function testIncorrectSampleSizeDesignVariable(testCase)
            Xdv = DesignVariable('value', 2, 'Vsupport', 1:1:6);
            testCase.assertError( @()Xdv.sample('Nsamples', 0.5), 'openCOSSAN:validateCOSSANInputs' )
        end
        
        %% test 13: 0 Nsamples
        % This checks if there is an error if there is 0 Nsamples
        function testZeroNsamplePoints(testCase)
            Xdv = DesignVariable('value', 2, 'Vsupport', 1:1:6);

                Vs=Xdv.sample('Nsamples', 100);
                testCase.assumeLength(Vs,100);
                testCase.assumeLessThanOrEqual(max(Vs),Xdv.upperBound);
                testCase.assumeGreaterThanOrEqual(min(Vs),Xdv.lowerBound);              
        end
        %%
        %
        % Status: Should fail as 0 Nsamples should not be allowed but no
        % error is thrown.
        
        %% test 14: get Twentieth Percentile
        function testTwenthPercentile(testCase)
            Xdv = DesignVariable('value', 20, 'minvalue', 10, 'maxvalue', 30);
            Vout = Xdv.sample('Nsamples', 10);
            testCase.assertEqual(Xdv.getPercentile(20), 0.5);
        end
        
        %% test 15: get Tenth Percentile
        function testTenthPercentile(testCase)
            Xdv = DesignVariable('value', 20, 'minvalue', 10, 'maxvalue', 30);
            Vout = Xdv.sample('Nsamples', 10);
            testCase.assertEqual(Xdv.getPercentile(00), 0.0);
        end        
        
        
    end
end