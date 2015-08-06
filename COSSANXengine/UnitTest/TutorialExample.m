%% TutorialExample
%This script gives a guide to writing a tutorial for OpenCOSSAN.
%The importance of keeping tutorials in the uniform style described in this
%tutorial is so that an automated unit testing strategy may be implimented
%on all the tutorials for Open COSSAN.

%Please do not name any scripts as Tutorial*...*.m unless they adhere to
%this format.

%In this example tutorial we will simply be finding the roots of the
%equation y = x^2 + 7x + 12.

%% Prepare Tutorial To Run
%Please ensure any other scripts that require running in order for the main
%tutorial to function correctly are run at this stage. Also set any other
%conditions that are required for the tutorial to run at this stage, e.g.
%resetting the random seed.

%NOTE: In this example no prior scripts require running so we will move
%stright on.

%% Output for Unit Testing
%So that the automated unit testing system can function correctly, every
%tutorial must output a structure named 'TutUnitTest' with fields
%'VariableNames', 'Expected', 'Tolerance' and 'Actual'.

TutUnitTest = struct('VariableNames',[],'Expected',[],...
                       'Tolerance',[],'Actual',[]); %Initialising the
                                                    %structure.

%TutUnitTest.VariableNames should include the names of all the variables
%that require testing in a cell array.
TutUnitTest.VariableNames = {'rt(1)','rt(2)'};

%TutUnitTest.Expected should include the expected contents of the variables
%being tested in a cell array.
TutUnitTest.Expected      = {-4,-3};

%TutUnitTest.Tolerance should include values for the +/- tolarances to be
%used when comparing the expected and actual results. Again the results
%should be included in a cell array.
TutUnitTest.Tolerance     = {sqrt(eps),sqrt(eps)};



%% Main Sequence
%Construct the tutorials code, explaining the processes being performed in
%as much detail as possible by using sections and comments.

rt  = roots([1 7 12]);      %Calculates the roots of y = x^2 + 7x + 12
                            %outputs them into the vector rt.


%TutUnitTest.Actual should include the actual result of the tutorial's main
%sequence (i.e. The contents of the variables listed in
%TutUnitTest.VariableNames). Cell arrays should be used here as well.
TutUnitTest.Actual        = {rt(1),rt(2)};


%IMPORTANT: Each piece of data entered into one of the aforementioned cell
%arrays must correspond to data in the same position in the other cell
%arrays.

%NOTE: If you wish to create TutUnitTest in the following way (commented
%out as it's not needed in this example), ensure when inputting a cell
%array into the 'struct' function to use a double set of curly brackets,
%{{...}}, rather than the usual single set {...}.

%TutUnitTest = struct('VariableNames',{{'rt(1)','rt(2)'}},'Expected',{{-4,-3}},...
%                'Tolerance',{{sqrt(eps),sqrt(eps)}},'Actual',{{rt(1),rt(2)}});
            