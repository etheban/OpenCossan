function varargout = unitTestConnector
%% UNIT TEST FOR CONNECTOR

% Test constructor
Ntest=1;
Vtest(1:Ntest)=true;
Cmess=cell(Ntest,1);
Cmess{1} = 'No unit test exists for class Identifier'

%% Finalize the test

if nargout>0
    % Export name of the UnitTest
    varargout{1}='Identifier';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
    
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of Identifier (' datestr(now) ')'])
    disp('--------------------------------------------------------------------')
    for i=1:length(Vtest)
        if Vtest(i)
            disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
        else
            disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
        end
    end
end

end
