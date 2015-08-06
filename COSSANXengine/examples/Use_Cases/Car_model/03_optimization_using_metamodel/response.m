function Toutput = response(Tinput)

%% 1.   Load information on metamodel
load Tmm.mat

%% 2.   Information on design variables
TNominalValues  = struct('pshell_11',1,...
    'pshell_17',2,...
    'pshell_19',0.9,...
    'pshell_53',1.5,...
    'pshell_71',2,...
    'pshell_79',1.8,...
    'pshell_82',1.4,...
    'pshell_84',0.8,...
    'pshell_86',1.2,...
    'pshell_88',2.5,...
    'pshell_91',1.5,...
    'pshell_98',1.2,...
    'pshell_100',1.2,...
    'pshell_101',0.9,...
    'pshell_102',1,...
    'pshell_105',1.8,...
    'pshell_107',1.2,...
    'pshell_109',5,...
    'pshell_110',1.4);

%% 2.   Evaluate response
Cfieldnames     = fieldnames(Tinput);
for i=1:length(Tinput),
    MU  = zeros(19,1);
    for j=1:length(Cfieldnames),
        MU(j,1)     = (Tinput(i).(Cfieldnames{j}) - ...
            TNominalValues.(Cfieldnames{j})) / ...
            (0.05*TNominalValues.(Cfieldnames{j}));
    end
    TSpectralQuant  = apply_mm(Tmm,MU);
    TSpectralQuant  = TSpectralQuant.Tvalues;
    [ohm,ampl]      = frf(TSpectralQuant.Eval_cl_1,...  %Eigenvalues
        TSpectralQuant.Evec_cl_1,...    %Eigenvectors
        61845,...   %first DOF
        231334,...  %second DOF
        1);         %pow (what is this? MAV: my guess is that it is an 
    %exponent controlling the type of FRF, pow=1 is a velocity FRF
    % semilogy(ohm,ampl,'b')
    v               = max(ampl);
    Toutput(i,1).fobj  = v;
    %num2str(MU(:)','%3.2e ')
    %keyboard
end


return

%**************************************************************************
%
%   FRF
%
%**************************************************************************
    function [ohm,ampl] = frf(eval,evec,dof1,dof2,pow)
        %
        %
        %-----------------------------------
        ohm = 200:0.25:350.0;
        ohm_q = ohm.^2;
        n_ohm = numel(ohm);
        ampl = zeros(1,n_ohm);
        %pow = 0;%2; %0...x,1...dx,2...ddx
        ohm_p = ohm.^pow;
        damp_ratio = 0.02;
        %dof1 = 45883;
        %dof2 = 13724;
        %-----------------------------------
        n_modes = numel(eval(1:8));
        evec = evec(:,1:n_modes);
        [n_dof,cols] = size(evec);
        if(n_modes~=cols)
            error('Inconsistent parameters');
            return
        end
        eig_freq = eval.^0.5;
        %
        %
        resp = zeros(1,n_ohm);
        for i1=1:n_modes
            cij = evec(dof1,i1)*evec(dof2,i1);
            for i2=1:n_ohm
                cmpl = cij*ohm_p(i2)/(eval(i1)-ohm_q(i2)+2*1i*ohm(i2)*damp_ratio...
                    *eig_freq(i1));
                resp(i2) = resp(i2)+cmpl;
            end
        end
        ampl = abs(resp);
        
        return
        
        %**************************************************************************
        %
        %   APPLY
        %
        %**************************************************************************
        function Xo = apply_mm(Xobj,MU)%Xout = apply(Xobj,Pinput)
            %[varargout] = apply(Xobj,Pinput)%Xout = apply(Xobj,Pinput)

            n_modes = Xobj.Nmodes;
            Eval_cl_0 = Xobj.Xmodes0.Vlambda;
            Evec_cl_0 = Xobj.Xmodes0.MPhi;
            mass_mat_0 = Xobj.Xmodes0.Mmass;

            % Eval_cl = Xobj.Xmodes(i).Vlambda;
            % Evec_cl = Xobj.Xmodes(i).MPhi;

            X = Xobj.Valpha(:,1:end-1)';
            
            A = Xobj.Vlincomb;
            
            %MUcal = get(Xobj.Xin2,'values_matrix_sn');
            MUcal = MU(:)';
            rnd_var = MUcal(1,:);
            
            rnd = [rnd_var'];%[rnd_var'; rnd_qua'];
            Eval_cl_1
            n_rnd = numel(rnd);
            approx = A*rnd;
            na = numel(approx);
            
            Eval_cl_1 = Eval_cl_0;
            Evec_cl_1 = Evec_cl_0;
            
            idx_mod = find(Xobj.list_mod<0);
            idx_mod(end+1) = numel(Xobj.list_mod)+1;
            idx_mod;
            
            for i1=1:n_modes
                i2 = idx_mod(i1);
                i3 = idx_mod(i1+1);
                factor = approx(i2);
                omega = sqrt(Eval_cl_0(i1));
                omega_i1 = (1+factor)*omega;
                Eval_cl_1(i1) = omega_i1^2;
                ampl_i1 = 1+approx(i2+1);
                nn = i3-i2-1;
                coef = zeros(nn,1);
                cosq = 1;
                for i4=2:nn
                    coef(i4) = approx(i2+i4);%sin(approx(i2+i4));
                    %cosq = cosq-coef(i4)^2;
                end
                coef(1) = 1+approx(i2+1)-norm(coef)^2;%coef(1) = cosq^.5;
                idx0 = Xobj.list_mod(i2+1:i3-1);
                modes_0 = Evec_cl_0(:,idx0);
                mode = modes_0*coef;
                mode = mode.*ampl_i1;
                Evec_cl_1(:,i1) = mode;
                % comp = [sqrt(Eval_cl(i1)) omega_i1; Evec_cl(:,i1) mode];
                % comp
            end
            
            %modes = a* Mmodes*Xin;
            %lambda = b*Mlam*Xin;
            
            %Tinput = Xin;
            %looooop
            Toutput(1).Evec_cl_1 = Evec_cl_1;
            Toutput(1).Eval_cl_1 = Eval_cl_1;
            
            
            %Xo = Xoutput('Tinput',Tinput,'Toutput',Toutput,'Sdescription',' - apply(@Xmodel)')
            Xo = SimulationData('Tvalues',Toutput);
            %% Export data
            %             if varargout == 1
            %                 varargout = Xo;
            %             elseif varargout == 1
            %                 %Xmodes = ...
            %varargout{1} = Xo;
            %                 varargout{2} = Xmodes;
            %             end
            
            
            
            return
