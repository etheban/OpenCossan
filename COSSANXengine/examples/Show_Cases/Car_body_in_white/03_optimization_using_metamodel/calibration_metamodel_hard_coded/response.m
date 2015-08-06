function v = response(x,Xmm1,Xin)

x =x(:);

Xsamp   = Samples('Xinput',Xin,'MX',x');
Xin1    = add(Xin,Xsamp);

Xo      = apply(Xmm1,Xin1);
a       = Xo.Tvalues;
Xmodess = Xo_modes('Nmodes',8,'Lmodes',1,'MPhi',a(1).Evec_cl_1,'Vlambda',a(1).Eval_cl_1);
[ohm,ampl] = frf(a(1).Eval_cl_1,a(1).Evec_cl_1,61845,231334,1);
semilogy(ohm,ampl,'b')

%v = max(ampl(1:601));
v = max(ampl)
%plot(x','r')


return

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