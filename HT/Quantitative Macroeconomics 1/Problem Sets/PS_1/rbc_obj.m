function error=rbc_obj(x,alpha,beta,sigma,delta,k0,cbar,zpath)

%x(1:T) is capital
%x(T+1:2T) is consumption

T=length(x)/2;

error(1,1)=x(1,1)-k0;
error(2*T,1)=x(2*T)-cbar;

%%
% calculate errors in budget constraint
for t=2:T
   %error(t,1) = (c(t) + k(t+1) - (1 - delta)*k(t)) - k(t)^(alpha);
   error(t,1) = (x(T+t) + x(t+1) - (1 - delta)*x(t)) - x(t)^(alpha);
end

%%
% calculate errors in EE
for t=1:T-1
   %error(T+t,1) =  (c(t)/c(t+1))^(-sigma) - (beta *( alpha*k(t+1)^(alpha-1) + (1 - delta)));
   error(T+t,1) =  (x(T+t)/x(T+t+1))^(-sigma) - (beta *( alpha*x(t+1)^(alpha-1) + (1 - delta)));
end


end