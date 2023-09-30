% ==========================
% Problem set solving the stochastic neoclassical growth model using a
% sequence method
% ==========================
% clear the workspace
clear
% close all figures
close all
addpath C:/Users/david/OneDrive/Desktop/Education/APE-M2/HT/Quantitative Macroeconomics 1/Problem Sets/PS_1 

% ============
% parameters  - you may have to change this according to instructions
% ============
alpha = 0.4; % capital share
beta = 0.99; % discount factor
rho = 0.95;   % persistence of TFP shock
sigma = 1.0001; % CRRA coefficient (for 1 equals log, but need to replace the function, so set close to 1)
delta = 0.025;


% ============
% options and convergence criteria
% ============

criter_V = 1e-7; % conv criterion for value function
T=150; % periods for transition

%mean of capital non-stochastic steady state
kbar = (1/(alpha * beta) - (1- delta)/alpha)^(1/(alpha - 1)) ;
% initial level of capital in the transition
k_0= 0.9*kbar; % you may have to change this from the problem set instructions

cbar = (1/(alpha * beta) - (1 - delta)/alpha)^(alpha/(alpha - 1)) + (1 - delta)*(1/(alpha * beta) - (1 - delta)/alpha)^(1/(alpha - 1));

% ==============
% Solve deterministic sequence
% ==============

% as usual we need an initial guess for the sequences of k and c - here we
% just use steady state
x0=[kbar*ones(T,1);cbar*ones(T,1)];

zpath=ones(T,1);
zpath(1)=1.01;
for i=2:T-1
zpath(i)=exp(log(zpath(i-1)));
end

% now we need a function that returns errors of the equation system,
% constraining k(0)=k_0, and c(T)=cbar

% ==============
% a. Broyden's method
% ==============

%Initial guess for jacobian - use finite difference at steady state
%sequences of k and c


%These are the error functions witten out
%error_euler = (c(t)/c(t+1))^(-sigma) -  (beta ( alpha k(t+1)^(alpha-1) + (1 - delta) ) ) ;
%error_feas = ( c(t) + k(t+1) - (1 - delta)k(t) ) - k(t)^(alpha)  

c_0 = 1;

%%
clear dx J
for i=1:2*T
    dx = zeros(2*T,1);
    dx(i)=x0(i)*0.001;
    J(:,i) = [rbc_obj( x0 + dx,alpha,beta,sigma,delta,k_0,cbar,zpath) - rbc_obj(x0 ,alpha,beta,sigma,delta,k_0,cbar,zpath)] /dx(i);
end

crit=1e-10;
x=x0;
f=rbc_obj(x,alpha,beta,sigma,delta,k_0,cbar,zpath);

while max(abs(f))>crit

dx = [xxxx fill this in xxxx]; 
x = x+dx;
f = rbc_obj(x,theta,beta,gamma,delta,k_0,cbar,ones(T,1));
J = [xxxx fill this in xxxx];



end

k_trans_br=x(1:T,1);
c_trans_br=x(T+1:2*T,1);

% or we just let matlab solve it

[x,fsol]=fsolve([xxxx fill this in xxxx] );

k_trans=x(1:T,1);
c_trans=x(T+1:2*T,1);

% ==============
% b. Multiple shooting using bisection
% ==============
c_0l=cbar/1.1*k0/kbar;
c_0u=cbar*1.1*k0/kbar;

% now you need to write a function that caculates cT using the dynamic
% equations and starting at guess c0, given k0
% 
% note that the equations may
% produce errors on the way if c or k go to 0
% you may have to redefine the errors to accomodate this

%%
errorl=cT(c_0l,k0)-cbar; 
erroru=cT(c_0u,k0)-cbar;
while abs(c_0l-c_0u)>criter_V
if errorl>0 || erroru<0
    disp('bisection not working')
    break
end
%%
c_0n=(c_0l+c_0u)/2;
errorn=cT(c_0n,k0)-cbar;
if errorn>0
c_0u=c_0n;
else
c_0l=c_0n;
end
  
end

% ==============
% Figures
% ==============

% plot the transition
figure(2)
title('Simulated transition - deterministic')
hold on
[xxxx fill this in xxxx] 

