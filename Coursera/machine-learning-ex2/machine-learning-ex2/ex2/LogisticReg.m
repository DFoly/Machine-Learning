%%%% linear regression manully
clear 
clc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   Load Data
addpath('C:\Users\dfoley\Dropbox\Machine Learning\Coursera\machine-learning-ex2\machine-learning-ex2\ex2')
data = load('ex2data1.txt');
X = data(:,1:2) ; y = data(:,3);

[m, n] = size(X);
X = [ones(m,1) X];



%% ============ Part 2: Compute Cost and Gradient ============
initial_theta = zeros(n+1,1);


% compute cost at inital_theta
[cost, grad] = costFunction(initial_theta, X, y);
fprintf('Cost at initial theta (zeros): %f\n', cost);
fprintf('Gradient at initial theta (zeros): \n');
fprintf(' %f \n', grad);


%% ============ Optimise using fminunc ============

%  Set options for fminunc
options = optimset('GradObj', 'on', 'MaxIter', 400);

%  Run fminunc to obtain the optimal theta
%  This function will return theta and the cost 
% note @t allows us to insert wrapper function costFunction
[theta, cost] = ...
	fminunc(@(t)(costFunction(t, X, y)), initial_theta, options);

% Print theta to screen
fprintf('Cost at theta found by fminunc: %f\n', cost);
fprintf('theta: \n');
fprintf(' %f \n', theta);




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sigmoid function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function g = sigmoid(z)

g = 1./(1+exp(-z));
end


%%%%%%%%%%%%%%%%%%%%%%
% costfuntion
%%%%%%%%%%%%%%%%%%%%%%
function [J grad] = costFunction(theta,X,y)


%Initialise
J = 0;
grad = zeros(size(theta));

%%%% cost
h = sigmoid(X*theta);
J =  1/m * sum((-y .* log(h)) - (1-y) .* log(1 - h)); 

% gradient
grad = 1/m * X'*(h-y);

% using loop
% for i = 1:size(theta)
%    grad(i) = 1/m * sum((h-y).* X(:,i));
% end
end


    