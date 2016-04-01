%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gradient descent Linear regression

clear ; clc
addpath('C:\Users\dfoley\Dropbox\Machine Learning\Coursera\machine-learning-ex1\machine-learning-ex1\ex1');

data = load('ex1data2.txt');
X = data(:,2:3);
y = data(:,1);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
[m, n] = size(X);

X = [ones(m,1), X];
theta = zeros(n+1,1);


[cost, grad] = costFunction(theta,X,y);

function [J, grad] = costFunction(theta,X,y, alpha ,num_iters);
J = 0;
grad = zeros(theta);
h = (X*theta);

J = 1/(2*m) * (y - h)' * (y-h);

for i = 1:num_iters
theta(i) = theta(i) - (alpha/m) * X(:,i)' * (h-y);
end







