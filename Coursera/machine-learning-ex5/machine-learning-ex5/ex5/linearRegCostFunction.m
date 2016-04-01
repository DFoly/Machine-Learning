function [J, grad] = linearRegCostFunction(X, y, theta, lambda)
%LINEARREGCOSTFUNCTION Compute cost and gradient for regularized linear 
%regression with multiple variables
%   [J, grad] = LINEARREGCOSTFUNCTION(X, y, theta, lambda) computes the 
%   cost of using theta as the parameter for linear regression to fit the 
%   data points in X and y. Returns the cost in J and the gradient in grad

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;
grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost and gradient of regularized linear 
%               regression for a particular choice of theta.
%
%               You should set J to the cost and grad to the gradient.
%
% htheta = [ones(m,1) X] * theta;
% create X1 to test conformity
%X1 = [ones(m,1) X];
%htheta = X*theta
%n = size(theta);
% note dont regularise theta0 so start at 2
% J = (1/(2*m)) * sum((htheta - y).^2) + (lambda/(2*m)) * sum(theta(2:n).^2);



%noTheta0(1,1) = 0;

% resids = [ones(m,1) X] * theta - y; add ones to test
% ex5 adds constant in main file
resids = X*theta-y;
RSS =  sum(resids.^2);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cost function
n = size(theta);
% dont regularise constant
J = (RSS + lambda*sum(theta(2:n).^2))/(2*m);

%J = ((y-X1*theta)'*(y-X1*theta) + lambda*sum(theta(2:n).^2)/(2*m));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gradient
% grad = (X' * resids)/m
% or
grad(1) = (X(:,1)' * resids)/m
for i = 2:size(theta,1)
    grad(i) = ((X(:,i)' * resids) + lambda * theta(i))/m
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Alternate approach
%grad(1) = sum((resids).*X(:,1))/m;
%for j = 2:size(theta)
%    grad(j) = sum((resids).*X(:,j))/m + lambda * theta(j)/m;
%end

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Using Matrix form - regterm should be n+1
%regterm = lambda.*eye(size(theta,1));
%regterm(1,1) = 0  % dont regularise constant
%theta = inv(X1'*X1 + regterm) * X1'*y;



% =========================================================================

grad = grad(:);

end
