

iterations = 100000;

chosen_ws = zeros(iterations,3);

for iter = 1:iterations
    w1 = (rand * 2)-1;
    w2 = (rand * (2*sqrt(1-w1^2)))-sqrt(1-w1^2);
    w3 = randsample([-1 1],1)*sqrt(1-w1^2-w2^2);
    chosen_ws(iter,:) = datasample([w1 w2 w3],3,'Replace',false);
end

scatter3(chosen_ws(:,1),chosen_ws(:,2), chosen_ws(:,3))