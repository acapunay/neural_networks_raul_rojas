
% Bipolar Coding
alpha=1;
syms x y;

figure
quiver3(0,0,0,-1,-1,1,'Color','red','LineWidth',5);
hold on
quiver3(0,0,0,-1,1,1,'Color','blue','LineWidth',5); quiver3(0,0,0,1,-1,1,'Color','green','LineWidth',5); quiver3(0,0,0,1,1,1,'Color','black','LineWidth',5);
fsurf(x+y,'FaceAlpha',alpha); fsurf(x-y,'FaceAlpha',alpha); fsurf(-x+y,'FaceAlpha',alpha); fsurf(-x-y,'FaceAlpha',alpha);
hold off