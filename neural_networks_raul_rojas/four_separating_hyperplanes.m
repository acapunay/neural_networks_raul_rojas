
%%

syms x1 y1;
% Binary Coding

figure
fsurf(sqrt(1-x1^2-y1^2),'FaceAlpha',0.5,'LineStyle','none');
hold on
fsurf(-sqrt(1-x1^2-y1^2),'FaceAlpha',0.5,'LineStyle','none');
fsurf(@(x,y) -x-y,'FaceAlpha',0.8); fsurf(@(x,y) -x,'FaceAlpha',0.8); fsurf(@(x,y) -y,'FaceAlpha',0.8); fsurf(0,'FaceAlpha',0.8);
xlabel('x'); ylabel('y'); zlabel('z');
hold off

%%
% Bipolar Coding
[x,y] = meshgrid(-10:0.5:10,-10:0.5:10);
bip1 = x+y;
bip2 = x-y;
bip3 = y-x;
bip4 = -x-y;
[u1,v1,w1] = surfnorm(x,y,bip1);
[u2,v2,w2] = surfnorm(x,y,bip2);
[u3,v3,w3] = surfnorm(x,y,bip3);
[u4,v4,w4] = surfnorm(x,y,bip4);

%figure
%quiver3(x,y,bip1,u1,v1,w1,0.5);
%quiver3(x,y,bip2,u2,v2,w2,0.5);
% quiver3(x,y,bip3,u3,v3,w3,0.5);
% quiver3(x,y,bip4,u4,v4,w4,0.5);
% 
surf(bip1);
hold on
surf(bip2); surf(bip3); surf(bip3);
view(-35,45)
hold off
