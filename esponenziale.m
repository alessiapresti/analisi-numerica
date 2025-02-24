clear all; 
close all; 
format long;

% esponenziale di -20 con la formula di taylor 
% esce un valore sbagliato 

a=-20; %esponente
x=1; %primo termine della serie 
n=100; %numero termini 

for i=1:n 
   x=x+(a^i)/factorial(i); 
end 

disp(x)
y=exp(a)




clear all; 
close all; 
format long;

% esponenziale di -20 con la formula di taylor 
% esce il valore correto  

a=20; %esponente
x=1; %primo termine della serie 
n=100; %numero termini 

for i=1:n 
   x=x+(a^i)/factorial(i); 
end 

disp(x)
y=exp(a)


>> prova_esp2
     4.173637499437795e-09


y =

     2.061153622438558e-09

>> prova_esp3
     4.851651954097902e+08


y =

     4.851651954097903e+08
