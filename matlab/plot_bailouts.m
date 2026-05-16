%% This Code Generates Figures 2-12 and Figure C.2 in the Paper
% Edited on October 29, 2023

clear
close all


set(0,'DefaultFigureWindowStyle','normal')




load ../fortran/graphs_b_grid_dss.txt
load ../fortran/graphs_y_grid_dss.txt
load ../fortran/graphs_epsilon_grid_dss.txt
load ../fortran/graphs_transfer_grid_dss.txt
load ../fortran/graphs_v_dss.txt
load ../fortran/graphs_value_planner_dss.txt
load ../fortran/graphs_q.txt
load ../fortran/graphs_default_dss.txt
load ../fortran/graphs_b_next.txt
load ../fortran/graphs_consumption.txt
load ../fortran/graphs_interest.txt
load ../fortran/graphs_taxes.txt
load ../fortran/graphs_labor.txt

load ../fortran/graphs_w_dss.txt
load ../fortran/graphs_transfers.txt
load ../fortran/graphs_loans.txt
load ../fortran/graphs_vw_dss.txt
load ../fortran/graphs_parameters.txt
load ../fortran/graphs_deviation.txt

load ../fortran/objective_trcand.txt
load ../fortran/output_trcand.txt
load ../fortran/objective_cands.txt
load ../fortran/output_cands.txt

load ../fortran/tax_vector.txt
load ../fortran/bp_choice.txt

load ../fortran/graphs_simulation_spread.txt

v_nobail=load('../fortran/nb_files/graphs_value_planner_dss.txt');
q_nobail=load('../fortran/nb_files/graphs_q.txt');
c_nobail=load('../fortran/nb_files/graphs_consumption.txt');

epsilon_grid_dss = graphs_epsilon_grid_dss;
epsilon_grid = graphs_epsilon_grid_dss;
b_grid = graphs_b_grid_dss;
y_grid = graphs_y_grid_dss;
v_dss = graphs_v_dss;
value_dss = graphs_value_planner_dss;
q = graphs_q;
default_dss = graphs_default_dss;
b_next = graphs_b_next;

interest= graphs_interest;
taxes= graphs_taxes;
labor= graphs_labor;
cons = graphs_consumption;
transfers= graphs_transfers;
loans= graphs_loans;

w_dss= graphs_w_dss;
vw_dss= graphs_vw_dss;
parameters= graphs_parameters;
tr_grid = graphs_transfer_grid_dss;

v = v_dss;
default = default_dss;

vw=vw_dss;
w=w_dss;

b_num = length(b_grid);
y_num = length(y_grid);
epsilon_num = length(epsilon_grid_dss);
crisis_num = 2;

value0_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
value1_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
value_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
value_matrix_nobail = zeros(b_num, y_num, epsilon_num, crisis_num);


vw0_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
vw1_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
vw_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
v0_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
v1_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
v_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
w0_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
w1_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
w_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
default_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
q_matrix = zeros(b_num, y_num);
q_matrix_nobail = zeros(b_num, y_num);
b_next_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
b_next_index_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
dev_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
dev_q_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
q_paid_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
q_paid_matrix_nodef = zeros(b_num, y_num, epsilon_num, crisis_num);
interest0_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
interest1_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
interest_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
taxes0_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
taxes1_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
taxes_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
labor0_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
labor1_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
labor_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
cons0_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
cons1_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
cons_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
cons0_nb_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
cons1_nb_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
cons_nb_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
output0_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
output1_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
output_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);

loan0_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
loan1_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
loan_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);

transfer0_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);
transfer_matrix = zeros(b_num, y_num, epsilon_num, crisis_num);


for i_crisis=1:crisis_num
    for i_e = 1:epsilon_num        
        for i_y = 1:y_num
            for i_b = 1:b_num
                
                pointer = (i_crisis -1)*epsilon_num*y_num*b_num +(i_e -1)*y_num*b_num + ...
                    (i_y -1)*b_num  + i_b;                
                
                vw0_matrix(i_b, i_y, i_e, i_crisis) = vw(pointer,2);
                vw1_matrix(i_b, i_y, i_e, i_crisis) = vw(pointer,3);
                vw_matrix(i_b, i_y, i_e, i_crisis) = vw(pointer,1);
                value0_matrix(i_b, i_y, i_e, i_crisis) = value_dss(pointer,2);
                value1_matrix(i_b, i_y, i_e, i_crisis) = value_dss(pointer,3);
                value_matrix(i_b, i_y, i_e, i_crisis) = value_dss(pointer,1);
                value_matrix_nobail(i_b, i_y, i_e, i_crisis) = v_nobail(pointer,1);
                
                v0_matrix(i_b, i_y, i_e, i_crisis) = v(pointer,2);
                v1_matrix(i_b, i_y, i_e, i_crisis) = v(pointer,3);
                v_matrix(i_b, i_y, i_e, i_crisis) = v(pointer,1);
                w0_matrix(i_b, i_y, i_e, i_crisis) = w(pointer,2);
                w1_matrix(i_b, i_y, i_e, i_crisis) = w(pointer,3);
                w_matrix(i_b, i_y, i_e, i_crisis) = w(pointer,1);
                default_matrix(i_b, i_y, i_e, i_crisis) = default(pointer);
                
                if (i_e==1)&&(i_crisis==1)
                    q_matrix(i_b, i_y) = q(pointer,1);
                    q_matrix_nobail(i_b, i_y) = q_nobail(pointer,1);
                end
                q_paid_matrix(i_b, i_y, i_e, i_crisis) = q(pointer,2);
                b_next_index_matrix(i_b, i_y, i_e, i_crisis) = b_next(pointer);                
                interest0_matrix(i_b, i_y, i_e, i_crisis) = interest(pointer,2);
                interest1_matrix(i_b, i_y, i_e, i_crisis) = interest(pointer,3);
                interest_matrix(i_b, i_y, i_e, i_crisis) = interest(pointer,1);
                taxes0_matrix(i_b, i_y, i_e, i_crisis) = taxes(pointer,2);
                taxes1_matrix(i_b, i_y, i_e, i_crisis) = taxes(pointer,3);
                taxes_matrix(i_b, i_y, i_e, i_crisis)  =  taxes(pointer,1);
                labor_matrix(i_b, i_y, i_e, i_crisis) = labor(pointer,1);
                labor0_matrix(i_b, i_y, i_e, i_crisis) = labor(pointer,2);
                labor1_matrix(i_b, i_y, i_e, i_crisis) = labor(pointer,3);
                cons_matrix(i_b, i_y, i_e, i_crisis) = cons(pointer,1);
                cons0_matrix(i_b, i_y, i_e, i_crisis) = cons(pointer,2);
                cons1_matrix(i_b, i_y, i_e, i_crisis) = cons(pointer,3);
                cons_nb_matrix(i_b, i_y, i_e, i_crisis) = c_nobail(pointer,1);
                cons0_nb_matrix(i_b, i_y, i_e, i_crisis) = c_nobail(pointer,2);
                cons1_nb_matrix(i_b, i_y, i_e, i_crisis) = c_nobail(pointer,3);
                loan0_matrix(i_b, i_y, i_e, i_crisis) = loans(pointer,2);
                loan1_matrix(i_b, i_y, i_e, i_crisis) = loans(pointer,3);
                loan_matrix(i_b, i_y, i_e, i_crisis) = loans(pointer,1);
                transfer_matrix(i_b, i_y, i_e, i_crisis) = transfers(pointer,1);
                transfer0_matrix(i_b, i_y, i_e, i_crisis) = transfers(pointer,2);
                dev_v_matrix(i_b, i_y, i_e, i_crisis) = graphs_deviation(pointer,1);
                dev_q_matrix(i_b, i_y, i_e, i_crisis) = graphs_deviation(pointer,2);

            end            
        end        
    end
end

t_num = length(graphs_transfer_grid_dss);
objective_cands = reshape(objective_cands,[b_num,t_num,crisis_num]);
output_cands = reshape(output_cands,[b_num,t_num,crisis_num]);
tax_vector = reshape(tax_vector,[b_num,t_num,crisis_num]);
bp_choice = reshape(bp_choice,[t_num,crisis_num]);

%% NAME PARAMETERS
%alpha, beta, beta_banker, sigma, prob_excl_end, theta, gamma, omega, gasto, AA

alfa=parameters(1);
betta=parameters(2);
betta_banker=parameters(3);
sigma=parameters(4);
prob_excl_end=parameters(5);
theta=parameters(6);
gama=parameters(7);
omega=parameters(8);
gasto=parameters(9);
AA=parameters(10);
endow_bank=parameters(10);
prob_bc=parameters(11);

mean_gdp =.8;

for i_crisis=1:crisis_num
    for i_e = 1:epsilon_num        
        for i_y =1 :y_num
            for i_b = 1:b_num
                
                output_matrix(i_b, i_y, i_e, i_crisis) =exp(y_grid(i_y)) *  labor_matrix(i_b, i_y, i_e, i_crisis)^alfa;
                output0_matrix(i_b, i_y, i_e, i_crisis) =exp(y_grid(i_y)) *  labor0_matrix(i_b, i_y, i_e, i_crisis)^alfa;
                output1_matrix(i_b, i_y, i_e, i_crisis) =exp(y_grid(i_y)) *  labor1_matrix(i_b, i_y, i_e, i_crisis)^alfa;
                output1_matrix(i_b, i_y, i_e, i_crisis) =b_grid(b_next_index_matrix(i_b, i_y, i_e, i_crisis));
            end
        end
    end
end




%% LOAD DATA
load ../fortran/graphs_data_sim_dss.txt
load ../fortran/graphs_def_per_dss.txt
load ../fortran/graphs_param_simulation_dss.txt



data_sim = graphs_data_sim_dss;
def_per = graphs_def_per_dss;
param = graphs_param_simulation_dss;
data_spread = graphs_simulation_spread;
data_spread = data_spread';

%dprobability = graphs_simulation_dprob;
per_num = param(1);  %HOW MANY PERIODS IN EACH SAMPLE
n =param(2);        %HOW MANY SAMPLES


y = zeros(per_num, n);
b = zeros(per_num, n);
q = zeros(per_num, n);
c = zeros(per_num, n);
nn = zeros(per_num, n);
r = zeros(per_num, n);
d = zeros(per_num, n);
e = zeros(per_num, n); 
epsilon = zeros(per_num, n);
bc = zeros(per_num, n);
b_next = zeros(per_num-1, n);
tao = zeros(per_num,n);
pi_f = zeros(per_num,n);
pi_b = zeros(per_num,n);
loan = zeros(per_num,n);
tax_repayment = zeros(per_num,n);
transfers = zeros(per_num,n);
spread= zeros(per_num, n);
promised_trans = zeros(per_num, n);


for i=1:n
   y(:,i) = data_sim((i-1)*per_num+1:i*per_num,1); 
   b(:,i) = data_sim((i-1)*per_num+1:i*per_num,2); 
   q(:,i) = data_sim((i-1)*per_num+1:i*per_num,3); 
   c(:,i) = data_sim((i-1)*per_num+1:i*per_num,4); 
   nn(:,i) = data_sim((i-1)*per_num+1:i*per_num,5);
   d(:,i) = data_sim((i-1)*per_num+1:i*per_num,6)-1;
   r(:,i) = data_sim((i-1)*per_num+1:i*per_num,7);
   tao(:,i) = data_sim((i-1)*per_num+1:i*per_num,8);
   pi_f(:,i) = data_sim((i-1)*per_num+1:i*per_num,9);
   pi_b(:,i) = data_sim((i-1)*per_num+1:i*per_num,10);
   loan(:,i) = data_sim((i-1)*per_num+1:i*per_num,11);
   tax_repayment(:,i) = data_sim((i-1)*per_num+1:i*per_num,12);
   e(:,i) = data_sim((i-1)*per_num+1:i*per_num,13);
   transfers(:,i) = data_sim((i-1)*per_num+1:i*per_num,14);
   bc(:,i) = data_sim((i-1)*per_num+1:i*per_num,15);
   epsilon(:,i) = data_sim((i-1)*per_num+1:i*per_num,16);
   spread(:,i) =  data_spread(1,(i-1)*per_num+1:i*per_num);
   promised_trans(:,i)=data_sim((i-1)*per_num+1:i*per_num,17);
  
end
z = (y./(nn.^alfa));
bad_epsilon=zeros(size(epsilon));
bad_epsilon(epsilon>1)=1;

banking_crisis = bad_epsilon.*bc;

tax_revenue = (loan./gama).*tao;

b_next = b(2:per_num,:);

b_next=[b_next;nan(1,500)];

%CREATE OUTPUT SERIES
num = 500; %HOW MANY PERIODS IN EACH SUBSAMPLE

last_y = y(per_num - num+1:per_num,:);  %TRIM THE FIRST (per_num - num) OBSERVATIONS
last_c = c(per_num - num+1:per_num,:);  
last_pi_b = pi_b(per_num - num+1:per_num,:);
last_nn = nn(per_num - num+1:per_num,:);  
last_e = e(per_num - num+1:per_num,:);
last_d = d(per_num - num+1:per_num,:);
last_bc = bc(per_num - num+1:per_num,:);
last_b = b(per_num - num+1:per_num,:);
last_b_next = b_next(per_num - num+1:per_num,:);

last_revenue = tax_revenue(per_num - num+1:per_num,:);
last_tao = tao(per_num - num+1:per_num,:);
last_loan = loan(per_num - num+1:per_num,:);
last_l_to_y= last_loan./last_y;
last_tax_repayment = tax_repayment(per_num - num+1:per_num,:);
last_q = q(per_num - num+1:per_num,:);
last_r = r(per_num - num+1:per_num,:);
last_transfers = transfers(per_num - num+1:per_num,:);
last_promised_trans = promised_trans(per_num - num+1:per_num,:);

last_epsilon = epsilon(per_num - num+1:per_num,:);
last_bad_epsilon = bad_epsilon(per_num - num+1:per_num,:);

last_banking_crisis = last_bad_epsilon.*last_bc;

last_interest = (1./last_q)-1;
last_interest(~isfinite(last_interest)) = NaN; %% added this

last_sp = spread(per_num - num+1:per_num,:);
last_spread=last_sp;
%last_dprob = dprob(per_num - num+1:per_num,:);
last_z = z(per_num - num+1:per_num,:);



beta_bank=0.96;
rw=1/beta_bank -1;

% last_spread_rw = max( ((1./last_q))./((1+rw))-1  ,0);
last_spread_rw = 1./last_q-(1+rw) ;

lambda = 100;


% Apply HP filter with new syntax: returns [Trend, Cyclical]
[~, y_trend] = hpfilter(log(last_y), 'Smoothing', lambda);              % FILTER log(output)
[~, c_trend] = hpfilter(log(last_c), 'Smoothing', lambda);              % FILTER log(consumption)
[~, pi_b_trend] = hpfilter(last_pi_b, 'Smoothing', lambda);             % FILTER consumption_banker
[~, nn_trend] = hpfilter(log(last_nn), 'Smoothing', lambda);            % FILTER log(hours)
[~, loan_trend] = hpfilter(log(last_loan), 'Smoothing', lambda);        % FILTER log(loan)
[~, l_to_y_trend] = hpfilter(last_l_to_y, 'Smoothing', lambda);         % FILTER loan_to_y
[~, spread_trend] = hpfilter(last_spread, 'Smoothing', lambda); % FILTER spread

spread_rw_trend = NaN(size(last_spread_rw));

for j = 1:size(last_spread_rw,2)
    x = last_spread_rw(:,j);
    good = isfinite(x);
    if all(good)
        [~, spread_rw_trend(:,j)] = hpfilter(x, 'Smoothing', lambda);
    end
end

[~, tao_trend] = hpfilter(log(max(last_tao, 1e-6)), 'Smoothing', lambda);
[~, tax_repayment_trend] = hpfilter(log(max(last_tax_repayment, 1e-6)), 'Smoothing', lambda);


%COMPUTE DEVIATIONS FROM TREND  
y_dev = log(last_y)- y_trend;
c_dev = log(last_c) - c_trend;
pi_b_dev = last_pi_b - pi_b_trend;
nn_dev = log(last_nn) - nn_trend;
loan_dev= log(last_loan)- loan_trend;
spread_dev = last_spread - spread_trend;
spread_rw_dev = last_spread_rw - spread_rw_trend;
l_to_y_dev=last_l_to_y-l_to_y_trend;
tao_dev = log(last_tao) - tao_trend;
tax_repayment_dev = last_tax_repayment-tax_repayment_trend;


excluded_indicator = last_e-1;
bc_indicator = last_bc.*last_epsilon>1;



% Creating plots that show series around a banking crisis
[row,col] = find(last_banking_crisis);
obscount = size(row,1); % no of banking crisis obs.

irow = 0;

for i=1:obscount
    if (row(i,1)-4>0 && row(i,1)+4<=500) 
        if sum(last_d(row(i,1)-4:row(i,1)-1, col(i,1)))<1
            irow = irow+1;
        iseries(irow,:)= last_interest(row(i,1)-4:row(i,1)+4, col(i,1))  ; 
        yseries2(irow,:)= last_y(row(i,1)-4:row(i,1)+4, col(i,1))  ;
        bseries(irow,:)= last_b(row(i,1)-4:row(i,1)+4, col(i,1))  ;
        dseries(irow,:)= last_d(row(i,1)-4: row(i,1)+4, col(i,1))  ;
        %dprobseries(i,:)= last_dprob(row(i,1)-4: row(i,1)+4, col(i,1))  ;
        sseries(irow,:)= last_spread_rw(row(i,1)-4: row(i,1)+4, col(i,1))  ;
        zseries(irow,:) = last_z(row(i,1)-4: row(i,1)+4, col(i,1))  ;
        rseries(irow,:) = last_r(row(i,1)-4: row(i,1)+4, col(i,1))  ;
        nseries(irow,:) = last_nn(row(i,1)-4: row(i,1)+4, col(i,1))  ;
        taxseries(irow,:) = last_tao(row(i,1)-4: row(i,1)+4, col(i,1))  ;
        revenueseries(irow,:) = last_revenue(row(i,1)-4: row(i,1)+4, col(i,1))  ;
        end
    end
end

b_to_yseries=bseries./yseries2;


%% --- Now for no-default samples ---

num = 25; %HOW MANY PERIODS IN EACH SUBSAMPLE
indices = find(sum(d(per_num - num - 10 +1:per_num,:))<1); %LAST DEFAULT: 10 PERIODS BEFORE THE BEGINNING OF EACH SAMPLE.

num_observations = length(indices);

df_y = y(per_num - num+1:per_num, indices);  %TRIM THE FIRST (per_num - num) OBSERVATIONS
df_c = c(per_num - num+1:per_num, indices);  
df_pi_b = pi_b(per_num - num+1:per_num, indices);
df_nn = nn(per_num - num+1:per_num, indices);  
df_e = e(per_num - num+1:per_num, indices);
df_d = d(per_num - num+1:per_num, indices);
df_bc = bc(per_num - num+1:per_num, indices);
df_b = b(per_num - num+1:per_num, indices);
df_b_next = b_next(per_num - num+1:per_num, indices);
df_tao = tao(per_num - num+1:per_num, indices);
df_loan = loan(per_num - num+1:per_num, indices);
df_l_to_y= df_loan./df_y;
df_tax_repayment = tax_repayment(per_num - num+1:per_num, indices);
df_q = q(per_num - num+1:per_num, indices);
df_r = r(per_num - num+1:per_num, indices);
df_transfers = transfers(per_num - num+1:per_num, indices);
df_promised_trans = promised_trans(per_num - num+1:per_num, indices);
df_epsilon = epsilon(per_num - num+1:per_num, indices);
df_bad_epsilon = bad_epsilon(per_num - num+1:per_num, indices);
df_banking_crisis= banking_crisis(per_num - num+1:per_num, indices);
df_spr = spread(per_num - num+1:per_num, indices);

df_spread = df_spr;

df_spread_rw = 1./df_q -(1+rw);
per_num_local = 500;

df_y_dev = y_dev(per_num_local - num+1:per_num_local, indices);

aux = df_bc.*df_epsilon;
crisis_indicator = zeros(size(aux));
crisis_indicator(aux>1) = 1;


% Save results for plots
dlmwrite('bss1.txt', df_b);
dlmwrite('crisis_indicator.txt', crisis_indicator);

%unconditional means
mean_y = zeros(1,9);
mean_b = zeros(1,9);
mean_z = zeros(1,9);
mean_r = zeros(1,9);
mean_i = zeros(1,9);
mean_spread = zeros(1,9);
mean_n = zeros(1,9);

for i=1:9
    mean_y(1,i) = mean(yseries2(:,i));
    mean_b(1,i) = mean(bseries(:,i));
    mean_b_to_y(1,i) = mean(b_to_yseries(:,i));
    mean_z(1,i) = mean(zseries(:,i));
    mean_r(1,i) = mean(rseries(:,i));
    mean_i(1,i) = mean(iseries(:,i), 'omitnan'); 
    mean_spread(1,i) = mean(sseries(:,i));
    mean_n(1,i) = mean(nseries(:,i));
    mean_tax(1,i) = mean(taxseries(:,i));
    mean_revenue(1,i) = mean(revenueseries(:,i));
    
end

b75 = prctile(bseries, 75);
z25 = prctile(zseries, 25);
b_to_y_75 = prctile(b_to_yseries, 75);

[ro,co]=size(bseries);
highdebt_indicator=zeros(ro, co);
% highdebt_indicator(bseries(:,5)>=b75)=1;
highdebt_indicator(b_to_yseries(:,5)>=b_to_y_75)=1;


% conditional means
mean_y_highdebt = zeros(1,9);
mean_z_highdebt = zeros(1,9);
mean_b_highdebt = zeros(1,9);
mean_r_highdebt = zeros(1,9);
mean_i_highdebt = zeros(1,9);
mean_spread_highdebt = zeros(1,9);

for i=1:9
    mean_y_highdebt(1,i) = mean(yseries2(highdebt_indicator(:,i)==1,i));
    mean_z_highdebt(1,i) = mean(zseries(highdebt_indicator(:,i)==1,i));
    mean_b_highdebt(1,i) = mean(bseries(highdebt_indicator(:,i)==1,i));
    mean_b_to_y_highdebt(1,i) = mean(b_to_yseries(highdebt_indicator(:,i)==1,i));
    mean_r_highdebt(1,i) = mean(rseries(highdebt_indicator(:,i)==1,i));
    mean_i_highdebt(1,i) = mean(iseries(highdebt_indicator(:,i)==1,i), 'omitnan'); 
    mean_spread_highdebt(1,i) = mean(sseries(highdebt_indicator(:,i)==1,i));
    mean_n_highdebt(1,i) = mean(nseries(highdebt_indicator(:,i)==1,i));
    mean_tax_highdebt(1,i) = mean(taxseries(highdebt_indicator(:,i)==1,i));
    mean_revenue_highdebt(1,i) = mean(revenueseries(highdebt_indicator(:,i)==1,i));
end

% %% One subplot
 time=-4:4;
 x_tick_vector=-3:3;
 
 loyolagreen = 1/255*[0,104,87];

%NOTE: HERE WE SAVE THE MEAN_GDP, TO BE USED WHEN CONSTRUCTING MOST OF THE
%PLOTS. 
Mean_GDP = mean(mean(last_y(excluded_indicator==0)));
csvwrite('mean_gdp.txt',Mean_GDP)


outdir = fullfile('..','figures');
if ~exist(outdir,'dir')
    mkdir(outdir);
end




%% Figure 2 : Output around banking crises

data_yield = xlsread('rgdp_yield.xlsx','yield');
data_gdp = xlsread('rgdp_yield.xlsx','rgdp');
data_yield = data_yield(:,2:3);
data_gdp = data_gdp(:,2:3);


hfig = figure;
pos = get(hfig,'position');
set(hfig,'Units','inches');
set(hfig,'Position',[1 1 16 6]);


subplot(1,2,1)
H= plot(-3:3, [data_gdp(:,2) data_gdp(:,1)]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex','FontName','Times New Roman')
set(H(1), 'LineStyle','-','LineWidth',2,'MarkerSize',.1,'Marker','square', 'Color','k')
set(H(2), 'LineStyle','--','LineWidth',2,'MarkerSize',.1,'Marker','square', 'Color','r')
ylabel('Index','Interpreter', 'latex','FontSize',20,'FontName','Times New Roman')
xlabel('Time','Interpreter', 'latex','FontSize',20,'FontName','Times New Roman')
%title('Data', 'Interpreter', 'latex', 'FontSize', 16)
xticks(x_tick_vector)
leg1=legend('Unconditional','High debt');
set(leg1,'Interpreter','latex');
set(leg1,'FontSize',20);
set(leg1,'Location','SouthWest','Orientation','vertical');
legend boxoff

grid on


subplot(1,2,2)
H= plot(time, 100*[mean_y/mean_y(1,4); mean_y_highdebt/mean_y_highdebt(1,4)]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex','FontName','Times New Roman')
set(H(1), 'LineStyle','-','LineWidth',2,'MarkerSize',.1,'Marker','square', 'Color','k')
set(H(2), 'LineStyle','--','LineWidth',2,'MarkerSize',.1,'Marker','square', 'Color','r')
axis([min(x_tick_vector) max(x_tick_vector) 95 101])
ylabel('Index','Interpreter', 'latex','FontSize',20,'FontName','Times New Roman')
xlabel('Time','Interpreter', 'latex','FontSize',20,'FontName','Times New Roman')
%title('Model', 'Interpreter', 'latex', 'FontSize', 16)
xticks(x_tick_vector)
grid on
%sgtitle('Figure 2: Output around banking crises', 'Interpreter', 'latex', 'FontSize', 18)


drawnow
exportgraphics(hfig, fullfile(outdir,'Fig_dynamics_gdp.png'),'Resolution',400)


%% Figure 3 : Debt and taxes around banking crises


hfig = figure;
pos = get(hfig,'position');

set(hfig,'Units','inches');
set(hfig,'Position',[1 1 16 6]);

subplot(1,2,1)
H= plot(time, 100*[mean_b_to_y; mean_b_to_y_highdebt]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex','FontName','Times New Roman')
set(H(1), 'LineStyle','-','LineWidth',2,'MarkerSize',.1,'Marker','square', 'Color','k')
set(H(2), 'LineStyle','--','LineWidth',2,'MarkerSize',.1,'Marker','square', 'Color','r')
axis([min(x_tick_vector) max(x_tick_vector) 15 18])
ylabel('Percent','Interpreter', 'latex','FontSize',20,'FontName','Times New Roman')
xlabel('Time','Interpreter', 'latex','FontSize',20,'FontName','Times New Roman')
%title('Debt/GDP', 'Interpreter', 'latex', 'FontSize', 16)
xticks(x_tick_vector)
leg1=legend('Unconditional','High debt');
set(leg1,'Interpreter','latex');
set(leg1,'FontSize',16);
set(leg1,'Location','NorthWest','Orientation','vertical');
legend boxoff
grid on


subplot(1,2,2)

H= plot(time, 100*[mean_tax; mean_tax_highdebt]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex','FontName','Times New Roman')
set(H(1), 'LineStyle','-','LineWidth',2,'MarkerSize',.1,'Marker','square', 'Color','k')
set(H(2), 'LineStyle','--','LineWidth',2,'MarkerSize',.1,'Marker','square', 'Color','r')
axis([min(x_tick_vector) max(x_tick_vector) 26 34])
ylabel('Percent','Interpreter', 'latex','FontSize',20,'FontName','Times New Roman')
xlabel('Time','Interpreter', 'latex','FontSize',20,'FontName','Times New Roman')
%title('Tax rate', 'Interpreter', 'latex', 'FontSize', 16)
xticks(x_tick_vector)
grid on
%sgtitle('Figure 3: Debt and taxes around banking crises', 'Interpreter', 'latex', 'FontSize', 18)


drawnow

exportgraphics(hfig, fullfile(outdir,'Fig_dynamics_debt_taxes_model.png'),'Resolution',400)

%% Figure 4: Sovereign yields around banking crises


hfig = figure;
set(hfig,'Units','inches');
set(hfig,'Position',[1 1 16 6]);

subplot(1,2,1)
H= plot(-3:3, [data_yield(:,2) data_yield(:,1)]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex','FontName','Times New Roman')
set(H(1), 'LineStyle','-','LineWidth',2,'MarkerSize',.1,'Marker','square', 'Color','k')
set(H(2), 'LineStyle','--','LineWidth',2,'MarkerSize',.1,'Marker','square', 'Color','r')
ylabel('Percent','Interpreter', 'latex','FontSize',20,'FontName','Times New Roman')
xlabel('Time','Interpreter', 'latex','FontSize',20,'FontName','Times New Roman')
%title('Data', 'Interpreter', 'latex', 'FontSize', 16)
xticks(x_tick_vector)
leg1=legend('Unconditional','High debt');
set(leg1,'Interpreter','latex');
set(leg1,'FontSize',16);
set(leg1,'Location','NorthWest','Orientation','vertical');
legend boxoff

grid on


subplot(1,2,2)

H= plot(time, 100*[mean_i; mean_i_highdebt]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex','FontName','Times New Roman')
set(H(1), 'LineStyle','-','LineWidth',2,'MarkerSize',.1,'Marker','square', 'Color','k')
set(H(2), 'LineStyle','--','LineWidth',2,'MarkerSize',.1,'Marker','square', 'Color','r')
axis([min(x_tick_vector) max(x_tick_vector) 4.5 5.5])
ylabel('Percent','Interpreter', 'latex','FontSize',20,'FontName','Times New Roman')
xlabel('Time','Interpreter', 'latex','FontSize',20,'FontName','Times New Roman')
%title('Model', 'Interpreter', 'latex', 'FontSize', 16)
xticks(x_tick_vector)
yticks([4.5 5 5.5]);
grid on

%sgtitle('Figure 4: Sovereign yields around banking crises', 'Interpreter', 'latex', 'FontSize', 18)



drawnow

exportgraphics(hfig, fullfile(outdir,'Fig_dynamics_yield.png'),'Resolution',400)



def_matrix=squeeze(default_matrix(:,:,:,2));
def_plot=2*ones(b_num, y_num, epsilon_num)-def_matrix;


%% Figure 5: Default sets and bond prices

for ie = 1:epsilon_num
    contour = sum(squeeze(default_matrix(:,:,ie,1))');
    nn = length(contour);
    for i = 1:nn
        if contour(i) == 0
            contour_plot(i,ie) = 1;
        else
            contour_plot(i,ie) = contour(i);
        end
    end
end

bw_grid = 0.9*(256-[0:255]')/256; % monocrome-scale
index_grey = 90;

hfig = figure;
set(hfig,'Units','inches');
set(hfig,'Position',[1 1 17 6]);

subplot(1,2,1)
J = area(100*b_grid/mean_gdp, exp(y_grid(contour_plot(:,2))), exp(y_grid(1)));
set(gca,'FontSize',18,'TickLabelInterpreter','latex','FontName','Times New Roman')
set(J,'FaceColor',[bw_grid(floor(index_grey/4)) bw_grid(floor(index_grey/4)) bw_grid(floor(index_grey/4))]);
set(J,'LineStyle','none','LineWidth',.01)
hold on
H = area(100*b_grid/mean_gdp, exp(y_grid(contour_plot(:,4))), exp(y_grid(1)));
set(H,'FaceColor',[bw_grid(index_grey) bw_grid(index_grey) bw_grid(index_grey)]);
set(H,'LineStyle','none','LineWidth',.01)
axis([0 60 exp(y_grid(1)) exp(y_grid(y_num))]);
xlabel('Debt/$E(y)$ (percent)','FontSize',20,'Interpreter','latex','FontName','Times New Roman');
ylabel('Productivity','FontSize',20,'Interpreter','latex','FontName','Times New Roman');
grid off
hold off
leg1 = legend([J(1); H(1)], {'Low $\varepsilon$ shock', 'High $\varepsilon$ shock'});

set(leg1,'Interpreter','latex','FontName','Times New Roman');
set(leg1,'FontSize',16);
set(leg1,'Location','NorthWest','Orientation','vertical');
legend boxoff

i_y_pointer = ceil(y_num/2);

subplot(1,2,2)
H = plot(100*b_grid/mean_gdp, [q_matrix(:, i_y_pointer-6) q_matrix(:, i_y_pointer+6)]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex','FontName','Times New Roman')
set(H(1), 'LineStyle','--','LineWidth',2,'MarkerSize',.1,'Color','r')
set(H(2), 'LineStyle','-','LineWidth',2,'MarkerSize',.1,'Color','k')
xlabel('Next-period debt/$E(y)$ (percent)','Interpreter','latex','FontSize',20,'FontName','Times New Roman')
ylabel('Bond price','Interpreter','latex','FontSize',20,'FontName','Times New Roman')
leg1 = legend('Low productivity', 'High productivity');
set(leg1,'Interpreter','latex','FontName','Times New Roman');
set(leg1,'FontSize',16);
set(leg1,'Location','NorthEast');
axis([0 60 0 1.1]);
legend boxoff

%sgtitle('Figure 5: Default sets and bond prices', 'Interpreter','latex','FontSize',18)

drawnow

exportgraphics(hfig, fullfile(outdir,'Fig_default_bondprice.png'),'Resolution',400)





%NOTE: HERE WE SAVE THE MEAN_GDP, TO BE USED WHEN CONSTRUCTING MOST OF THE
%PLOTS. 
Mean_GDP = mean(mean(last_y(excluded_indicator==0)));
csvwrite('mean_gdp.txt',Mean_GDP)

%% Figure 6: Conditional and unconditional debt distributions 

hfig = figure;
set(hfig,'Units','inches');
set(hfig,'Position',[1 1 8 6]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex')
hold on
histogram(100*last_b_next(excluded_indicator==0)./last_y(excluded_indicator==0), ...
    'Normalization','probability','BinWidth',1,'BinLimits',[10,25])
histogram(100*last_b_next(bc_indicator==1&excluded_indicator==0)./last_y(bc_indicator==1&excluded_indicator==0), ...
    'Normalization','probability','BinWidth',1,'BinLimits',[10,25])

yticks([0:0.05:0.4])
ax = gca;
ax.YAxis.FontSize = 18;
ax.XAxis.FontSize = 18;
leg1=legend('unconditional','banking crisis');
set(leg1, 'Interpreter','latex')
set(leg1,'FontSize',16);
ylabel('Probability','Interpreter','latex', 'fontsize', 20)
xlabel('Debt / Mean output','Interpreter','latex', 'fontsize', 20)
%title('Figure 6: Conditional and unconditional debt destributions', 'Interpreter', 'latex', 'FontSize', 16)
legend boxoff
hold off


drawnow

exportgraphics(hfig, fullfile(outdir,'hist_debt.png'),'Resolution',400)



%% Figure 7: The effect of bailouts on output and taxes

transfer_opt = transfer_matrix(:,:,:,2)./repmat(AA.*(1-reshape(epsilon_grid,[1,1,epsilon_num])),[b_num y_num 1]);
[~,it_opt] = min(abs(tr_grid-transfer_opt(6,13,3)));

hfig = figure;

set(hfig,'Units','inches');
set(hfig,'Position',[1 1 18 6]);

subplot(1,2,1)
H = plot(100*tr_grid(1:45), 100*[...
    output_cands(bp_choice(it_opt,1),1:45,1)'/output_cands(bp_choice(it_opt,1),1,1) ...
    output_cands(bp_choice(it_opt,2),1:45,2)'/output_cands(bp_choice(it_opt,2),1,2)], ...
    100*transfer_opt(6,13,3), 100*output_cands(bp_choice(it_opt,1),it_opt,1)/output_cands(bp_choice(it_opt,1),1,1),'ok', ...
    100*transfer_opt(6,13,3), 100*output_cands(bp_choice(it_opt,2),it_opt,2)/output_cands(bp_choice(it_opt,2),1,2),'sb');
set(gca,'FontSize',17,'TickLabelInterpreter','latex')
set(H(1), 'LineStyle','-','LineWidth',2.5,'MarkerSize',.1,'Color','k')
set(H(2), 'LineStyle','-.','LineWidth',2.5,'MarkerSize',1,'Color','r')
set(H(3), 'LineWidth',2.5,'MarkerSize',15,'Color','k')
set(H(4), 'LineWidth',2.5,'MarkerSize',15,'Color','r')
xlabel('Transfers (percent of potential loss)','Interpreter','latex','FontSize',20)
ylabel('Output (index, no transfer = 100)','Interpreter','latex','FontSize',20)
leg1 = legend([H(1:2)], {'No banking crisis', 'Banking crisis'});
set(leg1,'Interpreter','latex');
set(leg1,'FontSize',16);
set(leg1,'Location','SouthWest');
legend boxoff
axis([0 80 90 110]);
yticks([90 95 100 105 110]);

subplot(1,2,2)
H = plot(100*tr_grid(1:45), 100*[...
    tax_vector(bp_choice(it_opt,1),1:45,1)' tax_vector(bp_choice(it_opt,2),1:45,2)'], ...
    100*transfer_opt(6,13,3), 100*tax_vector(bp_choice(it_opt,1),it_opt,1) ,'ok', ...
    100*transfer_opt(6,13,3), 100*tax_vector(bp_choice(it_opt,2),it_opt,2),'sb');
set(gca,'FontSize',17,'TickLabelInterpreter','latex')
set(H(1), 'LineStyle','-','LineWidth',2.5,'MarkerSize',.1,'Color','k')
set(H(2), 'LineStyle','-.','LineWidth',2.5,'MarkerSize',1,'Color','r')
set(H(3), 'LineWidth',2.5,'MarkerSize',15,'Color','k')
set(H(4), 'LineWidth',2.5,'MarkerSize',15,'Color','r')
xlabel('Transfers (percent of potential loss)','Interpreter','latex','FontSize',20)
ylabel('Tax rate (percent)','Interpreter','latex','FontSize',20)
leg1 = legend([H(1:2)], {'No banking crisis', 'Banking crisis'});
set(leg1,'Interpreter','latex');
set(leg1,'FontSize',16);
set(leg1,'Location','SouthWest');
legend boxoff
xlim([0 50]);

%sgtitle('Figure 7: The effect of bailouts on output and taxes', 'Interpreter','latex','FontSize',18)

drawnow

exportgraphics(hfig, fullfile(outdir,'Fig_transfer_output_tax_fixedbprime.png'),'Resolution',400)

%% Figure 8: Bailout policy

hfig = figure('WindowStyle','normal');
set(hfig,'Units','inches');
set(hfig,'Position',[1 1 18 6]);


i_y_pointer = floor(y_num/2);
ylim([0 120])
subplot(1,2,1)
H = plot(100*b_grid/mean_gdp, ...
    100*[transfer_matrix(:,i_y_pointer,2,2)./(AA*epsilon_grid(2)) ...
         transfer_matrix(:,i_y_pointer,4,2)./(AA*epsilon_grid(4))]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex')
set(H(1), 'LineStyle','--','LineWidth',2.5,'MarkerSize',.1,'Color','r')
set(H(2), 'LineStyle','-','LineWidth',2.5,'MarkerSize',.1,'Color','k')
xlabel('Debt/$E(y)$ (percent)','Interpreter','latex','FontSize',20)
ylabel('Transfers (percent of potential loss)','Interpreter','latex','FontSize',20)
leg1 = legend('Low $\varepsilon$ shock', 'High $\varepsilon$ shock');
set(leg1,'Interpreter','latex');
set(leg1,'FontSize',16);
set(leg1,'Location','NorthEast');
legend boxoff
axis([0 30 0 110]);
yticks([0 20 40 60 80 100 ]);

i_y_pointer = ceil(y_num/2);

subplot(1,2,2)
H = plot(100*b_grid/mean_gdp, ...
    100*[transfer_matrix(:,i_y_pointer-6,3,2)./(AA*epsilon_grid(3)) ...
         transfer_matrix(:,i_y_pointer+6,3,2)./(AA*epsilon_grid(3))]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex')
set(H(1), 'LineStyle','--','LineWidth',2.5,'MarkerSize',.1,'Color','r')
set(H(2), 'LineStyle','-','LineWidth',2.5,'MarkerSize',.1,'Color','k')
xlabel('Debt/$E(y)$ (percent)','Interpreter','latex','FontSize',20)
ylabel('Transfers (percent of potential loss)','Interpreter','latex','FontSize',20)
leg1 = legend('Low productivity', 'High productivity');
set(leg1,'Interpreter','latex');
set(leg1,'FontSize',16);
set(leg1,'Location','NorthEast');
legend boxoff
axis([0 30 0 110]);
yticks([0 20 40 60 80 100 ]);

%sgtitle('Figure 8: Bailout policy', 'Interpreter','latex','FontSize',18)

drawnow


exportgraphics(hfig, fullfile(outdir,'Fig_transfer_policy_combined.png'),'Resolution',400)



%% Figure 9: Optimal bailout restrictions

results_constphi_table = readtable('results_constphi.csv');
phi_grid_constphi = results_constphi_table.T_max_frac;
welfmaxtmax = zeros(1, 12);
for i = 1:12
    w_i = results_constphi_table.(sprintf('avg_welf_b%d', i));
    w_max = max(w_i);
    tied = abs(w_i - w_max) < 1e-4;
    welfmaxtmax(i) = max(phi_grid_constphi(tied));
end




hfig = figure('WindowStyle','normal');
set(hfig,'Units','inches');
set(hfig,'Position',[1 1 10 6]);
H = plot(100*b_grid(1:12)/mean_gdp, welfmaxtmax);
set(gca,'FontSize',18,'TickLabelInterpreter','latex')
set(H(1), 'LineStyle','-','LineWidth',2.5,'MarkerSize',.1,'Color','k')

xlabel('Debt/$E(y)$ (percent)','Interpreter','latex','FontSize',20)
ylabel('Welfare maximizing $\phi$','Interpreter','latex','FontSize',20)
%title('Figure 9: Optimal bailout restrictions', 'Interpreter','latex','FontSize',16)

axis([0 23.5 0 1.0]);
yticks([0 0.2 0.4 0.6 0.8 1]);

drawnow

exportgraphics(hfig, fullfile(outdir,'Fig_welfare_maxtmax.png'),'Resolution',400)



%% Figure 10 : Bond prices with and without bailouts

hfig = figure('WindowStyle','normal');
set(hfig,'Units','inches');
set(hfig,'Position',[1 1 10 6]);
H = plot(100*b_grid/mean_gdp, [q_matrix(:, i_y_pointer) q_matrix_nobail(:, i_y_pointer)]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex')
set(H(1), 'LineStyle','--','LineWidth',2.5,'MarkerSize',.1,'Color','r')
set(H(2), 'LineStyle','-','LineWidth',2.5,'MarkerSize',.1,'Color','k')
%    set(H(3), 'LineStyle',':','LineWidth',2,'MarkerSize',.1,'Color','b')
xlabel('Next-period debt/$E(y)$ (percent)','Interpreter','latex','FontSize',20)
ylabel('Bond price','Interpreter','latex','FontSize',20)
%title('Figure 10: Bond prices with and without bailouts', 'Interpreter','latex','FontSize',16)
leg1 = legend('With bailouts','No bailouts');
set(leg1,'Interpreter','latex');
set(leg1,'FontSize',16);
set(leg1,'Location','NorthEast');
axis([0 60 0 1.1]);
legend boxoff

drawnow

exportgraphics(hfig, fullfile(outdir,'Fig_bond_price_compare.png'),'Resolution',400)



%% Figure 11 : Private Consumption

hfig = figure('WindowStyle','normal');
pos = get(hfig,'position');
set(hfig,'Units','inches');
set(hfig,'Position',[1 1 18 6]);

subplot(1,2,1)
H = plot(100*b_grid/Mean_GDP, [cons_matrix(:,7,2,1) cons_nb_matrix(:,7,2,1)]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex')
set(H(1), 'LineStyle','--','LineWidth',2.5,'MarkerSize',.1,'Color','r')
set(H(2), 'LineStyle','-','LineWidth',2.5,'MarkerSize',.1,'Color','k')
xlabel('Debt/$E(y)$ (percent)','Interpreter','latex','FontSize',20)
ylabel('Consumption','Interpreter','latex','FontSize',20)
%title('No banking crisis','Interpreter','latex','FontSize',16)
leg1 = legend('With bailouts','No bailouts');
set(leg1,'Interpreter','latex');
set(leg1,'FontSize',16);
set(leg1,'Location','NorthEast');
xlim([0 40]);
ylim([0.2 1.0]);
yticks([0.2 0.4 0.6 0.8 1.0]);

legend boxoff
grid off

subplot(1,2,2)
H = plot(100*b_grid/Mean_GDP, [cons_matrix(:,7,2,2) cons_nb_matrix(:,7,2,2)]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex')
set(H(1), 'LineStyle','--','LineWidth',2.5,'MarkerSize',.1,'Color','r')
set(H(2), 'LineStyle','-','LineWidth',2.5,'MarkerSize',.1,'Color','k')
xlabel('Debt/$E(y)$ (percent)','Interpreter','latex','FontSize',20)
ylabel('Consumption','Interpreter','latex','FontSize',20)
%title('Banking crisis','Interpreter','latex','FontSize',16)
leg1 = legend('With bailouts','No bailouts');
set(leg1,'Interpreter','latex');
set(leg1,'FontSize',16);
set(leg1,'Location','NorthEast');
xlim([0 40]);
ylim([0.2 1.0]);
yticks([0.2 0.4 0.6 0.8 1.0]);

legend boxoff
grid off

%sgtitle('Figure 11: Private Consumption', 'Interpreter','latex','FontSize',17)

drawnow

exportgraphics(hfig, fullfile(outdir,'Fig_cons_combined.png'),'Resolution',400)



%% Figure 12 : Value functions with and without bailouts

hfig = figure('WindowStyle','normal');
set(hfig,'Units','inches');
set(hfig,'Position',[1 1 10 6]);
H = plot(100*b_grid/mean_gdp,[value_matrix(:,7,2,1) value_matrix_nobail(:,7,2,1) ]);
set(gca,'FontSize',18,'TickLabelInterpreter','latex')
set(H(1), 'LineStyle','--','LineWidth',2.5,'MarkerSize',.1,'Color','r')
set(H(2), 'LineStyle','-','LineWidth',2.5,'MarkerSize',.1,'Color','k')
%    set(H(3), 'LineStyle',':','LineWidth',2,'MarkerSize',.1,'Color','b')
xlabel('Debt/$E(y)$','Interpreter','latex','FontSize',20)
ylabel('Value function','Interpreter','latex','FontSize',20)
%title('Figure 12: Value functions with and without bailouts', 'Interpreter','latex','FontSize',16)
leg1 = legend('With bailouts','No bailouts');
set(leg1,'Interpreter','latex');
set(leg1,'FontSize',16);
set(leg1,'Location','NorthEast');
axis([0 40 -13.5 -11.5]);
xticks([0 10 20 30 40]);
legend boxoff


drawnow

exportgraphics(hfig, fullfile(outdir,'fig_value.png'),'Resolution',400)








