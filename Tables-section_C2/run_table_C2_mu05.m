function run_table_C2_mu05()
% RUN_TABLE_C2_MU05
% Replicates Table C.2 simulated moments for mu = 0.5.
%
% Required files in the same folder as this .m file:
%   bailouts_optimal_mu05_nobailout.f90
%   bailouts_optimal_mu05_bailout.f90
%
% The script runs the no-bailout case first, copies the no-bailout policy
% files into nb_files/, then runs the bailout case and writes an Excel table.

clc;
fprintf('\n=== Table C.2 mu = 0.5 replication runner ===\n');
setenv('PATH', ['/opt/homebrew/bin:' getenv('PATH')]);

rootDir = pwd;

noBailoutSrc = fullfile(rootDir, 'bailouts_optimal_mu05_nobailout.f90');
bailoutSrc   = fullfile(rootDir, 'bailouts_optimal_mu05_bailout.f90');

assert(isfile(noBailoutSrc), 'Cannot find %s', noBailoutSrc);
assert(isfile(bailoutSrc),   'Cannot find %s', bailoutSrc);

runDir = fullfile(rootDir, 'table_C2_mu05_replication_run');
ensureCleanDir(runDir);

exeNB = 'bailouts_optimal_mu05_nobailout.out';
exeBL = 'bailouts_optimal_mu05_bailout.out';

fprintf('\nCompiling no-bailout code...\n');
runShell(sprintf('rm -f PARAM.mod param.mod; gfortran -O3 "%s" -o "%s"', noBailoutSrc, exeNB), runDir, ...
    'No-bailout compilation failed.');

fprintf('\nStep 1/2: running no-bailout code...\n');
runShell(sprintf('./%s', exeNB), runDir, 'No-bailout run failed.');
nbOut = snapshotRunOutputs(runDir, 'no_bailout');

% The bailout welfare block reads the no-bailout policy files from nb_files/.
nbFolder = fullfile(runDir, 'nb_files');
ensureCleanDir(nbFolder);
copyIfExists(runDir, nbFolder, 'graphs_default_dss.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_b_next.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_labor.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_consumption.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_cons_bank.txt', true);

fprintf('\nCompiling bailout code...\n');
runShell(sprintf('rm -f PARAM.mod param.mod; gfortran -O3 "%s" -o "%s"', bailoutSrc, exeBL), runDir, ...
    'Bailout compilation failed.');

fprintf('\nStep 2/2: running bailout code...\n');
runShell(sprintf('./%s', exeBL), runDir, 'Bailout run failed.');
blOut = snapshotRunOutputs(runDir, 'with_bailouts');

nb = readResultsOut(nbOut.path);
bl = readResultsOut(blOut.path);

T = table( ...
    string({ ...
        'Default frequency'; ...
        'Sovereign spread mean'; ...
        'Sovereign spread standard deviation'; ...
        'corr(GDP, spread)'; ...
        'Debt/GDP'; ...
        'Mean lending rate'; ...
        'Welfare gain of bailouts' ...
    }), ...
    [100*bl.default_freq_unconditional; ...
     100*bl.spread_mean; ...
     100*bl.spread_sd; ...
     bl.corr_spread_y; ...
     100*bl.debt_gdp; ...
     100*bl.mean_lending_rate; ...
     100*bl.avg_welf], ...
    [100*nb.default_freq_unconditional; ...
     100*nb.spread_mean; ...
     100*nb.spread_sd; ...
     nb.corr_spread_y; ...
     100*nb.debt_gdp; ...
     100*nb.mean_lending_rate; ...
     NaN], ...
    'VariableNames', {'Statistic','With_bailouts','No_bailouts'});

csvFile  = fullfile(runDir, 'table_C2_mu05_simulated_moments.csv');
xlsxFile = fullfile(runDir, 'table_C2_mu05_simulated_moments.xlsx');

writetable(T, csvFile);
writetable(T, xlsxFile);

fprintf('\nDone. Final Table C.2-style output written to:\n  %s\n  %s\n\n', csvFile, xlsxFile);
disp(T);
end

function info = snapshotRunOutputs(runDir, tag)
src = fullfile(runDir, 'results.out');
assert(isfile(src), 'No results.out found in %s', runDir);
dst = fullfile(runDir, sprintf('%s_results.out', tag));
copyfile(src, dst);
info.path = dst;
end

function s = readResultsOut(filename)
% Layout from WRITE(5,501) in bailouts_optimal_mu05_*.f90:
%  1 num
%  2 beta
%  3 AA
%  4 sig_e
%  5 gov_spending
%  6 T_max_frac
%  7 avg_loan_to_y*100
%  8 avg_transfer_to_y*100
%  9 def_prob_unconditional*100
% 10 def_prob_conditional*100
% 11 avg_g_to_y*100
% 12 avg_std_log_y*100
% 13 avg_b_to_y*100
% 14 avg_welf*100
% 15:34 avg_welf_b(1:20)*100
% 35 deviation
% 36 dev_q
% 37 iteration
% 38 avg_spread*100
% 39 avg_std_spread*100
% 40 avg_corr_spread_y
% 41 avg_rr*100

raw = strtrim(fileread(filename));
nums = sscanf(raw, '%f').';
assert(numel(nums) >= 41, 'Unexpected results.out format in %s. Found only %d numbers.', filename, numel(nums));

s = struct();
s.beta                        = nums(2);
s.assets                      = nums(3);
s.sig_e                       = nums(4);
s.gov_spending                = nums(5);
s.T_max_frac                  = nums(6);
s.loan_to_y                   = nums(7)  / 100;
s.transfer_to_y               = nums(8)  / 100;
s.default_freq_unconditional  = nums(9)  / 100;
s.default_freq_conditional_bc = nums(10) / 100;
s.g_to_y                      = nums(11) / 100;
s.std_log_y                   = nums(12) / 100;
s.debt_gdp                    = nums(13) / 100;
s.avg_welf                    = nums(14) / 100;
s.deviation                   = nums(35);
s.dev_q                       = nums(36);
s.iteration                   = nums(37);
s.spread_mean                 = nums(38) / 100;
s.spread_sd                   = nums(39) / 100;
s.corr_spread_y               = nums(40);
s.mean_lending_rate           = nums(41) / 100;
end

function runShell(cmd, workdir, errmsg)
oldDir = pwd;
cleanup = onCleanup(@() cd(oldDir)); 
cd(workdir);
fprintf('\n[Running]: %s\n\n', cmd);
[status, ~] = system(cmd, '-echo');
if status ~= 0
    error(errmsg);
end
end

function ensureCleanDir(d)
if exist(d, 'dir')
    rmdir(d, 's');
end
mkdir(d);
end

function copyIfExists(runDir, dstDir, filename, required)
src = fullfile(runDir, filename);
dst = fullfile(dstDir, filename);
if isfile(src)
    copyfile(src, dst);
elseif required
    error('Missing no-bailout benchmark file required by bailout run: %s', src);
end
end
