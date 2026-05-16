function run_storage_economy_table()
% RUN_STORAGE_ECONOMY_TABLE
% Replicates Table C.6-style simulated moments for the storage economy.
%
% Required files in the same folder as this .m file:
%   bailouts_storage_nobailout.f90
%   bailouts_storage_bailout.f90
%   param_calib_AA.txt
%   param_calib_alpha_k.txt
%   param_calib_beta.txt
%   param_calib_k_epsilon_frac.txt
%   param_calib_scale_mk.txt
%
% The script runs the no-bailout version first, copies its policy files into
% nb_files/, then runs the bailout version because the welfare block needs
% the no-bailout benchmark policy files.

clc;
fprintf('\n=== Storage-economy bailout vs no-bailout table runner ===\n');
setenv('PATH', ['/opt/homebrew/bin:' getenv('PATH')]);

rootDir = pwd;
noBailoutSrc = fullfile(rootDir, 'bailouts_storage_nobailout.f90');
bailoutSrc   = fullfile(rootDir, 'bailouts_storage_bailout.f90');

assert(isfile(noBailoutSrc), 'Cannot find %s', noBailoutSrc);
assert(isfile(bailoutSrc),   'Cannot find %s', bailoutSrc);

paramFiles = { ...
    'param_calib_AA.txt', ...
    'param_calib_alpha_k.txt', ...
    'param_calib_beta.txt', ...
    'param_calib_k_epsilon_frac.txt', ...
    'param_calib_scale_mk.txt'};
for p = 1:numel(paramFiles)
    assert(isfile(fullfile(rootDir, paramFiles{p})), 'Cannot find %s', fullfile(rootDir, paramFiles{p}));
end

runDir = fullfile(rootDir, 'storage_economy_replication_run');
ensureCleanDir(runDir);
copyParamFiles(rootDir, runDir, paramFiles);

exeNB = 'bailouts_storage_nobailout.out';
exeBL = 'bailouts_storage_bailout.out';

fprintf('\nCompiling no-bailout storage code...\n');
runShell(sprintf('gfortran -O3 "%s" -o "%s"', noBailoutSrc, exeNB), runDir, ...
    'No-bailout compilation failed.');

fprintf('\nStep 1/2: running no-bailout storage code...\n');
runShell(sprintf('./%s', exeNB), runDir, 'No-bailout run failed.');
nbOut = snapshotRunOutputs(runDir, 'no_bailout');
nb = readStorageResults(nbOut.storagePath);

% The bailout welfare block reads the no-bailout policy files from nb_files/.
nbFolder = fullfile(runDir, 'nb_files');
ensureCleanDir(nbFolder);
copyIfExists(runDir, nbFolder, 'graphs_default_dss.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_b_next.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_k_next.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_labor.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_consumption.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_cons_bank.txt', true);

fprintf('\nCompiling bailout storage code...\n');
runShell(sprintf('gfortran -O3 "%s" -o "%s"', bailoutSrc, exeBL), runDir, ...
    'Bailout compilation failed.');

fprintf('\nStep 2/2: running bailout storage code...\n');
runShell(sprintf('./%s', exeBL), runDir, 'Bailout run failed.');
blOut = snapshotRunOutputs(runDir, 'bailout');
bl = readStorageResults(blOut.storagePath);

T = table( ...
    string({ ...
        'Default frequency'; ...
        'Sovereign spread mean'; ...
        'Sovereign spread standard deviation'; ...
        'corr(GDP, spread)'; ...
        'Debt/GDP'; ...
        'Bailout/GDP'; ...
        'K/Assets'; ...
        'Mean lending rate'; ...
        'Welfare gain of bailouts' ...
    }), ...
    [bl.default_freq; ...
     bl.spread_mean; ...
     bl.spread_sd; ...
     bl.corr_spread_y; ...
     bl.debt_gdp; ...
     bl.bailout_gdp; ...
     bl.k_assets; ...
     bl.mean_lending_rate; ...
     bl.welfare_gain], ...
    [nb.default_freq; ...
     nb.spread_mean; ...
     nb.spread_sd; ...
     nb.corr_spread_y; ...
     nb.debt_gdp; ...
     nb.bailout_gdp; ...
     nb.k_assets; ...
     nb.mean_lending_rate; ...
     NaN], ...
    'VariableNames', {'Statistic','With_bailouts','No_bailouts'});

csvFile  = fullfile(runDir, 'storage_economy_results_table.csv');
xlsxFile = fullfile(runDir, 'storage_economy_results_table.xlsx');
writetable(T, csvFile);
writetable(T, xlsxFile);

fprintf('\nDone. Final Table C.6-style output written to:\n  %s\n  %s\n\n', csvFile, xlsxFile);
disp(T);
end

function copyParamFiles(rootDir, runDir, paramFiles)
for p = 1:numel(paramFiles)
    copyfile(fullfile(rootDir, paramFiles{p}), fullfile(runDir, paramFiles{p}));
end
end

function info = snapshotRunOutputs(runDir, tag)
resultsFile = fullfile(runDir, 'results.out');
storageFile = fullfile(runDir, 'results_storage.out');
assert(isfile(resultsFile), 'No results.out found in %s', runDir);
assert(isfile(storageFile), 'No results_storage.out found in %s', runDir);

info.resultsPath = fullfile(runDir, sprintf('%s_results.out', tag));
info.storagePath = fullfile(runDir, sprintf('%s_results_storage.out', tag));
copyfile(resultsFile, info.resultsPath);
copyfile(storageFile, info.storagePath);
end

function s = readStorageResults(filename)
% Layout of results_storage.out:
%  1 AA
%  2 alpha_k
%  3 k_epsilon_frac
%  4 scale_mk
%  5 beta
%  6 avg_loan_to_y*100
%  7 avg_transfer_to_y*100
%  8 def_prob_unconditional*100
%  9 def_prob_conditional*100
% 10 avg_g_to_y*100
% 11 avg_std_log_y*100
% 12 avg_b_to_y*100
% 13 avg_k_to_y*100
% 14 avg_welf*100
% 15 avg_spread*100
% 16 avg_std_spread*100
% 17 avg_corr_spread_y
% 18 avg_rr*100
% 19 deviation
% 20 dev_q
% 21 iteration
% 22 avg_exposure_1*100
% 23 avg_exposure_2*100
% 24 avg_sum_of_A*100
% 25 avg_frac_endog_A*100
% 26 avg_y
% 27 avg_k_to_assets*100
raw = strtrim(fileread(filename));
nums = sscanf(raw, '%f').';
assert(numel(nums) >= 27, 'Unexpected results_storage.out format in %s. Found only %d numbers.', filename, numel(nums));

s = struct();
s.AA                 = nums(1);
s.alpha_k            = nums(2);
s.k_epsilon_frac     = nums(3);
s.scale_mk           = nums(4);
s.beta               = nums(5);
s.loan_gdp           = nums(6);
s.bailout_gdp        = nums(7);
s.default_freq       = nums(8);
s.default_cond_bc    = nums(9);
s.gov_gdp            = nums(10);
s.std_log_y          = nums(11);
s.debt_gdp           = nums(12);
s.k_gdp              = nums(13);
s.welfare_gain       = nums(14);
s.spread_mean        = nums(15);
s.spread_sd          = nums(16);
s.corr_spread_y      = nums(17);
s.mean_lending_rate  = nums(18);
s.deviation          = nums(19);
s.dev_q              = nums(20);
s.iteration          = nums(21);
s.exposure_1         = nums(22);
s.exposure_2         = nums(23);
s.sum_of_A           = nums(24);
s.frac_endog_A       = nums(25);
s.avg_y              = nums(26);
s.k_assets           = nums(27);
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
