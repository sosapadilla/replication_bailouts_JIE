function run_bailout_nobailout_table()
% RUN_BAILOUT_NOBAILOUT_TABLE
% Compiles/runs two versions of bailouts_extensions.f90:
%   1) no bailout during default: indicator_bailout_default = 0
%   2) bailout during default:    indicator_bailout_default = 1
% Then reads each run's results.out and writes a comparison table.
%
% Put this .m file in the same folder as:
%   bailouts_extensions_nobailout.f90
%   bailouts_extensions_bailout.f90
% Then run:
%   run_bailout_nobailout_table

clc;
fprintf('\n=== Bailout vs no-bailout comparison runner ===\n');
setenv('PATH', ['/opt/homebrew/bin:' getenv('PATH')]);

rootDir = pwd;
noBailoutSrc = fullfile(rootDir, 'bailouts_extensions_nobailout.f90');
bailoutSrc   = fullfile(rootDir, 'bailouts_extensions_bailout.f90');

assert(isfile(noBailoutSrc), 'Cannot find %s', noBailoutSrc);
assert(isfile(bailoutSrc),   'Cannot find %s', bailoutSrc);

runDir = fullfile(rootDir, 'bailout_nobailout_comparison_run');
ensureCleanDir(runDir);

exeNB = 'bailouts_extensions_nobailout.out';
exeBL = 'bailouts_extensions_bailout.out';

fprintf('\nCompiling no-bailout code...\n');
runShell(sprintf('gfortran -O3 "%s" -o "%s"', noBailoutSrc, exeNB), runDir, ...
    'No-bailout compilation failed.');

fprintf('\nStep 1/2: running no-bailout code...\n');
runShell(sprintf('./%s', exeNB), runDir, 'No-bailout run failed.');
nbOut = snapshotRunOutputs(runDir, 'no_bailout');

% The bailout-welfare block reads the no-bailout policy files from nb_files/.
nbFolder = fullfile(runDir, 'nb_files');
ensureCleanDir(nbFolder);
copyIfExists(runDir, nbFolder, 'graphs_default_dss.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_b_next.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_labor.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_consumption.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_cons_bank.txt', true);
copyIfExists(runDir, nbFolder, 'graphs_var_e.txt', false);

fprintf('\nCompiling bailout code...\n');
runShell(sprintf('gfortran -O3 "%s" -o "%s"', bailoutSrc, exeBL), runDir, ...
    'Bailout compilation failed.');
fprintf('\nStep 2/2: running bailout code...\n');
runShell(sprintf('./%s', exeBL), runDir, 'Bailout run failed.');
blOut = snapshotRunOutputs(runDir, 'bailout');

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
        'Relative volatility'; ...
        'Welfare gain of bailouts' ...
    }), ...
    [100*nb.default_freq_unconditional; ...
     100*nb.spread_mean; ...
     100*nb.spread_sd; ...
     nb.corr_spread_y; ...
     100*nb.debt_gdp; ...
     100*nb.mean_lending_rate; ...
     bl.avg_var_e_nb; ...
     NaN], ...
    [100*bl.default_freq_unconditional; ...
     100*bl.spread_mean; ...
     100*bl.spread_sd; ...
     bl.corr_spread_y; ...
     100*bl.debt_gdp; ...
     100*bl.mean_lending_rate; ...
     bl.avg_var_e; ...
     100*bl.avg_welf], ...
    'VariableNames', {'Statistic','No_bailout','Bailout'});

csvFile  = fullfile(runDir, 'bailout_nobailout_results_table.csv');
xlsxFile = fullfile(runDir, 'bailout_nobailout_results_table.xlsx');
writetable(T, csvFile);
writetable(T, xlsxFile);

fprintf('\nDone. Final table written to:\n  %s\n  %s\n\n', csvFile, xlsxFile);
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
% Layout from the uploaded bailouts_extensions.f90 WRITE(5,501):
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
%42 = avg_var_e       ! bailout relative volatility
%43 = avg_var_e_nb    ! no-bailout relative volatility evaluated inside bailout code
raw = strtrim(fileread(filename));
nums = sscanf(raw, '%f').';
assert(numel(nums) >= 43, 'Unexpected results.out format in %s. Found only %d numbers.', filename, numel(nums));

s = struct();
s.beta                         = nums(2);
s.assets                       = nums(3);
s.sig_e                        = nums(4);
s.gov_spending                 = nums(5);
s.T_max_frac                   = nums(6);
s.transfer_gdp                 = nums(8)  / 100;
s.default_freq_unconditional   = nums(9)  / 100;
s.default_freq_conditional_bc  = nums(10) / 100;
s.debt_gdp                     = nums(13) / 100;
s.avg_welf                     = nums(14) / 100;
s.deviation                    = nums(35);
s.dev_q                        = nums(36);
s.iteration                    = nums(37);
s.spread_mean                  = nums(38) / 100;
s.spread_sd                    = nums(39) / 100;
s.corr_spread_y                = nums(40);
s.mean_lending_rate            = nums(41) / 100;
s.avg_var_e                    = nums(42) ;
s.avg_var_e_nb                 = nums(43);
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
