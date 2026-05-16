function run_single_comparison_table_fixed()
% Compile and run a no-bailout code first, then a bailout code, and build
% one final comparison table matching the desired format.
%
% This version is robust to the current Fortran files writing results.out
% instead of summary_simulation.txt / summary_results.txt.

clc;
fprintf('\n=== Single comparison table runner ===\n');
setenv('PATH', ['/opt/homebrew/bin:' getenv('PATH')]);

rootDir = pwd;

noBailoutSrc = fullfile(rootDir, 'bailouts_optimal_nb.f90');
bailoutSrc   = fullfile(rootDir, 'bailouts_optimal_bl.f90');

assert(isfile(noBailoutSrc), 'Cannot find %s', noBailoutSrc);
assert(isfile(bailoutSrc),   'Cannot find %s', bailoutSrc);

runDir = fullfile(rootDir, 'single_comparison_run');
ensureCleanDir(runDir);

exeNB = 'no_bailouts.out';
exeBL = 'bailouts_during_default.out';

fprintf('Compiling no-bailout code...\n');
cmd = sprintf('gfortran -O3 "%s" -o "%s"', noBailoutSrc, exeNB);
runShell(cmd, runDir, 'No-bailout compilation failed.');

fprintf('Step 1/2: running no-bailout code...\n');
runShell(sprintf('./%s', exeNB), runDir, 'No-bailout run failed.');
nbOut = snapshotRunOutputs(runDir, 'no_bailouts');

nbFolder = fullfile(runDir, 'nb_files');
if exist(nbFolder, 'dir')
    rmdir(nbFolder, 's');
end
mkdir(nbFolder);

nbFiles = { ...
    'graphs_default_dss.txt', ...
    'graphs_b_next.txt', ...
    'graphs_labor.txt', ...
    'graphs_consumption.txt', ...
    'graphs_cons_bank.txt'};

for k = 1:numel(nbFiles)
    src = fullfile(runDir, nbFiles{k});
    dst = fullfile(nbFolder, nbFiles{k});
    assert(isfile(src), 'Missing no-bailout benchmark file: %s', src);
    copyfile(src, dst);
end

fprintf('Compiling bailout code...\n');
cmd = sprintf('gfortran -O3 "%s" -o "%s"', bailoutSrc, exeBL);
runShell(cmd, runDir, 'Bailout compilation failed.');

fprintf('Step 2/2: running bailout code...\n');
runShell(sprintf('./%s', exeBL), runDir, 'Bailout run failed.');
blOut = snapshotRunOutputs(runDir, 'bailouts');


nb = readFortranOutputs(nbOut);
bl = readFortranOutputs(blOut);

% Try common welfare field names. If none are present, leave blank.
welfare = NaN;
for key = {'welfare_gain','welfare_at_mean_debt','avg_welf'}
    f = matlab.lang.makeValidName(key{1});
    if isfield(bl,f)
        welfare = bl.(f);
        break;
    end
end

T = table( ...
    string({'Default frequency'; 'Sovereign spread mean'; 'Sovereign spread standard deviation'; ...
            'corr(GDP, spread)'; 'Debt/GDP'; 'Mean lending rate'; 'Welfare gain of bailouts'}), ...
    [100*getField(nb,'default_freq_unconditional'); ...
     100*getField(nb,'spread_mean'); ...
     100*getField(nb,'spread_sd'); ...
     getField(nb,'corr_spread_y'); ...
     100*getField(nb,'debt_gdp'); ...
     100*getField(nb,'mean_lending_rate'); ...
     NaN], ...
    [100*getField(bl,'default_freq_unconditional'); ...
     100*getField(bl,'spread_mean'); ...
     100*getField(bl,'spread_sd'); ...
     getField(bl,'corr_spread_y'); ...
     100*getField(bl,'debt_gdp'); ...
     100*getField(bl,'mean_lending_rate'); ...
     100*welfare], ...
    'VariableNames', {'Statistic','No_bailouts','Bailouts_during_default'});

outfile = fullfile(runDir, 'table_c1_simulated_moments.csv');
writetable(T, outfile);

fprintf('\nDone. Final table:\n  %s\n', outfile);
end

function info = snapshotRunOutputs(runDir, tag)
% Save whichever output file the current Fortran run produced.
info = struct();

candidates = { ...
    fullfile(runDir, 'summary_results.txt'), ...
    fullfile(runDir, 'summary_simulation.txt'), ...
    fullfile(runDir, 'results.out')};

src = '';
for i = 1:numel(candidates)
    if isfile(candidates{i})
        src = candidates{i};
        break;
    end
end
assert(~isempty(src), 'No summary-like file found in %s', runDir);

[~,~,ext] = fileparts(src);
dst = fullfile(runDir, sprintf('%s_output%s', tag, ext));
copyfile(src, dst);
info.path = dst;
info.kind = lower(ext);
end

function s = readFortranOutputs(info)
% Read either key=value text output or the numeric results.out line.
assert(isfield(info,'path') && isfile(info.path), 'Missing output file.');
[~,name,ext] = fileparts(info.path);

if strcmpi(ext, '.txt') && contains(lower(name), 'summary')
    s = readKeyValueSummary(info.path);
else
    s = readResultsOut(info.path);
end
end

function s = readKeyValueSummary(filename)
raw = fileread(filename);
lines = regexp(raw, '\r?\n', 'split');
s = struct();
for i = 1:numel(lines)
    line = strtrim(lines{i});
    if isempty(line) || ~contains(line, '=')
        continue;
    end
    parts = strsplit(line, '=');
    key = matlab.lang.makeValidName(strtrim(parts{1}));
    val = str2double(strtrim(parts{2}));
    if ~isnan(val)
        s.(key) = val;
    end
end
end

function s = readResultsOut(filename)
% Parse the single numeric line written by results.out.
raw = strtrim(fileread(filename));
nums = sscanf(raw, '%f').';
assert(numel(nums) >= 40, 'Unexpected results.out format in %s', filename);

% Layout from WRITE(5,501):
% 1 num
% 2 beta
% 3 AA
% 4 sig_e
% 5 gov_spending
% 6 T_max_frac
% 7 phi_0
% 8 phi_b
% 9 phi_e
% 10 phi_y
% 11 avg_loan_to_y*100
% 12 avg_transfer_to_y*100
% 13 def_prob_unconditional*100
% 14 def_prob_conditional*100
% 15 avg_g_to_y*100
% 16 avg_std_log_y*100
% 17 avg_b_to_y*100
% 18 avg_welf*100
% 19:38 avg_welf_b(1:20)*100
% 39 deviation
% 40 dev_q
% 41 iteration
% 42 avg_spread*100
% 43 avg_std_spread*100
% 44 avg_corr_spread_y
% 45 avg_rr*100
% 46 prob_Tmax_binding
% 47 prob_Tmax_negative

s = struct();
s.default_freq_unconditional = nums(13) / 100;
s.default_freq_conditional_bc = nums(14) / 100;
s.spread_mean                = nums(42) / 100;
s.spread_sd                  = nums(43) / 100;
s.corr_spread_y              = nums(44);
s.debt_gdp                   = nums(17) / 100;
s.mean_lending_rate          = nums(45) / 100;
s.avg_welf                   = nums(18) / 100;
end

function v = getField(s, name)
name = matlab.lang.makeValidName(name);
if isfield(s, name)
    v = s.(name);
else
    v = NaN;
end
end

function runShell(cmd, workdir, errmsg)
    olddir = pwd;
    cleanup = onCleanup(@() cd(olddir));

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
