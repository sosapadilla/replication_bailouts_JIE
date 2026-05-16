function run_sensitivity_batch()

% Workflow for each case:
%   1) Write params_current.nml for the NO-BAILOUT run
%   2) Run solver in a dedicated working directory
%   3) Copy the welfare benchmark files into bailout_dir/nb_files
%   4) Write params_current.nml for the BAILOUT run
%   5) Run solver in a dedicated working directory
%   6) Read summary_simulation.txt from both runs and append to final tables
%
% Assumptions:
%   - The modified Fortran file reads params_current.nml from the current
%     working directory.
%   - The solver writes summary_results.txt in the current working
%     directory.
%   - Welfare in the bailout run reads the files in nb_files/.
%
% Edit the CASES block below to control which parameter values are used.

clc;
fprintf('\n=== Sensitivity batch runner ===\n');
setenv('PATH', ['/opt/homebrew/bin:' getenv('PATH')]);
%% ---------------- USER SETTINGS ----------------
rootDir    = pwd;
fortranSrcNB = fullfile(rootDir, 'bailouts_optimal_nb.f90');
fortranSrcBL = fullfile(rootDir, 'bailouts_optimal_bl.f90');
exeNameNB   = 'bailouts_optimal_nb.out';
exeNameBL   = 'bailouts_optimal_bl.out';
configNum  = 1;                  % first command-line argument to the solver
compileNow = true;               % set false if already compiled
optFlags   = '-O3';

% Baseline values used in the paper/code. Adjust if needed.
baseline = struct();
baseline.writeout                  = 1;
baseline.simulonly                 = 0;
baseline.maxiter                   = 100;
baseline.runwelf                   = 1;
baseline.indicator_external        = 0.0;
baseline.indicator_bailout_default = 0.0;
baseline.beta                      = 0.81232233;
baseline.AA                        = 0.27552786;
baseline.gov_spending              = 0.14646447;
baseline.sig_e                     = 4.2570787;
baseline.T_max_frac                = 1.0;
baseline.prob_excl_end             = 0.5;
baseline.theta                     = 1.0;
baseline.omega                     = 2.5;
baseline.alpha                     = 0.7;
baseline.gamma_firm                = 0.52;
baseline.pi                        = 0.03;
baseline.phi_0                     = 1.0;
baseline.phi_e                     = 0.0;
baseline.phi_y                     = 0.0;
baseline.phi_b                     = 0.0;
baseline.transfer_num_use          = 50;

% ---- CASES TO RUN ----
% Add/remove/edit rows here. The script will loop in this order.
% table_name/panel_name are just labels for the final table.
%cases = { ...
%    %makeCase('Sensitivity to A', 'Low A',  'AA', 0.26), ...
%    makeCase('Sensitivity to A', 'High A', 'AA', 0.30) ...
%};

 cases = { ...
     makeCase('Sensitivity', 'Low A',           'AA',            0.26), ...
     makeCase('Sensitivity', 'High A',          'AA',            0.30), ...
     makeCase('Sensitivity', 'Low sigma_e',     'sig_e',         3.76), ...
     makeCase('Sensitivity', 'High sigma_e',    'sig_e',         4.76), ...
     makeCase('Sensitivity', 'Low omega',       'omega',         2.30), ...
     makeCase('Sensitivity', 'High omega',      'omega',         2.70), ...
     makeCase('Sensitivity', 'Low beta',        'beta',          0.76), ...
     makeCase('Sensitivity', 'High beta',       'beta',          0.86), ...
     makeCase('Sensitivity', 'Low gamma',       'gamma_firm',    0.49), ...
     makeCase('Sensitivity', 'High gamma',      'gamma_firm',    0.55), ...
     makeCase('Sensitivity', 'Low pi',          'pi',            0.01), ...
     makeCase('Sensitivity', 'High pi',         'pi',            0.10), ...
     makeCase('Sensitivity', 'Low alpha',       'alpha',         0.65), ...
     makeCase('Sensitivity', 'High alpha',      'alpha',         0.75), ...
     makeCase('Sensitivity', 'Low theta',       'prob_excl_end', 0.40), ...
     makeCase('Sensitivity', 'High theta',      'prob_excl_end', 0.60)  ...
     };

% Files that the welfare routine expects in nb_files/
nbFiles = { ...
    'graphs_default_dss.txt', ...
    'graphs_b_next.txt', ...
    'graphs_labor.txt', ...
    'graphs_consumption.txt', ...
    'graphs_cons_bank.txt'};

batchDir = fullfile(rootDir, 'sensitivity_batch_runs');
if ~exist(batchDir, 'dir'); mkdir(batchDir); end

%% ---------------- COMPILE ----------------
if compileNow
    assert(isfile(fortranSrcNB), 'Cannot find no-bailout Fortran source: %s', fortranSrcNB);
    assert(isfile(fortranSrcBL), 'Cannot find bailout Fortran source: %s', fortranSrcBL);

    cmd = sprintf('gfortran %s "%s" -o "%s"', optFlags, fortranSrcNB, fullfile(rootDir, exeNameNB));
    runShell(cmd, rootDir, 'No-bailout Fortran compilation failed.');

    cmd = sprintf('gfortran %s "%s" -o "%s"', optFlags, fortranSrcBL, fullfile(rootDir, exeNameBL));
    runShell(cmd, rootDir, 'Bailout Fortran compilation failed.');
end
assert(isfile(fullfile(rootDir, exeNameNB)), 'No-bailout executable not found: %s', fullfile(rootDir, exeNameNB));
assert(isfile(fullfile(rootDir, exeNameBL)), 'Bailout executable not found: %s', fullfile(rootDir, exeNameBL));

%% ---------------- LOOP ----------------
longRows = struct([]);
wideRows = struct([]);

for i = 1:numel(cases)
    c = cases{i};
    caseSlug = sprintf('%02d_%s', i, slugify(c.panel_name));
    caseRoot = fullfile(batchDir, caseSlug);
    nbRunDir = fullfile(caseRoot, 'no_bailout');
    blRunDir = fullfile(caseRoot, 'bailout');
    ensureCleanDir(caseRoot);
    mkdir(nbRunDir);
    mkdir(blRunDir);
    mkdir(fullfile(blRunDir, 'nb_files'));

    fprintf('\n========================================\n');
    fprintf('Case %d / %d\n', i, numel(cases));
    fprintf('Table      : %s\n', c.table_name);
    fprintf('Panel      : %s\n', c.panel_name);
    fprintf('Parameter  : %s = %.10f\n', c.param_name, c.param_value);
    fprintf('========================================\n');

    % Copy executable into both run folders for simpler relative-path calls.
    copyfile(fullfile(rootDir, exeNameNB), fullfile(nbRunDir, exeNameNB));
    copyfile(fullfile(rootDir, exeNameBL), fullfile(blRunDir, exeNameBL));

    %% Step 1: no bailout
    p_nb = baseline;
    p_nb.(c.param_name) = c.param_value;
    p_nb.runwelf = 0;
    p_nb.T_max_frac = 0.0;   % no bailouts
    p_nb.transfer_num_use = 1;      % no bailouts
    p_nb.simulonly = 0;
    p_nb.indicator_external = 0.0;
    p_nb.writeout = 1;

    writeParamsNml(fullfile(nbRunDir, 'params_current.nml'), p_nb);
    fprintf('Step 1/2: solving NO-BAILOUT model...\n');
    runShell(sprintf('./%s %d', exeNameNB, configNum), nbRunDir, 'No-bailout run failed.');

    % Copy benchmark files into bailout/nb_files
    fprintf('Copying benchmark files into bailout/nb_files ...\n');
    for k = 1:numel(nbFiles)
        src = fullfile(nbRunDir, nbFiles{k});
        dst = fullfile(blRunDir, 'nb_files', nbFiles{k});
        assert(isfile(src), 'Missing no-bailout benchmark file: %s', src);
        copyfile(src, dst);
    end

    %% Step 2: bailout
    p_bl = baseline;
    p_bl.(c.param_name) = c.param_value;
    p_bl.runwelf = 1;
    p_bl.T_max_frac = 1.0;
    p_bl.transfer_num_use = 50;
    p_bl.simulonly = 0;
    p_bl.indicator_external = 0.0;
    p_bl.writeout = 1;

    writeParamsNml(fullfile(blRunDir, 'params_current.nml'), p_bl);
    fprintf('Step 2/2: solving BAILOUT model...\n');
    runShell(sprintf('./%s %d', exeNameBL, configNum), blRunDir, 'Bailout run failed.');

    %% Read summaries
    nbSummary = readSummaryFile(fullfile(nbRunDir, 'summary_results.txt'));
    blSummary = readSummaryFile(fullfile(blRunDir, 'summary_results.txt'));

    welfareGain = getFieldOrNaN(blSummary, 'welfare_gain');

    % Long format
row1 = makeLongRow(c, 'Model without bailouts', nbSummary, NaN);
row2 = makeLongRow(c, 'Baseline model',         blSummary, welfareGain);


if isempty(longRows)
    longRows = row1;
    longRows(end+1) = row2;
else
    longRows(end+1) = row1; 
    longRows(end+1) = row2; 
end

% Wide format
wrow = makeWideRow(c, nbSummary, blSummary, welfareGain);

if isempty(wideRows)
    wideRows = wrow;
else
    wideRows(end+1) = wrow; 
end
    fprintf('Finished case %d: %s\n', i, c.panel_name);
end

%% ---------------- WRITE OUTPUT TABLES ----------------
longTable = struct2table(longRows);
wideTable = struct2table(wideRows);

writetable(longTable, fullfile(batchDir, 'sensitivity_results_long.csv'));
writetable(wideTable, fullfile(batchDir, 'sensitivity_results_wide.csv'));

fprintf('\nDone.\n');
fprintf('Long table : %s\n', fullfile(batchDir, 'sensitivity_results_long.csv'));
fprintf('Wide table : %s\n', fullfile(batchDir, 'sensitivity_results_wide.csv'));
end

%% ========================= HELPERS =========================
function c = makeCase(table_name, panel_name, param_name, param_value)
c = struct('table_name', table_name, ...
           'panel_name', panel_name, ...
           'param_name', param_name, ...
           'param_value', param_value);
end

function writeParamsNml(filename, p)
fid = fopen(filename, 'w');
assert(fid > 0, 'Could not create %s', filename);
fprintf(fid, '&runtime_controls\n');
writeScalar(fid, 'writeout',                  p.writeout, true);
writeScalar(fid, 'simulonly',                 p.simulonly, true);
writeScalar(fid, 'maxiter',                   p.maxiter, true);
writeScalar(fid, 'runwelf',                   p.runwelf, true);
writeScalar(fid, 'indicator_external',        p.indicator_external, false);
writeScalar(fid, 'indicator_bailout_default', p.indicator_bailout_default, false);
writeScalar(fid, 'beta',                      p.beta, false);
writeScalar(fid, 'AA',                        p.AA, false);
writeScalar(fid, 'gov_spending',              p.gov_spending, false);
writeScalar(fid, 'sig_e',                     p.sig_e, false);
writeScalar(fid, 'T_max_frac',                p.T_max_frac, false);
writeScalar(fid, 'prob_excl_end',             p.prob_excl_end, false);
writeScalar(fid, 'theta',                     p.theta, false);
writeScalar(fid, 'omega',                     p.omega, false);
writeScalar(fid, 'alpha',                     p.alpha, false);
writeScalar(fid, 'gamma_firm',                p.gamma_firm, false);
writeScalar(fid, 'pi',                        p.pi, false);
writeScalar(fid, 'phi_0',                     p.phi_0, false);
writeScalar(fid, 'phi_e',                     p.phi_e, false);
writeScalar(fid, 'phi_y',                     p.phi_y, false);
writeScalar(fid, 'phi_b',                     p.phi_b, false);
writeScalar(fid, 'transfer_num_use',          p.transfer_num_use, true);
fprintf(fid, '/\n');
fclose(fid);
end

function writeScalar(fid, key, value, isInt)
if isInt
    fprintf(fid, '  %s = %d,\n', key, round(value));
else
    fprintf(fid, '  %s = %.15g,\n', key, value);
end
end

function runShell(cmd, workdir, errmsg)

    fullcmd = sprintf('cd "%s" && %s', workdir, cmd);

    fprintf('\n[Running]: %s\n\n', fullcmd);

    [status, ~] = system(fullcmd, '-echo');  % <-- THIS is the fix

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

function s = slugify(txt)
s = lower(strtrim(txt));
s = regexprep(s, '[^a-zA-Z0-9]+', '_');
s = regexprep(s, '^_+|_+$', '');
end

function summary = readSummaryFile(filename)
assert(isfile(filename), 'Missing summary file: %s', filename);
summary = struct();
raw = fileread(filename);
lines = regexp(raw, '\r?\n', 'split');
for i = 1:numel(lines)
    line = strtrim(lines{i});
    if isempty(line) || ~contains(line, '=')
        continue;
    end
    parts = strsplit(line, '=');
    key = strtrim(parts{1});
    val = str2double(strtrim(parts{2}));
    key = matlab.lang.makeValidName(key);
    summary.(key) = val;
end
end

function row = makeLongRow(c, modelName, s, welfareGain)
row = struct();
row.table_name               = string(c.table_name);
row.panel_name               = string(c.panel_name);
row.param_name               = string(c.param_name);
row.param_value              = c.param_value;
row.model                    = string(modelName);
row.default_freq_uncond      = getFieldOrNaN(s, 'default_freq_unconditional');
row.spread_mean              = getFieldOrNaN(s, 'spread_mean');
row.spread_sd                = getFieldOrNaN(s, 'spread_sd');
row.corr_spread_y            = getFieldOrNaN(s, 'corr_spread_y');
row.debt_gdp                 = getFieldOrNaN(s, 'debt_gdp');
row.mean_lending_rate        = getFieldOrNaN(s, 'mean_lending_rate');
row.welfare_gain_of_bailouts = welfareGain;
end

function row = makeWideRow(c, nb, bl, welfareGain)
row = struct();
row.table_name                    = string(c.table_name);
row.panel_name                    = string(c.panel_name);
row.param_name                    = string(c.param_name);
row.param_value                   = c.param_value;
row.baseline_default_uncond       = getFieldOrNaN(bl, 'default_freq_unconditional');
row.baseline_spread_mean          = getFieldOrNaN(bl, 'spread_mean');
row.baseline_spread_sd            = getFieldOrNaN(bl, 'spread_sd');
row.baseline_corr_spread_y   = getFieldOrNaN(bl, 'corr_spread_y');
row.baseline_debt_gdp             = getFieldOrNaN(bl, 'debt_gdp');
row.baseline_mean_lending_rate    = getFieldOrNaN(bl, 'mean_lending_rate');
row.nobail_default_uncond         = getFieldOrNaN(nb, 'default_freq_unconditional');
row.nobail_spread_mean            = getFieldOrNaN(nb, 'spread_mean');
row.nobail_spread_sd              = getFieldOrNaN(nb, 'spread_sd');
row.nobail_corr_spread_y     = getFieldOrNaN(nb, 'corr_spread_y');
row.nobail_debt_gdp               = getFieldOrNaN(nb, 'debt_gdp');
row.nobail_mean_lending_rate      = getFieldOrNaN(nb, 'mean_lending_rate');
row.welfare_gain_of_bailouts = welfareGain;
end

function v = getFieldOrNaN(s, field)
field = matlab.lang.makeValidName(field);
if isfield(s, field)
    v = s.(field);
else
    v = NaN;
end
end
