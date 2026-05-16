function run_table_c5_maturity()
%RUN_TABLE_C5_MATURITY Replicate Table C.5 using different debt maturities.
%
% For each maturity parameter mrate in {1.0, 0.75, 0.5, 0.25}:
%   1) Create patched temporary Fortran sources for no-bailout and bailout
%      versions by replacing the hard-coded line "mrate = 1.0d+0"
%   2) Compile the patched sources
%   3) Run the no-bailout model first
%   4) Copy nb_files into the bailout folder for welfare calculations
%   5) Run the bailout model
%   6) Read the bailout moments from summary_results.txt
%   7) Read welfare_gain from the bailout run (computed relative to nb_files)
%
% Output:
%   table_c5_results.csv
%
% Notes:
%   - This script reports ONLY the bailout-economy moments, matching Table C.5.
%   - The no-bailout run is still required to compute welfare_gain_of_bailouts.

clc;
fprintf('\n=== Table C.5 maturity runner ===\n');
setenv('PATH', ['/opt/homebrew/bin:' getenv('PATH')]);

%% ---------------- USER SETTINGS ----------------
rootDir      = pwd;
fortranSrcNB = fullfile(rootDir, 'bailouts_optimal_ltd_nb.f90');
fortranSrcBL = fullfile(rootDir, 'bailouts_optimal_ltd_bl.f90');
configNum    = 1;
optFlags     = '-O3';

assert(isfile(fortranSrcNB), 'Cannot find %s', fortranSrcNB);
assert(isfile(fortranSrcBL), 'Cannot find %s', fortranSrcBL);

% Baseline calibration.
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

% Table C.5 values: nu=1.0 is baseline; lower values imply longer maturity.
% See Table C.5 in the paper.
cases = { ...
    makeCase('1.0 (baseline)', 1.00), ...
    makeCase('0.75',           0.75), ...
    makeCase('0.5',            0.50), ...
    makeCase('0.25',           0.25)  ...
};

nbFiles = { ...
    'graphs_default_dss.txt', ...
    'graphs_b_next.txt', ...
    'graphs_labor.txt', ...
    'graphs_consumption.txt', ...
    'graphs_cons_bank.txt'};

batchDir = fullfile(rootDir, 'table_c5_maturity_runs');
if ~exist(batchDir, 'dir'); mkdir(batchDir); end

%% ---------------- LOOP ----------------
rows = struct([]);

for i = 1:numel(cases)
    c = cases{i};
    caseSlug = sprintf('%02d_mrate_%s', i, strrep(num2str(c.mrate, '%.2f'), '.', 'p'));
    caseRoot = fullfile(batchDir, caseSlug);
    nbRunDir = fullfile(caseRoot, 'no_bailout');
    blRunDir = fullfile(caseRoot, 'bailout');

    ensureCleanDir(caseRoot);
    mkdir(nbRunDir);
    mkdir(blRunDir);
    mkdir(fullfile(blRunDir, 'nb_files'));

    fprintf('\n========================================\n');
    fprintf('Case %d / %d\n', i, numel(cases));
    fprintf('mrate = %.2f\n', c.mrate);
    fprintf('========================================\n');

    % Patch sources locally for this case because mrate is hard-coded in the .f90 files.
    localSrcNB = fullfile(nbRunDir, 'bailouts_optimal_nb_case.f90');
    localSrcBL = fullfile(blRunDir, 'bailouts_optimal_bl_case.f90');
    patchMrateInSource(fortranSrcNB, localSrcNB, c.mrate);
    patchMrateInSource(fortranSrcBL, localSrcBL, c.mrate);

        exeNB = 'bailouts_optimal_nb.out';
    exeBL = 'bailouts_optimal_bl.out';

    reportTablesSrc = fullfile(rootDir, 'report_tables.f90');
    assert(isfile(reportTablesSrc), 'Cannot find %s', reportTablesSrc);

    cmd = sprintf('gfortran %s "%s" "%s" -o "%s"', ...
        optFlags, reportTablesSrc, localSrcNB, fullfile(nbRunDir, exeNB));
    runShell(cmd, nbRunDir, 'No-bailout Fortran compilation failed.');

    cmd = sprintf('gfortran %s "%s" "%s" -o "%s"', ...
        optFlags, reportTablesSrc, localSrcBL, fullfile(blRunDir, exeBL));
    runShell(cmd, blRunDir, 'Bailout Fortran compilation failed.');

    %% Step 1: no bailout
    p_nb = baseline;
    p_nb.runwelf = 0;
    p_nb.T_max_frac = 0.0;
    p_nb.transfer_num_use = 1;
    p_nb.simulonly = 0;
    p_nb.indicator_external = 0.0;
    p_nb.writeout = 1;

    writeParamsNml(fullfile(nbRunDir, 'params_current.nml'), p_nb);
    fprintf('Step 1/2: solving NO-BAILOUT model...\n');
    runShell(sprintf('./%s %d', exeNB, configNum), nbRunDir, 'No-bailout run failed.');

    fprintf('Copying benchmark files into bailout/nb_files ...\n');
    for k = 1:numel(nbFiles)
        src = fullfile(nbRunDir, nbFiles{k});
        dst = fullfile(blRunDir, 'nb_files', nbFiles{k});
        assert(isfile(src), 'Missing no-bailout benchmark file: %s', src);
        copyfile(src, dst);
    end

    %% Step 2: bailout
    p_bl = baseline;
    p_bl.runwelf = 1;
    p_bl.T_max_frac = 1.0;
    p_bl.transfer_num_use = 50;
    p_bl.simulonly = 0;
    p_bl.indicator_external = 0.0;
    p_bl.writeout = 1;

    writeParamsNml(fullfile(blRunDir, 'params_current.nml'), p_bl);
    fprintf('Step 2/2: solving BAILOUT model...\n');
    runShell(sprintf('./%s %d', exeBL, configNum), blRunDir, 'Bailout run failed.');

    %% Read bailout moments and welfare
    blSummary = readSummaryFile(fullfile(blRunDir, 'summary_results.txt'));
    welfareGain = getFieldOrNaN(blSummary, 'welfare_gain');

    row = struct();
    row.maturity_parameter_nu      = c.mrate;
    row.label                      = string(c.label);
    row.default_frequency          = getFieldOrNaN(blSummary, 'default_freq_unconditional');
    row.spread_mean                = getFieldOrNaN(blSummary, 'spread_mean');
    row.spread_sd                  = getFieldOrNaN(blSummary, 'spread_sd');
    row.corr_gdp_spread            = getFieldOrNaN(blSummary, 'corr_spread_y');
    row.debt_gdp                   = getFieldOrNaN(blSummary, 'debt_gdp');
    row.mean_lending_rate          = getFieldOrNaN(blSummary, 'mean_lending_rate');
    row.welfare_gain_of_bailouts   = welfareGain;

    if isempty(rows)
        rows = row;
    else
        rows(end+1) = row;
    end

    fprintf('Finished case %d: mrate = %.2f\n', i, c.mrate);
end

%% ---------------- WRITE OUTPUT ----------------
outTable = struct2table(rows);
writetable(outTable, fullfile(batchDir, 'table_c5_results.csv'));

fprintf('\nDone.\n');
fprintf('Output: %s\n', fullfile(batchDir, 'table_c5_results.csv'));
end

%% ========================= HELPERS =========================
function c = makeCase(label, mrate)
c = struct('label', label, 'mrate', mrate);
end

function patchMrateInSource(srcFile, dstFile, mrate)
raw = fileread(srcFile);
repl = sprintf('mrate   = %.15gd+0', mrate);
repl = strrep(repl, 'e+', 'd+');
repl = strrep(repl, 'e-', 'd-');
if ~contains(raw, 'mrate   = 1.0d+0') && ~contains(raw, 'mrate = 1.0d+0')
    error('Could not find hard-coded mrate assignment in %s', srcFile);
end
raw = regexprep(raw, 'mrate\s*=\s*[0-9.]+d[+-]?[0-9]+', repl, 'once');
fid = fopen(dstFile, 'w');
assert(fid > 0, 'Could not create %s', dstFile);
fwrite(fid, raw);
fclose(fid);
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
[status, ~] = system(fullcmd, '-echo');
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

function v = getFieldOrNaN(s, field)
field = matlab.lang.makeValidName(field);
if isfield(s, field)
    v = s.(field);
else
    v = NaN;
end
end
