function run_figure_c4_tk_replication()
% RUN_FIGURE_C4_TK_REPLICATION
% Reproduces Figure C.4 / storage-economy Gamma_k sensitivity figure.
%
% Workflow for each Gamma_k case:
%   1. Set param_calib_scale_mk.txt to Gamma_k x 100.
%   2. Feed in the pre-calibrated Abar value for that Gamma_k.
%   3. Run no-bailout first and copy benchmark policy files into nb_files/.
%   4. Run bailout economy and save final outputs.
%   5. Plot welfare gain of bailouts as a function of initial debt.

clc;
fprintf('\n=== Figure C.4 storage Gamma_k replication runner ===\n');
setenv('PATH', ['/opt/homebrew/bin:' getenv('PATH')]);

rootDir = pwd;
noBailoutSrc = fullfile(rootDir, 'bailouts_storage_nobailout.f90');
bailoutSrc   = fullfile(rootDir, 'bailouts_storage_bailout.f90');
assert(isfile(noBailoutSrc), 'Cannot find %s', noBailoutSrc);
assert(isfile(bailoutSrc),   'Cannot find %s', bailoutSrc);

baseParams = readBaselineParamFiles(rootDir);

gammaCases = [0.64, 0.74, 0.84];
scaleRawCases = round(100 * gammaCases);
aaRawCases = [273, 273, 258];

generatedRoot = fullfile(rootDir, 'figure_c4_tk_runs');
ensureCleanDir(generatedRoot);

summaryRows = struct('gamma_k', {}, 'scale_mk_raw', {}, 'AA_raw', {}, 'AA', {}, ...
    'bailout_gdp', {}, 'mean_debt_pct', {}, 'welfare_gain_pct', {});

for c = 1:numel(gammaCases)
    gammaVal = gammaCases(c);
    scaleRaw = scaleRawCases(c);
    caseFolder = sprintf('%02d_tk_%0.2f', c, gammaVal);
    caseRoot = fullfile(generatedRoot, caseFolder);
    ensureCleanDir(caseRoot);

    fprintf('\n==============================\n');
    fprintf('Processing Gamma_k = %.2f\n', gammaVal);
    fprintf('==============================\n');

    exeNB = 'bailouts_storage_nobailout.out';
    exeBL = 'bailouts_storage_bailout.out';

    fprintf('Compiling no-bailout storage code...\n');
    runShell(sprintf('gfortran -O3 "%s" -o "%s"', noBailoutSrc, exeNB), caseRoot, ...
        sprintf('No-bailout compilation failed for Gamma_k = %.2f.', gammaVal));

    fprintf('Compiling bailout storage code...\n');
    runShell(sprintf('gfortran -O3 "%s" -o "%s"', bailoutSrc, exeBL), caseRoot, ...
        sprintf('Bailout compilation failed for Gamma_k = %.2f.', gammaVal));

    aaBestRaw = aaRawCases(c);
    evalAA = aaBestRaw;
    evalObj = 0;
    evalStats = struct('AA_raw', aaBestRaw, 'scale_mk_raw', scaleRaw, ...
        'bailout_gdp', NaN, 'mean_debt_pct', NaN, ...
        'welfare_gain', NaN, 'success', true, 'error_message', '');

    fprintf('Using pre-calibrated AA raw value for Gamma_k = %.2f: %d\n', gammaVal, aaBestRaw);
    finalStats = runStoragePair(caseRoot, exeNB, exeBL, baseParams, scaleRaw, aaBestRaw, true);
    evalStats.bailout_gdp = finalStats.bailout_gdp;
    evalStats.mean_debt_pct = finalStats.mean_debt_pct;
    evalStats.welfare_gain = finalStats.welfare_gain;

    summaryRows(end+1) = struct( ...
        'gamma_k', gammaVal, ...
        'scale_mk_raw', scaleRaw, ...
        'AA_raw', aaBestRaw, ...
        'AA', aaBestRaw / 1000, ...
        'bailout_gdp', finalStats.bailout_gdp, ...
        'mean_debt_pct', finalStats.mean_debt_pct, ...
        'welfare_gain_pct', finalStats.welfare_gain); %#ok<AGROW>

    writeCaseSummary(caseRoot, gammaVal, scaleRaw, aaBestRaw, evalAA, evalObj, evalStats, finalStats);
end

summaryTable = struct2table(summaryRows);
summaryCsv = fullfile(generatedRoot, 'figure_c4_tk_calibration_summary.csv');
summaryXlsx = fullfile(generatedRoot, 'figure_c4_tk_calibration_summary.xlsx');
writetable(summaryTable, summaryCsv);
writetable(summaryTable, summaryXlsx);

figure_c4_welfare_tk(rootDir, generatedRoot);

fprintf('\nDone. Figure C.4 pipeline finished.\n');
fprintf('Summary files:\n  %s\n  %s\n', summaryCsv, summaryXlsx);
end

function [aaBestRaw, evalAA, evalObj, evalStats] = searchBestAA(caseRoot, exeNB, exeBL, baseParams, scaleRaw, targetBailoutGDP, lowerAA, upperAA)
opts = optimset('Display', 'off', 'TolX', 2, 'MaxFunEvals', 12, 'MaxIter', 12);
evalAA = [];
evalObj = [];
evalStats = struct('AA_raw', {}, 'scale_mk_raw', {}, 'bailout_gdp', {}, 'mean_debt_pct', {}, 'welfare_gain', {}, 'success', {}, 'error_message', {});

objfun = @(x) objectiveAA(x, caseRoot, exeNB, exeBL, baseParams, scaleRaw, targetBailoutGDP);
[xBest, ~] = fminbnd(objfun, lowerAA, upperAA, opts);
aaBestRaw = max(1, round(xBest));
    function obj = objectiveAA(x, caseRoot0, exeNB0, exeBL0, baseParams0, scaleRaw0, target0)
        aaRaw = max(1, round(x));
        idx = find(evalAA == aaRaw, 1);
        if ~isempty(idx)
            obj = evalObj(idx);
            return;
        end

        try
            statsFull = runStoragePair(caseRoot0, exeNB0, exeBL0, baseParams0, scaleRaw0, aaRaw, false);
            obj = abs(statsFull.bailout_gdp - target0);
            fprintf('  AA=%d -> bailout/GDP=%.4f, mean debt=%.4f, obj=%.6f\n', ...
                aaRaw, statsFull.bailout_gdp, statsFull.mean_debt_pct, obj);
            stats = struct('AA_raw', aaRaw, 'scale_mk_raw', scaleRaw0, ...
                'bailout_gdp', statsFull.bailout_gdp, 'mean_debt_pct', statsFull.mean_debt_pct, ...
                'welfare_gain', statsFull.welfare_gain, 'success', true, 'error_message', '');
        catch ME
            obj = 1.0e6 + aaRaw;
            stats = struct('AA_raw', aaRaw, 'scale_mk_raw', scaleRaw0, ...
                'bailout_gdp', NaN, 'mean_debt_pct', NaN, ...
                'welfare_gain', NaN, 'success', false, 'error_message', ME.message);
            fprintf('  AA=%d -> run failed (%s)\n', aaRaw, ME.message);
        end

        evalAA(end+1) = aaRaw;
        evalObj(end+1) = obj;
        evalStats(end+1) = stats;
    end
end

function base = readBaselineParamFiles(rootDir)
base = struct();
base.AA_raw = readSingleNumber(fullfile(rootDir, 'param_calib_AA.txt'));
base.alpha_k_raw = readSingleNumber(fullfile(rootDir, 'param_calib_alpha_k.txt'));
base.beta_raw = readSingleNumber(fullfile(rootDir, 'param_calib_beta.txt'));
base.k_epsilon_frac_raw = readSingleNumber(fullfile(rootDir, 'param_calib_k_epsilon_frac.txt'));
base.scale_mk_raw = readSingleNumber(fullfile(rootDir, 'param_calib_scale_mk.txt'));
end

function value = readSingleNumber(filename)
raw = strtrim(fileread(filename));
value = str2double(raw);
assert(isfinite(value), 'Could not parse numeric value from %s', filename);
end

function stats = runStoragePair(caseRoot, exeNB, exeBL, baseParams, scaleRaw, aaRaw, keepFinal)
workRoot = fullfile(caseRoot, '_work');
ensureCleanDir(workRoot);
noDir = fullfile(workRoot, 'no_bailout');
bailoutDir = fullfile(workRoot, 'bailout');
mkdir(noDir);
mkdir(bailoutDir);

copyfile(fullfile(caseRoot, exeNB), fullfile(noDir, exeNB));
copyfile(fullfile(caseRoot, exeBL), fullfile(bailoutDir, exeBL));

writeParamFiles(noDir, aaRaw, baseParams.alpha_k_raw, baseParams.beta_raw, baseParams.k_epsilon_frac_raw, scaleRaw);
writeParamFiles(bailoutDir, aaRaw, baseParams.alpha_k_raw, baseParams.beta_raw, baseParams.k_epsilon_frac_raw, scaleRaw);

runShell(sprintf('./%s', exeNB), noDir, 'No-bailout storage run failed.');

nbFolder = fullfile(bailoutDir, 'nb_files');
ensureCleanDir(nbFolder);
copyIfExists(noDir, nbFolder, 'graphs_default_dss.txt', true);
copyIfExists(noDir, nbFolder, 'graphs_b_next.txt', true);
copyIfExists(noDir, nbFolder, 'graphs_k_next.txt', true);
copyIfExists(noDir, nbFolder, 'graphs_labor.txt', true);
copyIfExists(noDir, nbFolder, 'graphs_consumption.txt', true);
copyIfExists(noDir, nbFolder, 'graphs_cons_bank.txt', true);

runShell(sprintf('./%s', exeBL), bailoutDir, 'Bailout storage run failed.');

stats = readStorageResults(fullfile(bailoutDir, 'results_storage.out'));
stats.AA_raw = aaRaw;
stats.scale_mk_raw = scaleRaw;

if keepFinal
    finalNoDir = fullfile(caseRoot, 'final_no_bailout');
    finalBailoutDir = fullfile(caseRoot, 'final_bailout');
    if exist(finalNoDir, 'dir')
        rmdir(finalNoDir, 's');
    end
    if exist(finalBailoutDir, 'dir')
        rmdir(finalBailoutDir, 's');
    end
    copyfile(noDir, finalNoDir);
    copyfile(bailoutDir, finalBailoutDir);
end
end

function writeParamFiles(runDir, aaRaw, alphaKRaw, betaRaw, kEpsFracRaw, scaleRaw)
writeNumberFile(fullfile(runDir, 'param_calib_AA.txt'), aaRaw);
writeNumberFile(fullfile(runDir, 'param_calib_alpha_k.txt'), alphaKRaw);
writeNumberFile(fullfile(runDir, 'param_calib_beta.txt'), betaRaw);
writeNumberFile(fullfile(runDir, 'param_calib_k_epsilon_frac.txt'), kEpsFracRaw);
writeNumberFile(fullfile(runDir, 'param_calib_scale_mk.txt'), scaleRaw);
end

function writeNumberFile(filename, value)
fid = fopen(filename, 'w');
assert(fid ~= -1, 'Could not open %s for writing.', filename);
cleanup = onCleanup(@() fclose(fid)); %#ok<NASGU>
fprintf(fid, '%.15g\n', value);
end

function writeCaseSummary(caseRoot, gammaVal, scaleRaw, aaBestRaw, evalAA, evalObj, evalStats, finalStats)
summaryFile = fullfile(caseRoot, 'case_summary.txt');
fid = fopen(summaryFile, 'w');
assert(fid ~= -1, 'Could not open %s for writing.', summaryFile);
cleanup = onCleanup(@() fclose(fid)); %#ok<NASGU>

fprintf(fid, 'Gamma_k: %.2f\n', gammaVal);
fprintf(fid, 'scale_mk raw: %d\n', scaleRaw);
fprintf(fid, 'Pre-calibrated AA raw: %d\n', aaBestRaw);
fprintf(fid, 'Pre-calibrated AA: %.6f\n', aaBestRaw / 1000);
fprintf(fid, 'Final bailout/GDP: %.6f\n', finalStats.bailout_gdp);
fprintf(fid, 'Final mean debt (percent): %.6f\n', finalStats.mean_debt_pct);
fprintf(fid, 'Final welfare gain at mean debt (percent): %.6f\n\n', finalStats.welfare_gain);
fprintf(fid, 'AA_raw, objective, bailout_gdp, mean_debt_pct, welfare_gain_pct, success\n');
for i = 1:numel(evalAA)
    bailoutGDP = NaN;
    meanDebt = NaN;
    welfareGain = NaN;
    success = false;
    if i <= numel(evalStats)
        if isfield(evalStats(i), 'bailout_gdp'), bailoutGDP = evalStats(i).bailout_gdp; end
        if isfield(evalStats(i), 'mean_debt_pct'), meanDebt = evalStats(i).mean_debt_pct; end
        if isfield(evalStats(i), 'welfare_gain'), welfareGain = evalStats(i).welfare_gain; end
        if isfield(evalStats(i), 'success'), success = evalStats(i).success; end
    end
    fprintf(fid, '%d, %.10f, %.10f, %.10f, %.10f, %d\n', ...
        evalAA(i), evalObj(i), bailoutGDP, meanDebt, welfareGain, success);
end
end

function s = readStorageResults(filename)
raw = strtrim(fileread(filename));
nums = sscanf(raw, '%f').';
assert(numel(nums) >= 27, 'Unexpected results_storage.out format in %s. Found only %d numbers.', filename, numel(nums));

s = struct();
s.AA                = nums(1);
s.alpha_k           = nums(2);
s.k_epsilon_frac    = nums(3);
s.scale_mk          = nums(4);
s.beta              = nums(5);
s.loan_gdp          = nums(6);
s.bailout_gdp       = nums(7);
s.default_freq      = nums(8);
s.default_cond_bc   = nums(9);
s.gov_gdp           = nums(10);
s.std_log_y         = nums(11);
s.debt_gdp          = nums(12);
s.k_gdp             = nums(13);
s.welfare_gain      = nums(14);
s.spread_mean       = nums(15);
s.spread_sd         = nums(16);
s.corr_spread_y     = nums(17);
s.mean_lending_rate = nums(18);
s.deviation         = nums(19);
s.dev_q             = nums(20);
s.iteration         = nums(21);
s.exposure_1        = nums(22);
s.exposure_2        = nums(23);
s.sum_of_A          = nums(24);
s.frac_endog_A      = nums(25);
s.avg_y             = nums(26);
s.k_assets          = nums(27);
s.mean_debt_pct     = nums(12);
end

function runShell(cmd, workdir, errmsg)
oldDir = pwd;
cleanup = onCleanup(@() cd(oldDir)); %#ok<NASGU>
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
