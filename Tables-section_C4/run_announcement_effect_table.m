function run_announcement_effect_table()
% RUN_ANNOUNCEMENT_EFFECT_TABLE
% Replicates Table C.4 for eta = 0, 0.5, 0.9.
%
% Required files in the same folder:
%   bailouts_extensions_eta_nobailout.f90
%   bailouts_extensions_eta_bailout.f90

clc;
fprintf('\n=== Announcement-effect table runner ===\n');
setenv('PATH', ['/opt/homebrew/bin:' getenv('PATH')]);

rootDir = pwd;
noBailoutTemplate = fullfile(rootDir, 'bailouts_extensions_eta_nobailout.f90');
bailoutTemplate   = fullfile(rootDir, 'bailouts_extensions_eta_bailout.f90');

assert(isfile(noBailoutTemplate), 'Cannot find %s', noBailoutTemplate);
assert(isfile(bailoutTemplate),   'Cannot find %s', bailoutTemplate);

etaGrid = [0.0, 0.5, 0.9];
runDir = fullfile(rootDir, 'announcement_effect_replication_run');
ensureCleanDir(runDir);

results = struct([]);

for k = 1:numel(etaGrid)
    etaVal = etaGrid(k);
    etaTag = strrep(sprintf('%.1f', etaVal), '.', 'p');
    caseDir = fullfile(runDir, ['eta_' etaTag]);
    ensureCleanDir(caseDir);

    fprintf('\n==============================\n');
    fprintf('Running eta = %.1f\n', etaVal);
    fprintf('==============================\n');

    nbSrc = fullfile(caseDir, 'bailouts_extensions_eta_nobailout_this_eta.f90');
    blSrc = fullfile(caseDir, 'bailouts_extensions_eta_bailout_this_eta.f90');

    writeEtaSource(noBailoutTemplate, nbSrc, etaVal, "nobailout");
    writeEtaSource(bailoutTemplate,   blSrc, etaVal, "bailout");

    exeNB = 'bailouts_extensions_eta_nobailout.out';
    exeBL = 'bailouts_extensions_eta_bailout.out';

    fprintf('\nCompiling no-bailout code for eta = %.1f...\n', etaVal);
    runShell(sprintf('gfortran -O3 "%s" -o "%s"', nbSrc, exeNB), caseDir, ...
        'No-bailout compilation failed.');

    fprintf('\nStep 1/2: running no-bailout code for eta = %.1f...\n', etaVal);
    runShell(sprintf('./%s', exeNB), caseDir, 'No-bailout run failed.');
    nbOut = snapshotRunOutputs(caseDir, ['no_bailout_eta_' etaTag]); 

    nbFolder = fullfile(caseDir, 'nb_files');
    ensureCleanDir(nbFolder);
    copyIfExists(caseDir, nbFolder, 'graphs_default_dss.txt', true);
    copyIfExists(caseDir, nbFolder, 'graphs_b_next.txt', true);
    copyIfExists(caseDir, nbFolder, 'graphs_labor.txt', true);
    copyIfExists(caseDir, nbFolder, 'graphs_consumption.txt', true);
    copyIfExists(caseDir, nbFolder, 'graphs_cons_bank.txt', true);

    fprintf('\nCompiling bailout code for eta = %.1f...\n', etaVal);
    runShell(sprintf('gfortran -O3 "%s" -o "%s"', blSrc, exeBL), caseDir, ...
        'Bailout compilation failed.');

    fprintf('\nStep 2/2: running bailout code for eta = %.1f...\n', etaVal);
    runShell(sprintf('./%s', exeBL), caseDir, 'Bailout run failed.');
    blOut = snapshotRunOutputs(caseDir, ['bailout_eta_' etaTag]);
    bl = readResultsOut(blOut.path);

    results(k).eta = etaVal; 
    results(k).promised_bailout_gdp = bl.promised_bailout_gdp;
    results(k).banking_crisis_prob = bl.banking_crisis_prob;
    results(k).actual_bailout_gdp_cond_bc = bl.actual_bailout_gdp_cond_bc;
    results(k).welfare_gain_bailouts = bl.avg_welf;
end

T = table( ...
    string({ ...
        'Bailout/GDP (promised)'; ...
        'Banking crisis prob.'; ...
        'Bailout/GDP (conditional on BC)'; ...
        'Welfare gain of bailouts' ...
    }), ...
    [results(1).promised_bailout_gdp; ...
     results(1).banking_crisis_prob; ...
     results(1).actual_bailout_gdp_cond_bc; ...
     results(1).welfare_gain_bailouts], ...
    [results(2).promised_bailout_gdp; ...
     results(2).banking_crisis_prob; ...
     results(2).actual_bailout_gdp_cond_bc; ...
     results(2).welfare_gain_bailouts], ...
    [results(3).promised_bailout_gdp; ...
     results(3).banking_crisis_prob; ...
     results(3).actual_bailout_gdp_cond_bc; ...
     results(3).welfare_gain_bailouts], ...
    'VariableNames', {'Statistic','eta_0_0','eta_0_5','eta_0_9'});

csvFile  = fullfile(runDir, 'announcement_effect_results_table.csv');
xlsxFile = fullfile(runDir, 'announcement_effect_results_table.xlsx');
writetable(T, csvFile);
writetable(T, xlsxFile);

fprintf('\nDone. Final Table C.4-style output written to:\n  %s\n  %s\n\n', csvFile, xlsxFile);
disp(T);
end

function writeEtaSource(templateFile, outFile, etaVal, modelType)
txt = fileread(templateFile);

etaStr = sprintf('%.1fd+0', etaVal);

% IMPORTANT: use word boundaries so eta does NOT match theta or beta
txt = regexprep(txt, '\<eta\>\s*=\s*[0-9.]+d[+-][0-9]+', ['eta=' etaStr]);

% Force single-run mode, not calibration-grid mode
txt = regexprep(txt, '\<N_bita\>\s*=\s*\d+',   'N_bita = 1');
txt = regexprep(txt, '\<N_abar\>\s*=\s*\d+',   'N_abar = 1');
txt = regexprep(txt, '\<N_sigmae\>\s*=\s*\d+', 'N_sigmae = 1');
txt = regexprep(txt, '\<N_gov\>\s*=\s*\d+',    'N_gov = 1');
txt = regexprep(txt, '\<N_tmax\>\s*=\s*\d+',   'N_tmax = 1');

% Fix graphs_parameters: pi_global is not initialized yet there
txt = strrep(txt, 'gov_spending, AA, pi_global, sig_e', ...
                  'gov_spending, AA, pi, sig_e');

if modelType == "nobailout"
    txt = regexprep(txt, '\<runwelf\>\s*=\s*\d+', 'runwelf = 0');
    txt = regexprep(txt, '\<transfer_num\>\s*=\s*\d+', 'transfer_num = 1');
    txt = regexprep(txt, '\<T_max_frac\>\s*=\s*[0-9.]+d[+-][0-9]+', 'T_max_frac = 0.0d+0');
    txt = regexprep(txt, '\<indicator_bailout_default\>\s*=\s*[0-9.]+d[+-][0-9]+', ...
                         'indicator_bailout_default = 0d+0');
else
    txt = regexprep(txt, '\<runwelf\>\s*=\s*\d+', 'runwelf = 1');
    txt = regexprep(txt, '\<transfer_num\>\s*=\s*\d+', 'transfer_num = 50');
    txt = regexprep(txt, '\<T_max_frac\>\s*=\s*[0-9.]+d[+-][0-9]+', 'T_max_frac = 1.0d+0');
    txt = regexprep(txt, '\<indicator_bailout_default\>\s*=\s*[0-9.]+d[+-][0-9]+', ...
                         'indicator_bailout_default = 0d+0');
end

fid = fopen(outFile, 'w');
assert(fid > 0, 'Could not open %s for writing.', outFile);
fprintf(fid, '%s', txt);
fclose(fid);
end

function s = readResultsOut(filename)
raw = strtrim(fileread(filename));
nums = sscanf(raw, '%f').';

assert(numel(nums) >= 45, ...
    ['Unexpected results.out format in %s. Found only %d numbers. ' ...
     'The patched Fortran templates should write the four table moments as the last four entries.'], ...
    filename, numel(nums));

s = struct();
s.promised_bailout_gdp       = nums(end-3);  % already percent
s.banking_crisis_prob        = nums(end-2);  % already percent
s.actual_bailout_gdp_cond_bc = nums(end-1);  % already percent
s.avg_welf                   = nums(end);    % already percent
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

function info = snapshotRunOutputs(runDir, tag)
src = fullfile(runDir, 'results.out');
assert(isfile(src), 'No results.out found in %s', runDir);

dst = fullfile(runDir, sprintf('%s_results.out', tag));
copyfile(src, dst);

info.path = dst;
end
