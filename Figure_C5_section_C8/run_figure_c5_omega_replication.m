function run_figure_c5_omega_replication(baseDir)
%RUN_FIGURE_C5_OMEGA_REPLICATION Build the omega comparison data and plot.
%   Uses the C.8-style parameterized Fortran pair to run three omega cases:
%   2.3, 2.5, 2.7. Each case is run as no-bailout first, then bailout,
%   and the final figure is produced from the generated bailout outputs.

clc;
fprintf('\n=== Figure C.5 omega replication runner ===\n');
setenv('PATH', ['/opt/homebrew/bin:' getenv('PATH')]);

if nargin < 1 || isempty(baseDir)
    fullPath = mfilename('fullpath');
    if isempty(fullPath)
        baseDir = pwd;
    else
        baseDir = fileparts(fullPath);
    end
end

fortranRoot = fullfile(baseDir, '..', 'Tables-section_C8');
fortranSrcNB = fullfile(fortranRoot, 'bailouts_optimal_nb.f90');
fortranSrcBL = fullfile(fortranRoot, 'bailouts_optimal_bl.f90');
assert(isfile(fortranSrcNB), 'Cannot find %s', fortranSrcNB);
assert(isfile(fortranSrcBL), 'Cannot find %s', fortranSrcBL);

exeNameNB = 'bailouts_optimal_omega_nb.out';
exeNameBL = 'bailouts_optimal_omega_bl.out';
optFlags = '-O3';
compileNow = true;

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

cases = { ...
    makeOmegaCase('01_omega_2_3', 2.3), ...
    makeOmegaCase('02_omega_2_5', 2.5), ...
    makeOmegaCase('03_omega_2_7', 2.7)};

nbFiles = { ...
    'graphs_default_dss.txt', ...
    'graphs_b_next.txt', ...
    'graphs_labor.txt', ...
    'graphs_consumption.txt', ...
    'graphs_cons_bank.txt'};

generatedRoot = fullfile(baseDir, 'figure_c5_omega_runs');
ensureCleanDir(generatedRoot);



if compileNow
    cmd = sprintf('gfortran %s "%s" -o "%s"', optFlags, fortranSrcNB, fullfile(baseDir, exeNameNB));
    runShell(cmd, baseDir, 'Omega no-bailout Fortran compilation failed.');

    cmd = sprintf('gfortran %s "%s" -o "%s"', optFlags, fortranSrcBL, fullfile(baseDir, exeNameBL));
    runShell(cmd, baseDir, 'Omega bailout Fortran compilation failed.');
end

assert(isfile(fullfile(baseDir, exeNameNB)), 'Omega no-bailout executable not found: %s', fullfile(baseDir, exeNameNB));
assert(isfile(fullfile(baseDir, exeNameBL)), 'Omega bailout executable not found: %s', fullfile(baseDir, exeNameBL));

for i = 1:numel(cases)
    c = cases{i};
    caseRoot = fullfile(generatedRoot, c.slug);
    nbRunDir = fullfile(caseRoot, 'no_bailout');
    blRunDir = fullfile(caseRoot, 'bailout');
    ensureCleanDir(caseRoot);
    mkdir(nbRunDir);
    mkdir(blRunDir);
    mkdir(fullfile(blRunDir, 'nb_files'));

    fprintf('\n==============================\n');
    fprintf('Processing omega = %.1f\n', c.omega);
    fprintf('==============================\n');

    copyfile(fullfile(baseDir, exeNameNB), fullfile(nbRunDir, exeNameNB));
    copyfile(fullfile(baseDir, exeNameBL), fullfile(blRunDir, exeNameBL));

    p_nb = baseline;
    p_nb.omega = c.omega;
    p_nb.runwelf = 0;
    p_nb.T_max_frac = 0.0;
    p_nb.transfer_num_use = 1;
    p_nb.simulonly = 0;
    p_nb.indicator_external = 0.0;
    p_nb.writeout = 1;

    writeParamsNml(fullfile(nbRunDir, 'params_current.nml'), p_nb);
    fprintf('Running no-bailout code...\n');
    runShell(sprintf('./%s 1', exeNameNB), nbRunDir, sprintf('Omega %.1f no-bailout run failed.', c.omega));

    for k = 1:numel(nbFiles)
        src = fullfile(nbRunDir, nbFiles{k});
        dst = fullfile(blRunDir, 'nb_files', nbFiles{k});
        assert(isfile(src), 'Missing omega no-bailout benchmark file: %s', src);
        copyfile(src, dst);
    end

    p_bl = baseline;
    p_bl.omega = c.omega;
    p_bl.runwelf = 1;
    p_bl.T_max_frac = 1.0;
    p_bl.transfer_num_use = 50;
    p_bl.simulonly = 0;
    p_bl.indicator_external = 0.0;
    p_bl.writeout = 1;

    writeParamsNml(fullfile(blRunDir, 'params_current.nml'), p_bl);
    fprintf('Running bailout code...\n');
    runShell(sprintf('./%s 1', exeNameBL), blRunDir, sprintf('Omega %.1f bailout run failed.', c.omega));
end

figure_c5_welfare_omega(baseDir, generatedRoot);
end

function c = makeOmegaCase(slug, omega)
c = struct('slug', slug, 'omega', omega);
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
